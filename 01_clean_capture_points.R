# Land tenure -----
library("dplyr")
library("readr")
library("tidyr")
library("sf")
library("geobr")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
            "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
            "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
            "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
            "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
            "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
            "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")



# Info on capture points --------------------------------------------------

capture_points <- read_csv2("/data/brazil_health/SISAGUA/unzipped/cadastro_pontos_captacao.csv", 
                            locale = readr::locale(encoding = "latin1"), 
                            skip = 1,
                            col_types = c("____n____n_c__n_____cnnn"), 
                            col_names = c("muni_code", "cnpj_office", "id_capture_point", 
                                          "year", "permit_drink", "lat", "lon", "flow"))
capture_points

#set invalid coordinates as NA
capture_points <- capture_points |>
  mutate(
    lat = replace(lat, abs(lat) > 90, NA),
    lon = replace(lon, abs(lon) > 180, NA)
  )

# change integers to NA or closest value
nearest_non_integer <- function(x) {
  
  # detect integers like 11.00000
  is_int <- !is.na(x) & abs(x - round(x)) < 1e-6
  non_int <- x[!is.na(x) & !is_int]
  
  # case 1: only integers
  if (length(non_int) == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  # case 2: mix of integers and non-integers
  sapply(x, function(v) {
    if (is.na(v) || abs(v - round(v)) >= 1e-6) {
      v
    } else {
      non_int[which.min(abs(non_int - v))]
    }
  }) |> as.numeric()
}

capture_points <- capture_points %>%
  group_by(id_capture_point) %>%
  mutate(
    lat = nearest_non_integer(lat),
    lon = nearest_non_integer(lon)
  ) %>%
  ungroup()

# get those capture points that have different lat/lon at some point
cps_dupl <- capture_points |> filter(!is.na(lat) | !is.na(lon)) |>
  distinct(id_capture_point, year, lat, lon, .keep_all = TRUE) |> group_by(id_capture_point, year) |> count() |> filter(n > 1)

# compute the range of lat and lon for those points that have multiple lat/lon
cps_dist <- capture_points |> filter((!is.na(lat) | !is.na(lon)), id_capture_point %in% c(cps_dupl$id_capture_point)) |>
  group_by(id_capture_point) |>
  # mutate(n_lat = nchar(stringr::str_split(as.character(lat), [file://.]\\., simplify = T)[, 2]),
  #        n_lon = nchar(stringr::str_split(as.character(lat), [file://.]\\., simplify = T)[, 2]),
  #        n_both = n_lat + n_lon) |>
  # slice_max(n_both, n = 1) |>
  # filter(n_lat >= 4, n_lon >= 4) |> # take only precise information
  summarise(lat_range = max(lat) - min(lat),
            lon_range = max(lon) - min(lon))

# those where the range is above ~11km, we set the coordinates to NA to preserve muni info
cps_to_remove <- cps_dist |>
  filter(lat_range > 0.1 | lon_range > 0.1)

capture_points <- capture_points |>
  mutate(lat = replace(lat, id_capture_point %in% cps_to_remove$id_capture_point, NA),
         lon = replace(lon, id_capture_point %in% cps_to_remove$id_capture_point, NA))

# for those where ranges are below ~11km, we simply average lat/lon
cps_to_avg <- cps_dist |>
  filter(lat_range <= 0.1 & lon_range <= 0.1)

capture_points_avg <- capture_points |> filter(id_capture_point %in% cps_to_avg$id_capture_point, !is.na(lat)) |>
  group_by(muni_code, id_capture_point) |>
  summarise(lat = mean(lat), lon = mean(lon), flow = mean(flow),
            year = first(year), permit_drink = first(permit_drink), cnpj_office = first(cnpj_office)) |>
  transmute(muni_code, cnpj_office, id_capture_point, year, permit_drink, lat, lon, flow)

capture_points <- capture_points |> filter(!id_capture_point %in% cps_to_avg$id_capture_point) |>
  rbind(capture_points_avg) |>
  mutate(
    lat = ifelse(is.na(lon), NA, lat),
    lon = ifelse(is.na(lat), NA, lon)
  )

capture_points_dupl <- capture_points |> filter((!is.na(lat) | !is.na(lon))) |>
  distinct(lat, lon, .keep_all = TRUE) |> group_by(id_capture_point, year) |> count() |> filter(n > 1)


#final dataset
capture_points_summary <- capture_points |>
  group_by(muni_code, id_capture_point) |>
  arrange(year) |>
  summarise(muni_code = first(na.omit(muni_code)), cnpj_office = first(na.omit(cnpj_office)), 
            id_capture_point = first(na.omit(id_capture_point)), 
            permit_drink = last(na.omit(permit_drink)), lat = first(na.omit(lat)), 
            lon = first(na.omit(lon)), flow = mean(flow))

# create location dummies
munis <- read_municipality(year = 2020) |>
  select(code_muni, geom)
country_brazil <- read_country(year = 2020)
st_crs(country_brazil) == st_crs(munis)

cp_sf <- capture_points_summary |>
  filter(!is.na(lon) & !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4674, remove = FALSE)

# Use st_within to get matches
within_muni <- st_within(cp_sf, munis, sparse = TRUE)
within_bra <- st_within(cp_sf, country_brazil, sparse = TRUE)

# Check if each point intersects polygon
cp_sf$muni_cp_match <- sapply(within_muni, function(x) ifelse(length(x) > 0, 1, 0))
cp_sf$bra_cp_match <- sapply(within_bra, function(x) ifelse(length(x) > 0, 1, 0))

cp_nosf <- st_drop_geometry(cp_sf) |>
  select(muni_code, id_capture_point, cnpj_office, muni_cp_match, bra_cp_match) 

cp_final <- capture_points_summary |>
  left_join(cp_nosf, by = c("muni_code", "id_capture_point", "cnpj_office")) 

write_rds(cp_final, "/home/francesca/brazil_water/sisagua/capture_points_cleaned.RDS")
