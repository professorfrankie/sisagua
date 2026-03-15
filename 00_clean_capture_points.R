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

#does each id_capture_point always have the same muni_code
all_same <- capture_points %>%
  group_by(id_capture_point) %>%
  summarise(n_muni = n_distinct(muni_code, na.rm = TRUE)) %>%
  summarise(all_same = all(n_muni <= 1)) %>%
  pull(all_same)

all_same #FALSE

cp_multiple_munis <- capture_points %>%
  group_by(id_capture_point) %>%
  filter(n_distinct(muni_code, na.rm = TRUE) > 1) %>%
  arrange(id_capture_point, muni_code)
length(unique(cp_multiple_munis$id_capture_point))/length(unique(capture_points$id_capture_point))
#0.0002

capture_points <- capture_points %>%
  group_by(across(-flow)) %>%        # Group by all columns except 'flow'
  summarise(flow = mean(flow), .groups = 'drop')

capture_points <- unique(capture_points)

#some points are outside of brazil
summary(capture_points$lat) 
summary(capture_points$lon)

# set as NA lat lon that are outside of its municipality
capture_points_sf <- st_as_sf(capture_points, coords = c("lon", "lat"), crs = 4674, na.fail = FALSE, remove = FALSE)
munis <- read_municipality(year = 2020) |>
  select(code_muni, geom)
st_crs(munis) == st_crs(capture_points_sf)

cps_join <- capture_points |>
  left_join(munis, by = c("muni_code" = "code_muni"))

cps_checked <- cps_join |>
  mutate(
    inside = lengths(st_intersects(st_as_sf(capture_points, coords = c("lon", "lat"), crs = 4674, na.fail = FALSE, remove = FALSE), geom)) > 0,
    lat = ifelse(inside, lat, NA),
    lon = ifelse(inside, lon, NA)
  ) |>
  filter(is.na(lat))

capture_points <- capture_points |>
  mutate(lat = replace(lat, id_capture_point %in% cps_checked$id_capture_point, NA),
         lon = replace(lon, id_capture_point %in% cps_checked$id_capture_point, NA))

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

capture_points <- capture_points %>%
  group_by(id_capture_point) %>%
  mutate(
    lat = if (n_distinct(na.omit(lat)) == 1) first(na.omit(lat)) else lat,
    lon = if (n_distinct(na.omit(lon)) == 1) first(na.omit(lon)) else lon
  ) %>%
  ungroup()


# get those capture points that have different lat/lon at some point
cps_dupl <- capture_points |> filter(!is.na(lat), substr(id_capture_point, 1, 1) %in% c("S", "C")) |>
  distinct(lat, lon, .keep_all = TRUE) |> group_by(id_capture_point, year) |> count() |> filter(n > 1)

# compute the range of lat and lon for those points that have multiple lat/lon
cps_dist <- capture_points |> filter(!is.na(lat), id_capture_point %in% c(cps_dupl$id_capture_point)) |>
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
  rbind(capture_points_avg)


summary_munis <- capture_points %>%
  group_by(muni_code) %>%
  summarise(
    n_capture_points = n_distinct(id_capture_point),
    n_years = n_distinct(year),
    .groups = "drop"
  )

ggplot(summary_munis, aes(x = n_capture_points, y = n_years)) +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0.2) +
  labs(
    title = "Capture Points vs Years Covered by Municipality",
    x = "Number of Capture Points",
    y = "Number of Years Covered"
  ) +
  theme_minimal()

p <- ggplot(summary_munis, aes(x = n_capture_points)) +
  geom_histogram(
    binwidth = 20,
    boundary = 0,
    fill = "steelblue",
    color = "white"
  ) +
  stat_bin(
    binwidth = 20,
    boundary = 0,
    geom = "text",
    aes(label = after_stat(count)),
    vjust = -0.3,
    size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Distribution of Capture Points per Municipality",
    x = "Number of Capture Points",
    y = "Number of Municipalities"
  ) +
  theme_minimal()
p
bin_data <- ggplot_build(p)$data[[1]][, c("xmin", "xmax", "count")]
bin_data

ggplot(summary_munis, aes(x = n_years)) +
  geom_histogram(bins = 11, fill = "gold", color = "white") +
  stat_bin(
    bins = 11,
    geom = "text",
    aes(label = after_stat(count)),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Distribution of Years per Municipality",
    x = "Number of years",
    y = "Number of Municipalities"
  ) +
  theme_minimal()
# check capture points with more than one lat lon combination
multiple_coords <- capture_points %>%
  group_by(id_capture_point) %>%
  mutate(n_combinations = n_distinct(lat, lon)) %>%
  ungroup() %>%
  filter(n_combinations > 1) 

#share of munis and id capture points with multiple coordinates
length(unique(multiple_coords$muni_code))/length(unique(capture_points$muni_code))      
length(unique(multiple_coords$id_capture_point))/length(unique(capture_points$id_capture_point))




#set lat and long outside of brazil as NA
capture_points_sf <- st_as_sf(capture_points, coords = c("lon", "lat"), crs = 4674, na.fail = FALSE, remove = FALSE)
country_brazil <- read_country(year = 2020)
st_crs(country_brazil) == st_crs(capture_points_sf)

capture_points_sf <- capture_points_sf %>%
  mutate(
    in_brazil = as.integer(st_within(capture_points_sf, country_brazil, sparse = FALSE)[,1])
  )
sum(capture_points_sf$in_brazil == 1, na.rm = TRUE)
sum(capture_points_sf$in_brazil == 0)   
#####







library(dplyr)

# Assuming your data has a 'year' column
capture_summary <- capture_points %>%
  # Count non-NA lat/lon per group
  group_by(muni_code, id_capture_points) %>%
  summarise(
    years_with_coords = sum(!is.na(lat) & !is.na(lon)),
    .groups = 'drop'
  )

# View result
capture_summary   


capture_points %>%
  group_by(id_capture_point) %>%
  summarise(
    n_lat = n_distinct(lat),
    n_lon = n_distinct(lon)
  ) %>%
  summary()

cp_with_multiple_locations <- capture_points %>%
  group_by(id_capture_point) %>%
  summarise(
    n_pos = n_distinct(paste(lat, lon))
  ) %>%
  count(n_pos) %>%
  arrange(desc(n_pos))
#337106 out of 352292 unique id_capture_points also have unique lat long


library(dplyr)

capture_points <- capture_points %>%
  mutate(
    missing_coord = is.na(lat) | is.na(lon)
  )

muni_year_missing <- capture_points %>%
  group_by(muni_code, year) %>%
  summarise(
    any_missing = any(missing_coord),
    all_missing = all(missing_coord),
    n = n(), #how many capture point exist for this municipality in this year?”
    .groups = "drop"
  )

muni_always_missing <- capture_points %>%
  group_by(muni_code) %>%
  summarise(all_missing = all(is.na(lat) | is.na(lon))) %>%
  filter(all_missing) %>%
  pull(muni_code)   


muni_consistent_missing <- muni_year_missing %>%
  group_by(muni_code) %>%
  summarise(
    years_observed = n(),
    always_missing = all(all_missing),
    ever_missing = any(any_missing),
    .groups = "drop"
  )

id_year_missing <- capture_points %>%
  group_by(id_capture_point, year) %>%
  summarise(
    any_missing = any(missing_coord),
    all_missing = all(missing_coord),
    .groups = "drop"
  )

id_consistent_missing <- id_year_missing %>%
  group_by(id_capture_point) %>%
  summarise(
    years_observed = n(),
    always_missing = all(all_missing),
    ever_missing = any(any_missing),
    .groups = "drop"
  )

#do munis have always the same id_capture point?
