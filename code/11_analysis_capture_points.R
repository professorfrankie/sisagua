# Analyze SISAGUA capture points -----
library("dplyr")
library("readr")
library("tidyr")
library("sf")
library("geobr")
library("ggplot2")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
            "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
            "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
            "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
            "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
            "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
            "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")


# Load cleaned files --------------------------------------------------------

cps_with_years <- read_rds("/data/brazil_health/SISAGUA/capture_points_withyears.RDS")



# Plots -------------------------------------------------------------------

summary_munis <- cps_with_years %>%
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

ggsave("./output/plots/cp_years.png", 
       width = 10, height = 6)

ggplot(summary_munis, aes(x = n_capture_points)) +
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

ggsave("./output/plots/cp_muni.png", 
       width = 10, height = 6)

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

ggsave("./output/plots/cp_years_muni.png", 
       width = 10, height = 6)



# Additional checks -------------------------------------------------------


# check capture points with more than one lat lon combination
multiple_coords <- cps_with_years %>%
  group_by(id_capture_point) %>%
  mutate(n_combinations = n_distinct(lat, lon)) %>%
  ungroup() %>%
  filter(n_combinations > 1) 

#share of munis and id capture points with multiple coordinates
length(unique(multiple_coords$muni_code))/length(unique(cps_with_years$muni_code))      
length(unique(multiple_coords$id_capture_point))/length(unique(cps_with_years$id_capture_point))


# 344905 out of 352292 unique id_capture_points also have unique lat long
cp_with_multiple_locations <- cps_with_years %>%
  group_by(id_capture_point) %>%
  summarise(
    n_pos = n_distinct(paste(lat, lon))
  ) %>%
  count(n_pos) %>%
  arrange(desc(n_pos))

# are coordinates missing ALWAYS or at ANY time for municipality groups
muni_year_missing <- cps_with_years %>%
  mutate(
    missing_coord = is.na(lat) | is.na(lon)
  ) %>%
  group_by(muni_code, year) %>%
  summarise(
    any_missing = any(missing_coord),
    all_missing = all(missing_coord),
    n = n(), #how many capture point exist for this municipality in this year?”
    .groups = "drop"
  ) %>%
  group_by(muni_code) %>%
  summarise(
    years_observed = n(),
    always_missing = all(all_missing),
    ever_missing = any(any_missing),
    .groups = "drop"
  )

# are coordinates missing ALWAYS or at ANY time for id groups
id_year_missing <- cps_with_years %>%
  mutate(
    missing_coord = is.na(lat) | is.na(lon)
  ) %>%
  group_by(id_capture_point, year) %>%
  summarise(
    any_missing = any(missing_coord),
    all_missing = all(missing_coord),
    .groups = "drop"
  ) %>%
  group_by(id_capture_point) %>%
  summarise(
    years_observed = n(),
    always_missing = all(all_missing),
    ever_missing = any(any_missing),
    .groups = "drop"
  )


capture_points <- readRDS("/data/brazil_health/SISAGUA/capture_points_cleaned.RDS")

# what is the share of capture points where coordinates fall within Brazil
sum(capture_points$bra_cp_match == 1, na.rm = T) / sum(!is.na(capture_points$bra_cp_match))
# ~94%

# what is the share of capture points where coordinates fall within recorded municipality
sum(capture_points$muni_cp_match == 1, na.rm = T) / sum(!is.na(capture_points$muni_cp_match))
# ~93%

