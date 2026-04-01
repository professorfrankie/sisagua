# Merge SISAGUA data and capture points files -----
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


# Load data ---------------------------------------------------------------

capture_points <- readRDS("/data/brazil_health/SISAGUA/capture_points_cleaned.RDS")
sisagua_semestral <- readRDS("/data/brazil_health/SISAGUA/sisagua_semestral_cleaned.RDS")

sisagua_semestral_cps_matched <- sisagua_semestral |>
  left_join(capture_points, by = c("muni_code", "id_capture_point", "cnpj_office"))

write_rds(sisagua_semestral_cps_matched, "/data/brazil_health/SISAGUA/sisagua_semestral_cps_matched.RDS")
