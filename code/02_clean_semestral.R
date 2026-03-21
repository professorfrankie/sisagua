# Clean SISAGUA semestral data files -----
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


# Data on half-yearly basis  --------------------------------------------------
load_data <- function(year)
    {
      df <- read_csv2(paste0("/data/brazil_health/SISAGUA/unzipped/controle_semestral_", year, ".csv"), 
                      locale = readr::locale(encoding = "latin1"), 
                      skip = 1,
                      col_types = c("____n___n_n_c__nn___c_ccc_____cccnnc"), 
                      col_names = c("muni_code", "cnpj_institution", "cnpj_office", "id_capture_point", 
                                    "year", "semester", "date_collected", "type_capture_point", 
                                    "parameter_group", "parameter", "sample_number", 
                                    "unit", "limit_permitted", "limit_det", "limit_quant", "value")) |> 
        mutate(limit_permitted = as.numeric(limit_permitted), 
               value = gsub(",", ".", value)) # fiddle around with coding of value
    }

sisagua_semestral_2014 <- load_data(2014)
sisagua_semestral_2015 <- load_data(2015)
sisagua_semestral_2016 <- load_data(2016)
sisagua_semestral_2017 <- load_data(2017)
sisagua_semestral_2018 <- load_data(2018)
sisagua_semestral_2019 <- load_data(2019)
sisagua_semestral_2020 <- load_data(2020)
sisagua_semestral_2021 <- load_data(2021)
sisagua_semestral_2022 <- load_data(2022) 
sisagua_semestral_2023 <- load_data(2023)
sisagua_semestral_2024 <- load_data(2024)

# Set values as numeric ------------------------------------------------------
values_numeric <- function(df)
{
  df_numeric <- df |>
    mutate(
      value = ifelse(value == "MENOR_LQ", 0, value),
      value = ifelse(value == "MENOR_LD", 0, value),
      value = as.numeric(value),
      limit_det = ifelse(is.na(limit_det), limit_quant, limit_det),
      limit_quant = ifelse(is.na(limit_quant), limit_det, limit_quant))
  return(df_numeric)
}
sisagua_semestral_2014_num <- values_numeric(sisagua_semestral_2014)
sisagua_semestral_2015_num <- values_numeric(sisagua_semestral_2015)
sisagua_semestral_2016_num <- values_numeric(sisagua_semestral_2016)
sisagua_semestral_2017_num <- values_numeric(sisagua_semestral_2017)
sisagua_semestral_2018_num <- values_numeric(sisagua_semestral_2018)
sisagua_semestral_2019_num <- values_numeric(sisagua_semestral_2019)
sisagua_semestral_2020_num <- values_numeric(sisagua_semestral_2020)
sisagua_semestral_2021_num <- values_numeric(sisagua_semestral_2021)
sisagua_semestral_2022_num <- values_numeric(sisagua_semestral_2022)
sisagua_semestral_2023_num <- values_numeric(sisagua_semestral_2023)
sisagua_semestral_2024_num <- values_numeric(sisagua_semestral_2024)

# Dictionary -----------------------------------------------------------------
unique(sisagua_semestral_2017$type_capture_point)
unique(sisagua_semestral_2017$parameter_group)
unique(sisagua_semestral_2017$parameter)

