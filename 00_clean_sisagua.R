# Land tenure -----
library("dplyr")
library("readr")
library("tidyr")
library("sf")

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


# Half-yearly samples (include much more parameters) ----------------------

sisagua_semestral_2017 <- read_csv2("/data/brazil_health/SISAGUA/unzipped/controle_semestral_2017.csv", 
                                    locale = readr::locale(encoding = "latin1"), 
                                    skip = 1,
                                    col_types = c("____n___n_n_c__nn___c_ccc_____cccnnc"), 
                                    col_names = c("muni_code", "cnpj_institution", "cnpj_office", "id_capture_point", 
                                                  "year", "semester", "date_collected", "type_capture_point", 
                                                  "parameter_group", "parameter", "sample_number", 
                                                  "unit", "limit_permitted", "limit_det", "limit_quant", "value")) |> 
  mutate(limit_permitted = as.numeric(limit_permitted), 
         value = gsub(",", ".", value)) # fiddle around with coding of value
sisagua_semestral_2017

# parameters_available <- sisagua_semestral_2017 |> distinct(parameter_group, parameter)
# write_csv(parameters_available, "/data/brazil_health/SISAGUA/semestral_available_parameters.csv")



# Monthly samples (basic parameters) --------------------------------------

sisagua_monthly_2017 <- read_csv2("/data/brazil_health/SISAGUA/unzipped/controle_mensal_parametros_basicos_2017.csv",
                                  locale = readr::locale(encoding = "latin1"), 
                                  skip = 1,
                                  col_types = c("___n____n_n_c__cnncccn"), 
                                  col_names = c("muni_code", "cnpj_institution", "cnpj_office", "id_capture_point", 
                                                "type_filter", "year", "month", "type_capture_point", 
                                                "parameter", "unit", "value"))
sisagua_monthly_2017


# Monthly samples (additional parameters) ---------------------------------

sisagua_monthly_addparas <- read_csv2("/data/brazil_health/SISAGUA/unzipped/controle_mensal_demais_parametros.csv",
                                  locale = readr::locale(encoding = "latin1"), 
                                  skip = 1,
                                  col_types = c("____n___n_n_c__nn__c_____ccc"), 
                                  col_names = c("muni_code", "cnpj_institution", "cnpj_office", "id_capture_point", 
                                                "year", "month", "date_collected", 
                                                "parameter", "unit", "value")) |> 
  mutate(date_collected = substr(date_collected, 1, 10))

sisagua_monthly_addparas


# Monthly samples (non-standard samples - maybe on-demand?) ---------------

sisagua_monthly_nonstd <- read_csv2("/data/brazil_health/SISAGUA/unzipped/controle_mensal_amostras_fora_padrao.csv",
                                  locale = readr::locale(encoding = "latin1"), 
                                  skip = 1,
                                  col_types = c("____n___n_n_c_nn__ccccccc__nnc_"), 
                                  col_names = c("muni_code", "cnpj_institution", "cnpj_office", "id_capture_point", 
                                                "year", "month", "date_collected", "type_capture_point", 
                                                "parameter",
                                                "zone", "area_category", "area", "type_place", "lat", "lon",
                                                "value")) |> 
  mutate(date_collected = substr(date_collected, 1, 10))

sisagua_monthly_nonstd



# Vigiliancia (basic parameters) ------------------------------------------

sisagua_vigilancia_2017 <- read_csv2("/data/brazil_health/SISAGUA/unzipped/vigilancia_parametros_basicos_2017.csv",
                                     locale = readr::locale(encoding = "latin1"), 
                                     skip = 1,
                                     col_types = c("____n_c_c__nnc____c_cccc_nnc____c_"), 
                                     col_names = c("muni_code", "motive_collection", "id_capture_point", 
                                                   "year", "month", "date_collected", "type_capture_point",
                                                   "zone", "area_category", "area", "type_place", "lat", "lon",
                                                   "parameter", "value"))

sisagua_vigilancia_2017


