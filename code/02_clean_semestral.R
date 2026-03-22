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
      value = ifelse(value %in% c("MENOR_LQ", "MENOR_LD"), 0, value),
      value = as.numeric(value),
      limit_det = ifelse(is.na(limit_det), limit_quant, limit_det),
      limit_quant = ifelse(is.na(limit_quant), limit_det, limit_quant),
      limit_exceeded = ifelse(pmax(limit_det, limit_quant) > limit_permitted, 1, 0)
    )
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

# Choose for duplicates maximum value ----------------------------------------

#sisagua_semestral_2017 |>
#  filter(id_capture_point == "C290570000003", parameter == "Bário") |>
#  select(id_capture_point, year, semester, parameter, value) |>
#  arrange(year, semester, value)

#sisagua_semestral_2017 |>
#  filter(id_capture_point == "C290570000003", parameter == "Bário") |>
#  group_by(muni_code, id_capture_point, year, semester, parameter) |>
#  slice_max(value, n = 1) |> slice_head(n = 1)

max_value <- function(df) 
{
  new_df <- df |>
    group_by(muni_code, id_capture_point, year, semester, parameter) |>
    slice_max(value, n = 1) |> slice_head(n = 1)
  return(new_df)
}

# Dictionary -----------------------------------------------------------------
unique_type_capture_points <- unique(c(sisagua_semestral_2014$type_capture_point,
                                       sisagua_semestral_2015$type_capture_point,
                                       sisagua_semestral_2016$type_capture_point,
                                       sisagua_semestral_2017$type_capture_point,
                                       sisagua_semestral_2018$type_capture_point,
                                       sisagua_semestral_2019$type_capture_point,
                                       sisagua_semestral_2020$type_capture_point,
                                       sisagua_semestral_2021$type_capture_point,
                                       sisagua_semestral_2022$type_capture_point,
                                       sisagua_semestral_2023$type_capture_point,
                                       sisagua_semestral_2024$type_capture_point))

unique_parameter_groups <- unique(c(sisagua_semestral_2014$parameter_group,
                                    sisagua_semestral_2015$parameter_group,
                                    sisagua_semestral_2016$parameter_group,
                                    sisagua_semestral_2017$parameter_group,
                                    sisagua_semestral_2018$parameter_group,
                                    sisagua_semestral_2019$parameter_group,
                                    sisagua_semestral_2020$parameter_group,
                                    sisagua_semestral_2021$parameter_group,
                                    sisagua_semestral_2022$parameter_group,
                                    sisagua_semestral_2023$parameter_group,
                                    sisagua_semestral_2024$parameter_group))

unique_parameters <- unique(c(sisagua_semestral_2014$parameter,
                              sisagua_semestral_2015$parameter,
                              sisagua_semestral_2016$parameter,
                              sisagua_semestral_2017$parameter,
                              sisagua_semestral_2018$parameter,
                              sisagua_semestral_2019$parameter,
                              sisagua_semestral_2020$parameter,
                              sisagua_semestral_2021$parameter,
                              sisagua_semestral_2022$parameter,
                              sisagua_semestral_2023$parameter,
                              sisagua_semestral_2024$parameter))

type_cp_dict <- tibble::tribble(
  ~type_capture_point_PRT,   ~type_capture_point_EN,
  "SAÍDA DO TRATAMENTO",     "Exit from treatment",
  "PONTO DE CAPTAÇÃO",       "Collection point",
  "SISTEMA DE DISTRIBUIÇÃO", "Distribution system",
  "PONTO DE CONSUMO",        "Consumption point"
)

parameter_group_dict <- tibble::tribble(
  ~parameter_group_PRT,                                      ~parameter_group_EN,
  "Substâncias Inorgânicas",                                 "Inorganic substances",
  "Parâmetros Organolépticos",                               "Organoleptic parameters",
  "Substâncias Orgânicas",                                   "Organic substances",
  "Agrotóxicos",                                             "Pesticides",
  "Radioatividade",                                          "Radioactivity",
  "Produtos Secundários de Desinfecção",                     "Disinfection byproducts",
  "Produtos Secundários de Desinfecção - 1º Trimestre",      "Disinfection byproducts - 1st quarter",
  "Produtos Secundários de Desinfecção - 2º Trimestre",      "Disinfection byproducts - 2nd quarter",
  "Outros",                                                  "Other",
  "Produtos Secundários de Desinfecção - 2º Bimestre",       "Disinfection byproducts - 2nd bimester",
  "Produtos Secundários de Desinfecção - 1º Bimestre",       "Disinfection byproducts - 1st bimester",
  "Produtos Secundários de Desinfecção - 3º Bimestre",       "Disinfection byproducts - 3rd bimester"
)

parameter_dict <- tibble::tribble(
  ~parameter_PRT, ~parameter_EN,
  "Nitrito (como N)", "Nitrite (as N)",
  "Urânio", "Uranium",
  "Sólidos dissolvidos totais", "Total dissolved solids",
  "1,2 Dicloroetano", "1,2-Dichloroethane",
  "Simazina", "Simazine",
  "Benzo[a]pireno", "Benzo[a]pyrene",
  "Diuron", "Diuron",
  "Sulfato", "Sulfate",
  "Cádmio", "Cadmium",
  "Pendimentalina", "Pendimethalin",
  "1,2 Diclorobenzeno", "1,2-Dichlorobenzene",
  "Cloreto", "Chloride",
  "Metolacloro", "Metolachlor",
  "Cobre", "Copper",
  "Manganês", "Manganese",
  "Amônia (como NH3)", "Ammonia (as NH3)",
  "Tetracloreto de Carbono", "Carbon tetrachloride",
  "Acrilamida", "Acrylamide",
  "Triclorobenzenos", "Trichlorobenzenes",
  "Monoclorobenzeno", "Monochlorobenzene",
  "Tebuconazol", "Tebuconazole",
  "Benzeno", "Benzene",
  "Aldrin + Dieldrin", "Aldrin + Dieldrin",
  "Alumínio", "Aluminum",
  "Tetracloroeteno", "Tetrachloroethene",
  "Surfactantes (como LAS)", "Surfactants (as LAS)",
  "Di (2-etilhexil) ftalato", "Di(2-ethylhexyl) phthalate",
  "Cromo", "Chromium",
  "Ferro", "Iron",
  "Nitrato (como N)", "Nitrate (as N)",
  "Selênio", "Selenium",
  "Zinco", "Zinc",
  "1,4 Diclorobenzeno", "1,4-Dichlorobenzene",
  "Arsênio", "Arsenic",
  "Estireno", "Styrene",
  "DDT + DDD + DDE", "DDT + DDD + DDE",
  "Cloreto de Vinila", "Vinyl chloride",
  "Permetrina", "Permethrin",
  "Parationa Metílica", "Methyl parathion",
  "Trifluralina", "Trifluralin",
  "Profenofós", "Profenofos",
  "Chumbo", "Lead",
  "Pentaclorofenol", "Pentachlorophenol",
  "Molinato", "Molinate",
  "Endossulfan (a, ß e sais)", "Endosulfan (alpha, beta and salts)",
  "Cor Aparente", "Apparent color",
  "Lindano (gama HCH)", "Lindane (gamma-HCH)",
  "Carbofurano", "Carbofuran",
  "Mercúrio", "Mercury",
  "Rádio-228", "Radium-228",
  "Clordano", "Chlordane",
  "Aldicarbe + Aldicarbesulfona + Aldicarbesulfóxido", "Aldicarb + Aldicarb sulfone + Aldicarb sulfoxide",
  "Endrin", "Endrin",
  "Xilenos", "Xylenes",
  "Mancozebe", "Mancozeb",
  "Diclorometano", "Dichloromethane",
  "Etilbenzeno", "Ethylbenzene",
  "Sódio", "Sodium",
  "Níquel", "Nickel",
  "Tolueno", "Toluene",
  "Atrazina", "Atrazine",
  "Dureza total", "Total hardness",
  "Cianeto", "Cyanide",
  "Carbendazim + benomil", "Carbendazim + Benomyl",
  "1,1 Dicloroeteno", "1,1-Dichloroethene",
  "Alacloro", "Alachlor",
  "Antimônio", "Antimony",
  "Clorpirifós + clorpirifós-oxon", "Chlorpyrifos + Chlorpyrifos-oxon",
  "2,4 D + 2,4,5 T", "2,4-D + 2,4,5-T",
  "Sulfeto de hidrogênio", "Hydrogen sulfide",
  "Tricloroeteno", "Trichloroethene",
  "Bário", "Barium",
  "1,2 Dicloroeteno (cis + trans)", "1,2-Dichloroethene (cis + trans)",
  "Terbufós", "Terbufos",
  "Glifosato + AMPA", "Glyphosate + AMPA",
  "Gosto e odor", "Taste and odor",
  "Rádio-226", "Radium-226",
  "Metamidofós", "Methamidophos",
  "Atividade alfa total", "Gross alpha activity",
  "Atividade beta total", "Gross beta activity",
  "2, 4, 6 Triclorofenol", "2,4,6-Trichlorophenol",
  "Ácidos haloacéticos total", "Total haloacetic acids",
  "Clorito", "Chlorite",
  "Bromato", "Bromate",
  "Amônia (como N)", "Ammonia (as N)",
  "Turbidez", "Turbidity",
  "Carbendazim", "Carbendazim",
  "Metribuzim", "Metribuzin",
  "Dioxano", "Dioxane",
  "Fipronil", "Fipronil",
  "Ciproconazol", "Cyproconazole",
  "Mercúrio Total", "Total mercury",
  "Tiametoxam", "Thiamethoxam",
  "Epicloridrina", "Epichlorohydrin",
  "Epoxiconazol", "Epoxiconazole",
  "Dimetoato + ometoato", "Dimethoate + Omethoate",
  "Clorotalonil", "Chlorothalonil",
  "Mancozebe + ETU", "Mancozeb + ETU",
  "Condutividade elétrica", "Electrical conductivity",
  "Fósforo Total", "Total phosphorus",
  "Sólidos Dissolvidos totais", "Total dissolved solids",
  "Demanda Química de Oxigênio (DQO)", "Chemical oxygen demand (COD)",
  "Metamidofós + Acefato", "Methamidophos + Acephate",
  "Tiodicarbe", "Thiodicarb",
  "Tiram", "Thiram",
  "Flutriafol", "Flutriafol",
  "Protioconazol + ProticonazolDestio", "Prothioconazole + Prothioconazole-desthio",
  "Paraquate", "Paraquat",
  "pH", "pH",
  "Nitrogênio Amoniacal Total", "Total ammoniacal nitrogen",
  "2,4 D", "2,4-D",
  "Picloram", "Picloram",
  "Malationa", "Malathion",
  "Atrazina + S-Clorotriazinas (Deetil-Atrazina - Dea, Deisopropil-Atrazina - Dia e Diaminoclorotriazina -Dact)", 
  "Atrazine + S-chlorotriazines (DEA, DIA, DACT)",
  "Propargito", "Propargite",
  "Hidroxi-Atrazina", "Hydroxyatrazine",
  "Demanda Bioquímica de Oxigênio (DBO)", "Biochemical oxygen demand (BOD)",
  "Oxigênio Dissolvido (OD)", "Dissolved oxygen (DO)",
  "Difenoconazol", "Difenoconazole",
  "Cor Verdadeira", "True color",
  "Ametrina", "Ametryn",
  "Atividade beta total (após subtração de K-40)", "Gross beta activity (excluding K-40)",
  "Atividade alfa total (após subtração de K-40)", "Gross alpha activity (excluding K-40)",
  "Cloraminas Total", "Total chloramines",
  "2, 4 Diclorofenol", "2,4-Dichlorophenol",
  "Ácidos Haloacéticos Total", "Total haloacetic acids",
  "Clorato", "Chlorate",
  "N-nitrosodimetilamina", "N-nitrosodimethylamine",
  "Trihalometanos Total", "Total trihalomethanes"
)

dictionary <- function(df) {
  new_df <- df |> 
    left_join(type_cp_dict, by = c("type_capture_point" = "type_capture_point_PRT")) |>
    left_join(parameter_group_dict, by = c("parameter_group" = "parameter_group_PRT")) |>
    left_join(parameter_dict, by = c("parameter" = "parameter_PRT")) |>
    select(
      -type_capture_point,
      -parameter_group,
      -parameter
    )
  return(new_df)
}