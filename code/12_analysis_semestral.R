# Analyze SISAGUA semestral data -----
library("dplyr")
library("readr")
library("tidyr")
library("sf")
library("geobr")
library("shiny")
library("leaflet")
library("htmltools")
library("purrr")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
            "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
            "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
            "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
            "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
            "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
            "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")

# Load Brazilian municipalities and country boundary
br_mun <- read_municipality(year = 2020)
st_geometry(br_mun) <- "geometry"

# Load cleaned data --------------------------------------------------------
sisagua_matched <- readRDS("/data/brazil_health/SISAGUA/sisagua_semestral_cps_matched.RDS")

# ---- Clean municipalities ----
br_mun_clean <- br_mun |>
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) |>
  st_transform(crs = 4326)  # WGS84 for Leaflet

# ---- Define fixed years ----
years <- 2014:2024

# ---- Precompute yearly sf datasets with all muni-year combinations ----
all_muni_years <- expand.grid(
  code_muni = br_mun_clean$code_muni,
  year = years
)

# Summarize SISAGUA
summary_tbl <- sisagua_matched |>
  group_by(muni_code, year) |>
  summarise(
    n_exceeded = sum(limit_exceeded == 1, na.rm = TRUE),
    exceeded_groups = paste(unique(parameter_group_EN[limit_exceeded == 1]), collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(exceeded_groups = ifelse(n_exceeded == 0 | exceeded_groups == "", "None", exceeded_groups))

# Join with all municipality-year combinations
full_summary <- all_muni_years |>
  left_join(summary_tbl, by = c("code_muni" = "muni_code", "year" = "year")) |>
  mutate(
    n_exceeded = ifelse(is.na(n_exceeded), 0, n_exceeded),
    exceeded_groups = ifelse(is.na(exceeded_groups), "None", exceeded_groups)
  )

# Join with polygon data
sf_summary_all_years <- br_mun_clean |>
  left_join(full_summary, by = "code_muni")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Water Quality Exceedance Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year",
        "Select year:",
        min = min(years),
        max = max(years),
        value = min(years),
        step = 1,
        sep = ""
      ),
      helpText("Select a year to compare how water quality changes over time.")
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Filter data by selected year
  filtered_data <- reactive({
    sf_summary_all_years |> filter(year == input$year)
  })
  
  # Color palette
  pal <- reactive({
    colorNumeric(
      palette = "Reds",
      domain = filtered_data()$n_exceeded,
      na.color = "transparent"
    )
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal()(n_exceeded),
        weight = 0.5,
        color = "white",
        fillOpacity = 0.8,
        label = ~lapply(
          paste0(
            "<strong>", name_muni, "</strong><br/>",
            "<strong># Exceedances:</strong> ", n_exceeded, "<br/>",
            "<strong>Groups:</strong> ", exceeded_groups
          ),
          HTML
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal(),
        values = ~n_exceeded,
        title = "Number of<br>Exceedances",
        position = "bottomright"
      )
  })
}

# ---- Run app ----
shinyApp(ui, server)


