
rm(list = ls())

library(lubridate)
library(htmltools)
library(shiny)
library(rsconnect)
library(leaflet)
library(sf)
library(dplyr)

# Load data safely
census_west_homs_geom <- readRDS("census_west_homs_geom.rds")
homs_west             <- readRDS("homs_west.rds")
projects_west         <- readRDS("projects_west.rds")
westside              <- readRDS("westside.rds")

# Dropdown choices
choices_select <-  c("Total Homicides"         = "homicides",
                     "Residual Homicides (Size and Pop. Adj.)" = "res_homicides_pop_size",
                     "Residual Homicides (Size Adj.)" = "res_homicides_size",
                     "Residual Homicides (Full Adj.)" = "res_homicides_all",
                     "Total Housing Units"     = "total_housing_units",
                     "Total Population"        = "total_pop",
                     "Under Age 25 (%)"        = "share_under_25",
                     "% Units Over 1 PPR"      = "share_over_1_ppr",
                     "Black Share (%)"         = "share_black",
                     "Black * Over 1 PPR (%)"  = "black_share_over_1_ppr",
                     "Median Home Value ($)"   = "median_value",
                     "Vacant Homes (%)"        = "share_vacant_homes",
                     "In Labor Force (%)"      = "in_labor_force",
                     "Foreign Born (%)"        = "foreign_born")

# UI
ui <- fluidPage(
  tags$style(type = "text/css", "
    .control-panel {
      background: white;
      padding: 6px;
      font-size: 12px;
      border-radius: 4px;
      box-shadow: 0 0 10px rgba(0,0,0,0.15);
      z-index: 1000;
    }
    .control-panel .form-control {
      font-size: 12px;
      height: 30px;
      padding: 2px 6px;
    }
    .control-panel .form-group {
      margin-bottom: 6px;
    }
    .control-panel .irs--shiny .irs-bar,
    .control-panel .irs--shiny .irs-line {
      height: 4px;
    }
  "),
  
  fluidRow(
    column(width = 6,
           leafletOutput("map1", height = "400px"),
           absolutePanel(id = "controlPanel1", class = "control-panel", fixed = TRUE,
                         top = 200, left = 20, width = 180,
                         selectInput("shade_var1", "Shade census tracts by:", choices = choices_select, selected = "homicides"),
                         sliderInput("selected_year1", "Select Year:", min = 1940, max = 1990, step = 10, value = 1960, sep = "", animate = TRUE)
           )),
    column(width = 6,
           leafletOutput("map2", height = "400px"),
           absolutePanel(id = "controlPanel2", class = "control-panel", fixed = TRUE,
                         top = 200, left = "52%", width = 180,
                         selectInput("shade_var2", "Shade census tracts by:", choices = choices_select, selected = "share_over_1_ppr"),
                         sliderInput("selected_year2", "Select Year:", min = 1940, max = 1990, step = 10, value = 1960, sep = "", animate = TRUE)
           ))
  ),
  
  fluidRow(
    column(width = 6,
           leafletOutput("map3", height = "400px"),
           absolutePanel(id = "controlPanel3", class = "control-panel", fixed = TRUE,
                         bottom = 15, left = 20, width = 180,
                         selectInput("shade_var3", "Shade census tracts by:", choices = choices_select, selected = "share_black"),
                         sliderInput("selected_year3", "Select Year:", min = 1940, max = 1990, step = 10, value = 1960, sep = "", animate = TRUE)
           )),
    column(width = 6,
           leafletOutput("map4", height = "400px"),
           absolutePanel(id = "controlPanel4", class = "control-panel", fixed = TRUE,
                         bottom = 15, left = "52%", width = 180,
                         selectInput("shade_var4", "Shade census tracts by:", choices = choices_select, selected = "median_value"),
                         sliderInput("selected_year4", "Select Year:", min = 1940, max = 1990, step = 10, value = 1960, sep = "", animate = TRUE)
           ))
  )
)

# Server logic
server <- function(input, output, session) {
  
  filtered_data <- function(year_input) {
    if (is.null(year_input)) return(list(census = NULL, homs = NULL, projects = NULL))
    year_input <- as.numeric(year_input)
    
    list(
      census   = census_west_homs_geom %>% filter(as.numeric(year) == year_input),
      homs     = homs_west %>% filter(year_decade == year_input),
      projects = projects_west %>% filter(opened <= year_input)
    )
  }
  
  render_map <- function(map_id, year_input, var_input) {
    data <- filtered_data(year_input)
    if (is.null(data$census)) return(NULL)
    
    pal <- colorNumeric("YlOrRd", domain = unlist(data$census[[var_input]]))
    
    leafletProxy(map_id) %>%
      clearShapes() %>%
      addPolygons(
        data = data$census,
        fillColor = ~pal(get(var_input)),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~sprintf(
          "GICS: %s<br>Homicides: %s<br>Over 1 PPR: %s<br>Black Share: %s<br>Under Age 25: %s<br>Residual Adj. Size: %s<br>Residual Adj. Size & Pop: %s",
          GISJOIN_1970, homicides, round(share_over_1_ppr, 2), round(share_black, 2), round(share_under_25, 2), round(res_homicides_size, 0), round(res_homicides_pop_size, 0)
        ) %>% lapply(HTML)
      ) %>%
      addPolygons(data = data$projects,
                  color = "blue",
                  label = ~paste0(name, ": ", opened))
  }
  
  # Generate 4 maps with independent controls
  for (i in 1:4) {
    local({
      map_id_i       <- paste0("map", i)
      shade_var_i    <- paste0("shade_var", i)
      year_input_i   <- paste0("selected_year", i)
      
      output[[map_id_i]] <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addPolygons(data = westside, fillOpacity = 0.7)
      })
      
      observe({
        render_map(
          map_id     = map_id_i,
          year_input = input[[year_input_i]],
          var_input  = input[[shade_var_i]]
        )
      })
    })
  }
}

shinyApp(ui = ui, server = server)


