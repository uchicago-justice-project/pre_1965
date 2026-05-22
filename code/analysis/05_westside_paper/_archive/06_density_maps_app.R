rm(list = ls())

library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(glue)
library(patchwork)
library(scales)

# --- Configuration ---
DECADES <- c(1940, 1950, 1960, 1970)
CRS_M <- 26916 # UTM 16N (Meters)

# Mapping UI labels to file stubs
POINT_VARS <- c(
  "Share Units w/ >1 PPR" = "n_ppr_share",
  "Share Black"        = "n_black_share",
  "Share Population"   = "n_pop_share", 
  "Share Renter Occ. Units" = "n_renter_occ_share",
  "Share Age 20–29"    = "n_20_29_share",
  "Share Age 15–24"    = "n_15_24_share",
  "Share Age 15–19"    = "n_15_19_share",
  "Share Males 15–19"    = "n_males_15_19_share",
  "Share Males 20–29"   = "n_male_20_29_share", 
  "Total Black"        = "n_black_total",
  "Total Population"   = "n_pop_total", 
  "Total Renter Occ. Units" = "n_renter_occ_total",
  "Total Units w/ > 1 PPR"   = "n_ppr_total",
  "Total Age 20–29"    = "n_20_29_total",
  "Total Males 20–29" = "n_male_20_29_total",
  "Total Age 15-24" = "n_15_24_total", 
  "Total Age 15-19" = "n_15_19_total",
  "Total Males 15-19" = "n_male_15_19_total"
)

# Colors for variable-specific ellipses
VAR_COLORS <- c(
  "n_ppr_share" = "orange",
  "n_black_share" = "orange",
  "n_pop_share"   = "orange", 
  "n_renter_occ_share" = "orange",
  "n_20_29_share" = "orange",
  "n_15_24_share" = "orange",
  "n_male_20_29_share" = "orange", 
  "n_ppr_total" = "orange",
  "n_black_total" = "orange",
  "n_pop_total"   = "orange", 
  "n_renter_occ_total" = "orange",
  "n_20_29_total" = "orange",
  "n_15_24_total" = "orange",
  "n_male_20_29_total" = "orange", 
  "n_15_19_share" = "orange", 
  "n_male_15_19_share" = "orange", 
  "n_15_19_total" = "orange", 
  "n_male_15_19_total" = "orange"
)

# --- Data Loading (Projected Once) ---
census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson", quiet = TRUE) %>% 
  st_transform(CRS_M)

ellipses_homs <- st_read("data/mst/for_paper/homs_ellipses_all_years.geojson", quiet = TRUE) %>% 
  st_transform(CRS_M)

# Environments for caching spatial data to prevent repeated disk reads
.points_cache <- new.env(parent = emptyenv())
.ell_cache    <- new.env(parent = emptyenv())

# Helper to fetch and cache point data
get_points <- function(point_name, year) {
  key <- paste0(point_name, year)
  if (!exists(key, envir = .points_cache)) {
    fp <- glue("output/westside_paper/ellipses/map_images_1940_65/points_{point_name}_{year}.shp")
    if (file.exists(fp)) {
      pts <- st_read(fp, quiet = TRUE) %>% st_transform(CRS_M)
      assign(key, pts, envir = .points_cache)
    } else { assign(key, NULL, envir = .points_cache) }
  }
  get(key, envir = .points_cache)
}

# Helper to fetch and cache variable ellipses
get_var_ellipses <- function(point_name) {
  if (!exists(point_name, envir = .ell_cache)) {
    fp <- glue("data/mst/for_paper/{point_name}_ellipses_all_years.geojson")
    if (file.exists(fp)) {
      ell <- st_read(fp, quiet = TRUE) %>% st_transform(CRS_M)
      assign(point_name, ell, envir = .ell_cache)
    } else { assign(point_name, NULL, envir = .ell_cache) }
  }
  get(point_name, envir = .ell_cache)
}

# --- UI ---
ui <- fluidPage(
  titlePanel("West Side Density & Ellipse Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Variable", choices = POINT_VARS),
      sliderInput("bandwidth_m", "KDE Smoothness (Bandwidth)", min = 100, max = 1000, value = 100),
      hr(),
      checkboxInput("show_var_ellipse", "Show Variable Ellipse", value = TRUE),
      sliderInput("var_alpha", "Ellipse Opacity", min = 0, max = 0.6, value = 0),
      checkboxInput("show_homs", "Show Homicide Ellipse (Red)", value = TRUE),
      hr(),
      actionButton("save_plot", "Save High-Res PNG", class = "btn-success")
    ),
    mainPanel(
      plotOutput("panel", height = "850px")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # 1. Reactive KDE Calculation (triggers only on slider/var change)
  kde_list <- reactive({
    req(input$var, input$bandwidth_m)
    bb <- st_bbox(census_west_outline)
    
    lapply(DECADES, function(y) {
      pts <- get_points(input$var, y)
      if (is.null(pts) || nrow(pts) < 5) return(NULL)
      
      coords <- st_coordinates(pts)
      kde <- MASS::kde2d(coords[,1], coords[,2], h = input$bandwidth_m, n = 200,
                         lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
      
      expand.grid(X = kde$x, Y = kde$y) %>% 
        mutate(density = as.vector(kde$z), year = y)
    })
  })
  
  # 2. Main Plotting Logic
  panel_plot <- reactive({
    kdes <- kde_list()
    all_densities <- bind_rows(kdes)
    d_limits <- if(!is.null(all_densities) && nrow(all_densities) > 0) range(all_densities$density) else c(0,1)
    
    plots <- lapply(seq_along(DECADES), function(i) {
      y <- DECADES[i]
      df <- kdes[[i]]
      
      # Base Layer: West Side Outline
      p <- ggplot() +
        geom_sf(data = census_west_outline, fill = "white", color = "grey50", linewidth = 0.2)
      
      # KDE Density Layer
      if (!is.null(df)) {
        p <- p + geom_raster(data = df, aes(X, Y, fill = density), interpolate = TRUE)
      }
      
      # Variable Ellipse Layer
      if (input$show_var_ellipse) {
        v_ell_all <- get_var_ellipses(input$var)
        if (!is.null(v_ell_all)) {
          v_ell_y <- filter(v_ell_all, year == y)
          v_color <- VAR_COLORS[[input$var]] %||% "blue"
          p <- p + geom_sf(data = v_ell_y, fill = v_color, linewidth = 0.8, color = v_color, alpha = input$var_alpha)
        }
      }
      
      # Homicide Ellipse Layer
      if (input$show_homs) {
        h_ell <- filter(ellipses_homs, year == y)
        p <- p + geom_sf(data = h_ell, fill = NA, color = "red", linewidth = 0.8)
      }
      
      
      p + scale_fill_viridis_c(option = "inferno", limits = d_limits, oob = squish, na.value = "white") +
        theme_void() + labs(title = y) +
        theme(legend.position = "none",
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA)) +
        geom_sf(data = census_west_outline, fill = NA, color = "grey50", linewidth = 0.2)
      
    })
    
    wrap_plots(plots, ncol = 2) +
      plot_layout(guides = "collect") +
      plot_annotation(title = names(POINT_VARS)[POINT_VARS == input$var],
                      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                                    plot.background = element_rect(fill = "white", color = NA)))
  })
  
  output$panel <- renderPlot({ panel_plot() }, res = 110)
  
  observeEvent(input$save_plot, {
    fn <- glue("output/westside_paper/ellipses/density_maps/{input$var}_{format(Sys.time(), '%H%M%S')}.png")
    ggsave(fn, panel_plot(), width = 10, height = 12, dpi = 300)
    showNotification(paste("Saved to", fn))
  })
}

shinyApp(ui, server)


# What happened in 1950? 
# Look at the density distribution over time 
# Look at general population density 
# Look at distribution 
# Number of renters 
# What is the measure of ppp 


