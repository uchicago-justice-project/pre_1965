# app.R
rm(list = ls())

library(shiny)
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(glue)
library(patchwork)

# If you rely on packages loaded in header.R, keep this:
source("header.R")

# --- Load data (same as your script) ---
ellipses_n_black_share <- st_read(glue("data/mst/for_paper/n_black_share_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_pop_share <- st_read(glue("data/mst/for_paper/n_pop_share_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_renter_occ_share <- st_read(glue("data/mst/for_paper/n_renter_occ_share_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_ppr_share <- st_read(glue("data/mst/for_paper/n_ppr_share_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_20_29_share <- st_read(glue("data/mst/for_paper/n_20_29_share_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_male_20_29_share <- st_read(glue("data/mst/for_paper/n_male_20_29_share_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_15_24_share <- st_read(glue("data/mst/for_paper/n_15_24_share_ellipses_all_years.geojson"))

ellipses_n_black_total <- st_read(glue("data/mst/for_paper/n_black_total_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_pop_total <- st_read(glue("data/mst/for_paper/n_pop_total_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_renter_occ_total <- st_read(glue("data/mst/for_paper/n_renter_occ_total_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_ppr_total <- st_read(glue("data/mst/for_paper/n_ppr_total_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_20_29_total <- st_read(glue("data/mst/for_paper/n_20_29_total_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_male_20_29_total <- st_read(glue("data/mst/for_paper/n_male_20_29_total_ellipses_all_years.geojson"), quiet = TRUE)
ellipses_n_15_24_total <-  st_read(glue("data/mst/for_paper/n_15_24_total_ellipses_all_years.geojson"))

ellipses_homs <- st_read(glue("data/mst/for_paper/homs_ellipses_all_years.geojson"), quiet = TRUE)

census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson", quiet = TRUE)

# --- Your list + palette (same structure) ---
ellipses_list <- list(
  "Share of Black Population" = ellipses_n_black_share,
  "Share of West Side Population" = ellipses_n_pop_share,
  "Share of Renter Occ. Units" = ellipses_n_renter_occ_share, 
  "Share of Units w/ > 1 PPR"   = ellipses_n_ppr_share,
  "Share Age 20–29" = ellipses_n_20_29_share,
  "Share Age 15-24" = ellipses_n_15_24_share,
  "Share of Males Aged 20–29" = ellipses_n_male_20_29_share,
  "Total Black Population" = ellipses_n_black_total,
  "Total Population" = ellipses_n_pop_total, 
  "Total Renter Occ. Units" = ellipses_n_renter_occ_total,
  "Total Units w/ > 1 PPR"   = ellipses_n_ppr_total,
  "Total Age 20–29" = ellipses_n_20_29_total,
  "Total Males Aged 20–29" = ellipses_n_male_20_29_total,
  "Total Age 15-24" = ellipses_n_15_24_total, 
  "Homicides" = ellipses_homs
)

palette <- c(
  "Share of Black Population" = "green",
  "Share of West Side Population" = "pink",
  "Share of Renter Occ. Units" = "darkred", 
  "Share of Units w/ > 1 PPR" = "purple",
  "Share Age 20–29" = "blue",
  "Share of Males Aged 20–29" = "lightblue", 
  "Share Age 15-24" = "orange",
  "Total Black Population" = "green",
  "Total Population" = "pink", 
  "Total Renter Occ. Units" = "darkred",
  "Total Units w/ > 1 PPR"   = "purple",
  "Total Age 20–29" = "blue",
  "Total Males Aged 20–29" = "lightblue",
  "Total Age 15-24" = "orange",
  "Homicides" = "red"
)

# --- Your plotting function (slightly extended with outline toggle) ---
plot_ellipses_map <- function(year,
                              outline_sf,
                              ellipses_list,
                              which = names(ellipses_list),
                              alpha = 0.2,
                              palette = NULL,
                              show_outline = TRUE) {
  stopifnot(all(which %in% names(ellipses_list)))
  
  ellipses_long <- imap_dfr(
    ellipses_list[which],
    ~ .x %>%
      filter(year == !!year) %>%
      mutate(layer = .y)
  )
  
  p <- ggplot()
  
  if (show_outline) {
    p <- p + geom_sf(data = outline_sf, fill = NA, color = "black")
  }
  
  p <- p +
    geom_sf(
      data = ellipses_long,
      aes(fill = layer, color = layer),
      alpha = alpha
    ) +
    theme_void() +
    labs(title = glue("{year}"), fill = NULL, color = NULL) +
    theme(
      plot.title = element_text(size = 18),
      legend.position = "right",
      plot.margin = margin(0, 0, 0, 0)
    ) +
    coord_sf(expand = TRUE)
  
  if (!is.null(palette)) {
    p <- p +
      scale_fill_manual(values = palette, breaks = which) +
      scale_color_manual(values = palette, breaks = which)
  }
  
  p
}

# --- Infer available years from the data (robust) ---
all_years <- sort(unique(unlist(lapply(ellipses_list, \(x) x$year))))
if (length(all_years) == 0) all_years <- c(1940, 1950, 1960, 1970)

# =========================
# Shiny
# =========================
ui <- fluidPage(
  titlePanel("West Side Ellipse Maps"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "years",
        "Years (one map per year)",
        choices = all_years,
        selected = all_years,   # default: show all decades
        multiple = TRUE
      ),
      
      selectizeInput(
        "which",
        "Ellipses to plot",
        choices = names(ellipses_list),
        selected = c("Homicides"),
        multiple = TRUE,
        options = list(placeholder = "Pick one or more layers…")
      ),
      
      sliderInput(
        "alpha",
        "Fill transparency (alpha)",
        min = 0, max = 1, value = 0.2, step = 0.05
      ),
      
      checkboxInput("show_outline", "Show outline", value = TRUE),
      
      helpText("Tip: pick multiple layers; the legend updates automatically.")
    ),
    
    mainPanel(
      plotOutput("map_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  output$map_plot <- renderPlot({
    req(input$which, input$years)
    yrs <- sort(as.integer(input$years))
    
    validate(need(length(yrs) > 0, "Select at least one year."))
    
    plots <- lapply(yrs, function(y) {
      plot_ellipses_map(
        year = y,
        outline_sf = census_west_outline,
        ellipses_list = ellipses_list,
        which = input$which,
        alpha = input$alpha,
        palette = palette,
        show_outline = input$show_outline
      ) +
        theme(legend.position = "none")  # we'll add one shared legend below
    })
    
    # combine and collect a single legend
    combined <- wrap_plots(plots, ncol = 2) +
      plot_layout(guides = "collect") &
      theme(legend.position = "right")
    
    combined
  }, res = 120)
  
}

shinyApp(ui, server)



# The ellipses do not currently now very useful data 
# Investigate how the ellipses compare to the concentration of points - if there is a clear concentration of points, how can we show it more clearly 

