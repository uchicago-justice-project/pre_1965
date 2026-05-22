# app.R
rm(list = ls())

library(shiny)
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(glue)
library(patchwork)
library(ggnewscale)

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

# --- Load housing projects and highways ---
housing_projects <- st_read("data/mst/for_paper/projects.geojson", quiet = TRUE) %>%
  mutate(opened = as.numeric(opened))

highways <- st_read("data/mst/for_paper/select_hways.geojson", quiet = TRUE)

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

# --- Updated plotting function with projects and highways ---
plot_ellipses_map <- function(year,
                              outline_sf,
                              ellipses_list,
                              which = names(ellipses_list),
                              alpha = 0.2,
                              palette = NULL,
                              show_outline = TRUE,
                              show_projects = FALSE,
                              show_highway = FALSE,
                              projects_sf = NULL,
                              highway_sf = NULL) {
  stopifnot(all(which %in% names(ellipses_list)))
  
  ellipses_long <- imap_dfr(
    ellipses_list[which],
    ~ .x %>%
      filter(year == !!year) %>%
      mutate(layer = .y)
  )
  
  p <- ggplot()
  
  if (show_outline) {
    p <- p + geom_sf(data = outline_sf, fill = "white", color = "grey50")
  }
  
  p <- p +
    geom_sf(
      data = ellipses_long,
      aes(fill = layer, color = layer),
      alpha = alpha
    )
  
  if (!is.null(palette)) {
    p <- p +
      scale_fill_manual(values = palette, breaks = which, name = "Ellipses") +
      scale_color_manual(values = palette, breaks = which, name = "Ellipses")
  }
  
  # Add housing projects if requested
  if (show_projects && !is.null(projects_sf)) {
    # Filter projects that were opened by this year and add decade
    projects_filtered <- projects_sf %>%
      filter(opened <= year) %>%
      mutate(
        decade = case_when(
          opened < 1940 ~ "Pre-1940",
          opened >= 1940 & opened < 1950 ~ "1940s",
          opened >= 1950 & opened < 1960 ~ "1950s",
          opened >= 1960 & opened < 1970 ~ "1960s",
          opened >= 1970 ~ "1970s",
          TRUE ~ "Unknown"
        ),
        decade = factor(decade, levels = c("Pre-1940", "1940s", "1950s", "1960s", "1970s"))
      )
    
    if (nrow(projects_filtered) > 0) {
      # Define decade color palette
      decade_colors <- c(
        "Pre-1940" = "#8B4513",  # brown
        "1940s" = "#8B4513",     # goldenrod
        "1950s" = "#8B4513",     # dark orange
        "1960s" = "#8B4513",     # crimson
        "1970s" = "#8B4513"      # dark magenta
      )
      
      p <- p +
        new_scale_fill() +
        geom_sf(data = projects_filtered, aes(fill = decade), color = "black", 
                alpha = 0.7, linewidth = 0.3) +
        scale_fill_manual(
          name = "Housing Projects",
          values = decade_colors,
          breaks = c("Pre-1940", "1940s", "1950s", "1960s", "1970s"),
          drop = FALSE
        )
    }
  }
  
  # Add highway if requested
  # Eisenhower Expressway was built in 1949, so show it from 1949 onward
  if (show_highway && !is.null(highway_sf) && year >= 1949) {
    p <- p +
      geom_sf(data = highway_sf, color = "navy", linewidth = 1.5, alpha = 0.8)
  }
  
  p <- p +
    theme_void() +
    labs(title = glue("{year}")) +
    theme(
      plot.title = element_text(size = 18),
      legend.position = "right",
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    coord_sf(expand = TRUE)
  
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
      radioButtons(
        "year_interval",
        "Year interval",
        choices = c("10 years" = 10, "5 years" = 5),
        selected = 10
      ),
      
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
      
      checkboxInput("show_projects", "Show housing projects", value = FALSE),
      
      checkboxInput("show_highway", "Show Eisenhower Expressway (1949+)", value = FALSE),
      
      helpText("Note: Housing projects appear based on their opening year. ",
               "The Eisenhower Expressway appears from 1949 onward.")
    ),
    
    mainPanel(
      plotOutput("map_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to generate year sequences based on interval
  available_years <- reactive({
    interval <- as.numeric(input$year_interval)
    
    # Find min and max years from the data
    min_year <- min(all_years)
    max_year <- max(all_years)
    
    # Generate sequence based on interval
    seq(min_year, max_year, by = interval)
  })
  
  # Update year choices when interval changes
  observe({
    years <- available_years()
    updateSelectizeInput(
      session,
      "years",
      choices = years,
      selected = years
    )
  })
  
  output$map_plot <- renderPlot({
    req(input$which, input$years)
    yrs <- sort(as.integer(input$years))
    
    validate(need(length(yrs) > 0, "Select at least one year."))
    
    # Create all plots without legends
    plots <- lapply(yrs, function(y) {
      plot_ellipses_map(
        year = y,
        outline_sf = census_west_outline,
        ellipses_list = ellipses_list,
        which = input$which,
        alpha = input$alpha,
        palette = palette,
        show_outline = input$show_outline,
        show_projects = input$show_projects,
        show_highway = input$show_highway,
        projects_sf = housing_projects,
        highway_sf = highways
      ) +
        theme(legend.position = "none")
    })
    
    # Create one reference plot with legends to extract
    ref_plot <- plot_ellipses_map(
      year = yrs[length(yrs)],  # Use last year to ensure all decades appear
      outline_sf = census_west_outline,
      ellipses_list = ellipses_list,
      which = input$which,
      alpha = input$alpha,
      palette = palette,
      show_outline = input$show_outline,
      show_projects = input$show_projects,
      show_highway = input$show_highway,
      projects_sf = housing_projects,
      highway_sf = highways
    ) +
      theme(legend.position = "right",
            legend.box = "vertical")
    
    # Create layout based on number of years
    n_years <- length(yrs)
    
    if (n_years == 4) {
      # For 4 years: 2x2 grid with legend on the right
      design <- "
        AABB
        CCDD
      "
      combined <- wrap_plots(
        A = plots[[1]], B = plots[[2]], 
        C = plots[[3]], D = plots[[4]],
        design = design
      )
    } else if (n_years == 7) {
      # For 7 years (5-year intervals): 4x2 grid with legend on the right
      design <- "
        AABB
        CCDD
        EEFF
        GG##
      "
      combined <- wrap_plots(
        A = plots[[1]], B = plots[[2]], 
        C = plots[[3]], D = plots[[4]], 
        E = plots[[5]], F = plots[[6]],
        G = plots[[7]],
        design = design
      )
    } else {
      # Default: automatic layout
      combined <- wrap_plots(c(plots, list(ref_plot)), ncol = 3)
    }
    
    combined
    
  }, res = 120)
  
}

shinyApp(ui, server)