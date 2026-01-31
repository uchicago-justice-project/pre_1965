
rm(list = ls())
source("header.R")

ellipses_n_black_share <- st_read(glue("data/mst/for_paper/n_black_share_ellipses_all_years.geojson"))
ellipses_n_ppr_share <- st_read(glue("data/mst/for_paper/n_ppr_share_ellipses_all_years.geojson"))
ellipses_n_20_29_share <- st_read(glue("data/mst/for_paper/n_20_29_share_ellipses_all_years.geojson"))
ellipses_n_male_20_29_share <- st_read(glue("data/mst/for_paper/n_male_20_29_share_ellipses_all_years.geojson"))
ellipses_n_black_total <- st_read(glue("data/mst/for_paper/n_black_total_ellipses_all_years.geojson"))
ellipses_n_ppr_total<- st_read(glue("data/mst/for_paper/n_ppr_total_ellipses_all_years.geojson"))
ellipses_n_20_29_total <- st_read(glue("data/mst/for_paper/n_20_29_total_ellipses_all_years.geojson"))
ellipses_n_male_20_29_total <- st_read(glue("data/mst/for_paper/n_male_20_29_total_ellipses_all_years.geojson"))
ellipses_n_15_24_share <- st_read(glue("data/mst/for_paper/n_15_24_share_ellipses_all_years.geojson"))
ellipses_n_15_24_total <-  st_read(glue("data/mst/for_paper/n_15_24_total_ellipses_all_years.geojson"))
ellipses_homs <- st_read(glue("data/mst/for_paper/homs_ellipses_all_years.geojson"))

census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson")



plot_ellipses_map <- function(year,
                              outline_sf = census_west_outline,
                              ellipses_list,    
                              which = names(ellipses_list),
                              alpha = 0.2,
                              palette = NULL      
                              ) {
  
  stopifnot(all(which %in% names(ellipses_list)))
  
  # subset to chosen layers and bind into one sf with a 'layer' column
  ellipses_long <- imap_dfr(
    ellipses_list[which],
    ~ .x %>%
      filter(year == !!year) %>%
      mutate(layer = .y)
  )
  
  p <- ggplot() +
    geom_sf(data = outline_sf, fill = "white", color = "grey50") +
    geom_sf(
      data = ellipses_long,
      aes(fill = layer, color = layer),
      alpha = alpha
    ) +
    theme_void() +
    labs(title = glue("{year}"), fill = NULL, color = NULL) +
    theme(
      plot.title  = element_text(size = 24),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    coord_sf(expand = TRUE)
  
  # optional: fixed colors (and legend updates automatically based on 'which')
  if (!is.null(palette)) {
    p <- p +
      scale_fill_manual(values = palette, breaks = which) +
      scale_color_manual(values = palette, breaks = which)
    }
  
  p
  
  }



ellipses_list <- list(
  "Share of Black Population" = ellipses_n_black_share,
  "Share of Households w/ > 1 PPR"   = ellipses_n_ppr_share,
  "Share Age 20–29" = ellipses_n_20_29_share,
  "Share Age 15-24" = ellipses_n_15_24_share,
  "Share of Males Aged 20–29" = ellipses_n_male_20_29_share,
  "Total Black Population" = ellipses_n_black_total,
  "Total Households w/ > 1 PPR"   = ellipses_n_ppr_total,
  "Total Age 20–29" = ellipses_n_20_29_total,
  "Total Males Aged 20–29" = ellipses_n_male_20_29_total,
  "Total Age 15-24" = ellipses_n_15_24_total, 
  "Homicides" = ellipses_homs
)

palette <- c(
  "Share of Black Population" = "green",
  "Share of Households w/ > 1 PPR" = "purple",
  "Share Age 20–29" = "blue",
  "Share of Males Aged 20–29" = "lightblue", 
  "Share Age 15-24" = "orange",
  "Total Black Population" = "green",
  "Total Households w/ > 1 PPR"   = "purple",
  "Total Age 20–29" = "blue",
  "Total Males Aged 20–29" = "lightblue",
  "Total Age 15-24" = "orange",
  "Homicides" = "red"
)

plot_ellipses_map(
  year = 1940,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Total Black Population", 
            "Total Age 15-24",
            "Homicides"),   
  palette = palette
)

plot_ellipses_map(
  year = 1970,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Share of Black Population", 
            "Share Age 15-24",
            "Homicides"),   # pick any subset
  palette = palette
)

