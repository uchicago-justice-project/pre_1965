
rm(list = ls())
source("header.R")
library(ggspatial)
library(prettymapr)

#Creates maps for ellipses with additional information like housing projects and highway construction 
# Census variables blocked out since no longer in use

# ellipses_n_black_share <- st_read(glue("data/mst/for_paper/n_black_share_ellipses_all_years.geojson"))
# ellipses_n_ppr_share <- st_read(glue("data/mst/for_paper/n_ppr_share_ellipses_all_years.geojson"))
# ellipses_n_20_29_share <- st_read(glue("data/mst/for_paper/n_20_29_share_ellipses_all_years.geojson"))
# ellipses_n_male_20_29_share <- st_read(glue("data/mst/for_paper/n_male_20_29_share_ellipses_all_years.geojson"))
# ellipses_n_black_total <- st_read(glue("data/mst/for_paper/n_black_total_ellipses_all_years.geojson"))
# ellipses_n_ppr_total<- st_read(glue("data/mst/for_paper/n_ppr_total_ellipses_all_years.geojson"))
# ellipses_n_20_29_total <- st_read(glue("data/mst/for_paper/n_20_29_total_ellipses_all_years.geojson"))
# ellipses_n_male_20_29_total <- st_read(glue("data/mst/for_paper/n_male_20_29_total_ellipses_all_years.geojson"))
# ellipses_n_15_24_share <- st_read(glue("data/mst/for_paper/n_15_24_share_ellipses_all_years.geojson"))
# ellipses_n_15_24_total <-  st_read(glue("data/mst/for_paper/n_15_24_total_ellipses_all_years.geojson"))
ellipses_homs <- st_read(glue("data/mst/for_paper/homs_ellipses_all_years.geojson"))

census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson")
select_hways_west <- st_read("data/mst/for_paper/select_hways.geojson")
cha_projects_west <- st_read("data/mst/for_paper/projects.geojson")
reg_data <- st_read("data/mst/for_paper/reg_data.geojson") 

library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(glue)
library(patchwork)

plot_ellipses_map <- function(year,
                              outline_sf = census_west_outline,
                              projects = cha_projects_west, 
                              highways = select_hways_west, 
                              ellipses_list,    
                              which = names(ellipses_list),
                              alpha = 0.2,
                              housing = TRUE,
                              hway = TRUE,
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
  
  
  projects_filtered <- projects %>%
    filter(opened <= year) %>%
    mutate(is_new = opened > (year - 10)) #%>%
    #st_centroid()
  
  filename_h <- "_hp"
  alpha_h <- 1
  
  if (!housing) {
    filename_h <- ""
    alpha_h <- 0
  } 
  
  p <- ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +  
    geom_sf(data = outline_sf, fill = NA, color = "black") +
    geom_sf(
      data = ellipses_long,
      aes(fill = layer, color = layer),
      alpha = alpha,
      show.legend = FALSE
    ) +
    geom_sf(data = reg_data %>% filter(year == !!year), 
            aes(fill = as.character(flag_twentiethmile_any_hp_open)), alpha = alpha_h*0.3) + 
    geom_sf(data = projects_filtered, 
            aes(fill = is_new),
            color = NA, 
            linewidth = 0.0001, 
            alpha = alpha_h,
            show.legend = FALSE)  +
    
    theme_void() +
    
    labs(title = glue("{year}"), fill = NULL, color = NULL) +
    
    theme(
      plot.title  = element_text(size = 24),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
      
    ) +
    
    coord_sf(expand = TRUE)

  
  if (!is.null(palette)) {
    
    p <- p +
      scale_fill_manual(values = palette, breaks = which) +
      scale_color_manual(values = palette, breaks = which)
    
  }
  
  
  filename_hw <- ""
  
  if (hway & year >= 1949) {
    
    p <- p +
      geom_sf(data = highways, linewidth = 1, color = "orange" ) +
      geom_sf(data = reg_data %>% filter(year == !!year), 
              aes(fill = as.character(flag_tenthmile_hway)), alpha = 0.1) 
    
    filename_hw <- "_hw"
    
  }
  
  ggsave(glue("output/westside_paper/ellipses/ellipse_{tolower(paste(which, collapse = '_'))}_{year}{filename_h}{filename_hw}.png"), p)
  
  p

}


ellipses_list <- list(
  # "Share of Black Population" = ellipses_n_black_share,
  # "Share of Households w/ > 1 PPR"   = ellipses_n_ppr_share,
  # "Share Age 20–29" = ellipses_n_20_29_share,
  # "Share Age 15-24" = ellipses_n_15_24_share,
  # "Share of Males Aged 20–29" = ellipses_n_male_20_29_share,
  # "Total Black Population" = ellipses_n_black_total,
  # "Total Households w/ > 1 PPR"   = ellipses_n_ppr_total,
  # "Total Age 20–29" = ellipses_n_20_29_total,
  # "Total Males Aged 20–29" = ellipses_n_male_20_29_total,
  # "Total Age 15-24" = ellipses_n_15_24_total, 
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
  "Homicides" = "red", 
  "FALSE" = "black",
  "TRUE" = "green",
  "1" = "purple", 
  "0" = NA
)


#Housing & Highway
p1 <- plot_ellipses_map(
  year = 1940,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  housing = T,
  hway = F,
  which = c("Homicides"),   
  palette = palette
)


p3 <- plot_ellipses_map(
  year = 1950,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Homicides"),  
  housing = T,
  hway = F,
  palette = palette
)


p5 <- plot_ellipses_map(
  year = 1960,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Homicides"),   
  housing = T,
  hway = F,
  palette = palette
)


p7 <- plot_ellipses_map(
  year = 1970,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Homicides"),  
  housing = T,
  hway = F, 
  palette = palette
)

combined <- (p1 + p3) / (p5 + p7) +
  plot_layout(guides = "collect")

ggsave("output/westside_paper/ellipses/housing_panel.png", combined, 
       width = 18, height = 18, dpi = 300)


#No Housing 

p1 <- plot_ellipses_map(
  year = 1940,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  housing = F,
  hway = T, 
  which = c("Homicides"),   
  palette = palette
)


p3 <- plot_ellipses_map(
  year = 1950,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Homicides"),  
  housing = F,
  hway = T, 
  palette = palette
)



p5 <- plot_ellipses_map(
  year = 1960,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Homicides"),   
  housing = F,
  hway = T, 
  palette = palette
)


p7 <- plot_ellipses_map(
  year = 1970,
  outline_sf = census_west_outline,
  ellipses_list = ellipses_list,
  which = c("Homicides"),  
  housing = F,
  hway = T, 
  palette = palette
)

combined <- (p1 + p3) / (p5 + p7) +
  plot_layout(guides = "collect")

ggsave("output/westside_paper/ellipses/hway_panel.png", combined, 
       width = 18, height = 18, dpi = 300)

