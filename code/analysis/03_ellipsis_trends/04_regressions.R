rm(list = ls())
source("header.R")
library(lubridate)
library(htmltools)
library(shiny)
library(rsconnect)
library(leaflet)
library(modelsummary)
library(fixest)

all_ellipses <- st_read("data/mst/all_ellipses.shp")
census_west_homs_geom <- readRDS("output/westside_interactive_maps/census_west_homs_geom.rds") %>% 
  filter(year <= 1970) %>% 
  mutate(ellipsis_in_GISJOIN= lengths(st_intersects(., all_ellipses)) > 0)



# Regresions ---------------------

reg1 <- feglm(homicides ~ share_black*share_over_1_ppr*share_under_25 
           
           + vacant_homes + unemployment_rate + college_grads + median_value |  GISJOIN_1970 + year
           ,family = poisson(), data = census_west_homs_geom, offset = ~log(total_pop)) 
summary(reg1)

reg2 <- feglm(homicides ~ share_black*share_over_1_ppr*share_under_25*share_foreign
            + vacant_homes + unemployment_rate + college_grads + median_value   |  GISJOIN_1970 + year
            ,family = poisson(), data = census_west_homs_geom, offset = ~log(total_pop))
summary(reg2)

reg3 <- feglm(homicides ~  share_black*share_over_1_ppr*share_under_25 
              + vacant_homes + unemployment_rate + college_grads + median_value | year
              ,family = poisson(), data = census_west_homs_geom, 
              vcov = ~ GISJOIN_1970, offset = ~log(total_pop))
summary(reg3)

reg4 <- feglm(homicides ~  share_black*share_over_1_ppr*share_under_25*share_foreign
              + vacant_homes + unemployment_rate + college_grads + median_value  | year
              ,family = poisson(), data = census_west_homs_geom, 
              vcov = ~ GISJOIN_1970, offset = ~log(total_pop))
summary(reg4)

reg5 <- feglm(homicides ~ share_black:share_over_1_ppr:share_under_25:share_foreign
              + vacant_homes + unemployment_rate + college_grads + median_value  | year
              ,family = poisson(), data = census_west_homs_geom, 
              vcov = ~ GISJOIN_1970, offset = ~log(total_pop))
summary(reg5)

modelsummary(list("Base" = reg1,
                  "Base + Foreign Interaction" = reg2, 
                  "No Tract Fixed Effects" = reg3, 
                  "No Tract Fixed Effects + Foreign Interaction" = reg4, 
                  "No Tract Fixed Effects + Foreign Interaction - Simpl." = reg5), stars = TRUE)


reg1 <- feglm(homicides ~ share_black + share_over_1_ppr + share_under_25 + share_foreign
              + vacant_homes + unemployment_rate + college_grads + median_value  | year
              ,family = poisson(), data = census_west_homs_geom, 
              vcov = ~ GISJOIN_1970, offset = ~log(total_pop))
summary(reg1)

reg2 <- feglm(homicides ~ share_black:share_over_1_ppr:share_under_25:share_foreign
              + vacant_homes + unemployment_rate + college_grads + median_value  | year
              ,family = poisson(), data = census_west_homs_geom, 
              vcov = ~ GISJOIN_1970, offset = ~log(total_pop))
summary(reg2)

reg3 <- feglm(homicides ~ share_black:share_over_1_ppr:share_under_25
              + vacant_homes + unemployment_rate + college_grads + median_value  | year
              ,family = poisson(), data = census_west_homs_geom, 
              vcov = ~ GISJOIN_1970, offset = ~log(total_pop))
summary(reg3)

modelsummary(list("No Interactions" = reg1,
                  "All Interactions Simpl" = reg2, 
                  "No Foreign Born" = reg3), stars = TRUE)


reg1 <- feglm(homicides ~  share_black*share_over_1_ppr*share_under_25*share_foreign  | year
              ,family = poisson(), data = census_west_homs_geom, vcov = ~ GISJOIN_1970) 
summary(reg1)

modelsummary(list("Base" = reg1), stars = TRUE)


# Plots -----------------------------------

years  <-  c(1940, 1950, 1960)
plots1 <- list()
plots2 <- list()
plots3 <- list() 
plots4 <- list() 

for (i in 1:3) {
  
  year <- years[i]
  
  p1 <- ggplot() + 
    geom_sf(data = census_west_homs_geom %>% filter(year == !!year), aes(fill = share_black)) + 
    geom_sf(data = all_ellipses %>% filter(year == !!year), alpha = 0.01, linewidth= 2, color = "red") + 
    theme_void() + 
    scale_fill_gradient( low = "black", high = "white", guide = guide_colorbar(barheight = 1)) + 
    labs(fill = "Share Black  ", title = year)+
    theme(legend.position = "bottom",
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5))
  
  img <- image_graph(width = 1200, height = 800, res = 300)
  print(p1)
  dev.off()
  plots1[[i]] <- img
  
  p2 <- ggplot() + 
    geom_sf(data = census_west_homs_geom %>% filter(year == !!year), aes(fill = share_over_1_ppr)) + 
    geom_sf(data = all_ellipses %>% filter(year == !!year), alpha = 0.01, linewidth= 2, color = "red") + 
    theme_void() + 
    scale_fill_gradient( low = "black", high = "white", guide = guide_colorbar(barheight = 1)) + 
    labs(fill = "Share > 1 PPR  ", title = year)+
    theme(legend.position = "bottom",
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5))
  
  img <- image_graph(width = 1200, height = 800, res = 300)
  print(p2)
  dev.off()
  plots2[[i]] <- img
  
  p3 <- ggplot() + 
    geom_sf(data = census_west_homs_geom %>% filter(year == !!year), aes(fill = share_under_25)) + 
    geom_sf(data = all_ellipses %>% filter(year == !!year), alpha = 0.01, linewidth= 2, color = "red") + 
    theme_void() + 
    scale_fill_gradient( low = "black", high = "white", guide = guide_colorbar(barheight = 1)) + 
    labs(fill = "Share < 25  ", title = year)+
    theme(legend.position = "bottom",
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5))
  
  img <- image_graph(width = 1200, height = 800, res = 300)
  print(p3)
  dev.off()
  plots3[[i]] <- img
  
  
  p4 <- ggplot() + 
    geom_sf(data = census_west_homs_geom %>% filter(year == !!year), aes(fill = share_black*share_over_1_ppr*share_under_25)) + 
    geom_sf(data = all_ellipses %>% filter(year == !!year), alpha = 0.01, linewidth= 2, color = "red") + 
    theme_void() + 
    scale_fill_gradient( low = "black", high = "white", guide = guide_colorbar(barheight = 1)) + 
    labs(fill = "Interaction  ", title = year)+
    theme(legend.position = "bottom",
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5))
  
  img <- image_graph(width = 1200, height = 800, res = 300)
  print(p4)
  dev.off()
  plots4[[i]] <- img
  
  
}


row1 <- image_append(c(plots1[[1]], plots2[[1]], plots3[[1]], plots4[[1]]))
row2 <- image_append(c(plots1[[2]], plots2[[2]], plots3[[2]], plots4[[2]]))
row3 <- image_append(c(plots1[[3]], plots2[[3]], plots3[[3]], plots4[[3]]))

panel <- image_append(c(row1, row2, row3), stack = TRUE)

image_write(panel, path = "output/westside/regressions/homicides_v_crowding_v_young_panel.png", format = "png", density = "300")
