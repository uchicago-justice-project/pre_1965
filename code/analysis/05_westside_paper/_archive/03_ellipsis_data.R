
rm(list = ls())
source("header.R")

# Make ellipses data frames

point_var <- "homs"

write_ellipses <- function(point_var) {
  
  geojson_files <- list.files("output/westside_paper/ellipses/map_images_1940_65/", pattern = glue("ellipse.*{point_var}.*\\.shp$"), full.names = TRUE)
  geojson_files <- geojson_files %>%
    lapply(read_sf) %>%      
    bind_rows()    
  
  all_ellipses <- geojson_files %>%
    sf::st_cast("POLYGON") %>% 
    mutate(size = as.numeric(sf::st_area(geometry))) 
  
  st_write(all_ellipses, glue("data/mst/for_paper/{point_var}_ellipses_all_years.geojson"), append = FALSE)
  
  
}

write_ellipses("n_black_share")
write_ellipses("n_pop_share")
write_ellipses("n_renter_occ_share")
write_ellipses("n_ppr_share")
write_ellipses("n_20_29_share")
write_ellipses("n_male_20_29_share")
write_ellipses("n_black_total")
write_ellipses("n_ppr_total")
write_ellipses("n_pop_total")
write_ellipses("n_renter_occ_total")
write_ellipses("n_20_29_total")
write_ellipses("n_male_20_29_total")
write_ellipses("homs")
write_ellipses("n_15_24_total")
write_ellipses("n_15_24_share")
write_ellipses("n_15_19_total")
write_ellipses("n_15_19_share")
write_ellipses("n_male_15_19_total")
write_ellipses("n_male_15_19_share")
