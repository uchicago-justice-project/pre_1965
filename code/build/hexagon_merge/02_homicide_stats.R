rm(list = ls())
source("header.R")

# Data Import ---------------------

df <- st_read("data/mst/hex_merged.gpkg")


# HOMICIDE STATS ---------------------------

hexhomshp_5 <-  df %>% 
  st_drop_geometry() %>% 
  dplyr::select(GRID_ID, hom_ct, hom_rt, year) %>% 
  mutate(year_5 = 5 * ceiling(year / 5)) %>% 
  group_by(GRID_ID, year_5) %>% 
  mutate(across(c(hom_ct, hom_rt), .fns = list(sum_5 = ~sum(.), 
                                               mean_5 = ~mean(.), 
                                               min_5 = ~min(.), 
                                               max_5 = ~max(.), 
                                               median_5 = ~median(.)))) %>% 
  ungroup() %>% 
  dplyr::select( -hom_ct,  -hom_rt) 

#hexhomshp_10 %>% filter(GRID_ID == "AK-22" & year_10 == 1945) %>% View()

df_out <- left_join(df, hexhomshp_5, by = c("GRID_ID", "year"))

sf_object <- st_as_sf(df_out, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_merged_w_stats.gpkg", layer = "hex_merged_w_stats", append = FALSE)
