rm(list = ls())
source("header.R")

# Data Import ---------------------

df <- st_read("data/mst/hex_merged.gpkg")


# HOMICIDE STATS ---------------------------

hexhomshp_10 <- df %>% 
  st_drop_geometry() %>% 
  dplyr::select(GRID_ID, hom_ct, hom_rt, year) %>% 
  mutate(year_10 = ceiling((year+5)/10)*10-5) %>% 
  group_by(GRID_ID, year_10) %>% 
  mutate(across(c(hom_ct, hom_rt), .fns = list(sum_10 = ~sum(.), 
                                                          mean_10 = ~mean(.), 
                                                          min_10 = ~min(.), 
                                                          max_10 = ~max(.), 
                                                          median_10 = ~median(.)))) %>% 
  ungroup() %>% 
  dplyr::select( -hom_ct,  -hom_rt) 

#hexhomshp_10 %>% filter(GRID_ID == "AK-22" & year_10 == 1945) %>% View()

df_out <- left_join(df, hexhomshp_10, by = c("GRID_ID", "year"))

sf_object <- st_as_sf(df_out, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_merged_w_stats.gpkg", layer = "hex_merged_w_stats", append = FALSE)
