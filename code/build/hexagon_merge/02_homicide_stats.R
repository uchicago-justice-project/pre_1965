rm(list = ls())
source("header.R")

# Data Import ---------------------

df <- st_read("data/mst/hex_merged.gpkg")


# HOMICIDE STATS ---------------------------

hexhomshp_10 <- df %>% 
  st_drop_geometry() %>% 
  dplyr::select(GRID_ID, hom_ct, hom_rt, bldg_ct, year) %>% 
  mutate(hom_pbldg = hom_ct / bldg_ct) %>% 
  mutate(year_10 = ceiling((year+5)/10)*10-5) %>% 
  group_by(GRID_ID, year_10) %>% 
  mutate(across(c(hom_ct, hom_rt, hom_pbldg), .fns = list(sum_10 = ~sum(.), 
                                                          mean_10 = ~mean(.), 
                                                          min_10 = ~min(.), 
                                                          max_10 = ~max(.), 
                                                          median_10 = ~median(.)))) %>% 
  ungroup() %>% 
  dplyr::select( -hom_ct,  -hom_rt, -bldg_ct, -year) 

df_out <- left_join(df, hexhomshp_10)

sf_object <- st_as_sf(df_out, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_merged_w_stats.gpkg", layer = "hex_merged_w_stats", append = FALSE)
