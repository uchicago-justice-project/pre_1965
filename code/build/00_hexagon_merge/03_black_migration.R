rm(list = ls())
source("header.R")

df <- st_read("data/mst/hex_merged_w_stats.gpkg")

df_black_res <- df %>% 
  st_drop_geometry() %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID, contains("bk_res")) %>% 
  #estimate black residency in 1938 and 1948 based on migration info
  #conservative estimate of black res: if there could be any black res given any of the mappings, we flag it as black res
  mutate(est_black_res_39 = pmax(bk_res_1, bk_res_2, bk_res_3) - (bk_res_4_1 + bk_res_4_2 + bk_res_4_3), 
         est_black_res_39 = ifelse(est_black_res_39 <=  0, 0, est_black_res_39)) %>% 
  mutate(est_black_res_48 = abs(pmax(bk_res_1, bk_res_2, bk_res_3) - (bk_res_5_1 + bk_res_5_2 + bk_res_5_3)), 
         est_black_res_48 = ifelse(est_black_res_48 <=0, 0, est_black_res_48)) %>% 
  dplyr::select(GRID_ID, est_black_res_39, est_black_res_48)

df_merge <- df %>% 
  left_join(df_black_res, by = "GRID_ID") %>% 
  #flag for any black migration 
  mutate(bk_res_4 = bk_res_4_1 + bk_res_4_2 + bk_res_4_3, 
         bk_res_5 = bk_res_5_1 + bk_res_5_2 + bk_res_5_3) %>% 
  #MIGRATION SINCE 1939
  #create category for degree of migration 
  #rank in priority if competing: 1) significant increase 2) any increase to more than 90% 3) any increase from less than 1%
  mutate(bk_res_4_cat = case_when(bk_res_4_2 > 0 ~ "Significant Increase From 1% or More to Less than 90%", 
                                  bk_res_4_3 > 0 ~ "Any Increase From 1% or More to More than 90%", 
                                  bk_res_4_1 > 0 ~ "Any Increase From Less Than 1%", 
                                  TRUE ~ "No Increase")) %>% 
  #create category for black migration since 1939
  mutate(bk_res_since_39 = case_when((bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_39 > 0 & bk_res_4 == 0 ~ 
                                       "Black Residency Likely Since 1939 & No Black Migration", 
                                     (bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_39 > 0 & bk_res_4 > 0 ~ 
                                       "Black Residency Likely Since 1939 & Black Migration",
                                     (bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_39 == 0 & bk_res_4 > 0 ~ 
                                       "Likely No Black Residency in 1939 & Black Migration",
                                     TRUE ~ "No Black Residency in 1950")) %>% 
  #MIGRATION SINCE 1948
  mutate(bk_res_5_cat = case_when(bk_res_5_2 > 0 ~ "Significant Increase From 1% or More to Less than 90%", 
                                  bk_res_5_3 > 0 ~ "Any Increase From 1% or More to More than 90%", 
                                  bk_res_5_1 > 0 ~ "Any Increase From Less Than 1%", 
                                  TRUE ~ "No Increase")) %>% 
  mutate(bk_res_since_48 = case_when((bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_48 > 0 & bk_res_5 == 0 ~ 
                                       "Black Residency Likely Since 1948 & No Black Migration", 
                                     (bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_48 > 0 & bk_res_5 > 0 ~ 
                                       "Black Residency Likely Since 1948 & Black Migration",
                                     (bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_48 == 0 & bk_res_5 > 0 ~ 
                                       "Likely No Black Residency in 1948 & Black Migration",
                                     TRUE ~ "No Black Residency in 1950")) 


sf_object <- st_as_sf(df_merge, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_merged_w_black_migr.gpkg", layer = "hex_merged_w_black_migr", append = FALSE)



## Testing 

df_merge %>% 
  filter(bk_res_4_1 > 0 & bk_res_4_cat == "Any Increase From 1% or More to More than 90%") %>% 
  dplyr::select(bk_res_4_3, bk_res_4_2, bk_res_4_1) %>% 
  View()




test <- df_merge %>%
  filter(year == 1950) %>% 
  filter(bk_res_3 < bk_res_2 | bk_res_2 < bk_res_1 | bk_res_3 < bk_res_1) %>%
  dplyr::select(GRID_ID, bk_res_1, bk_res_2, bk_res_3) %>% 
  mutate(diff_1_2 = bk_res_1 - bk_res_2, 
         diff_1_3 = bk_res_1 - bk_res_3, 
         diff_2_3 = bk_res_2 - bk_res_3) %>% 
  pivot_longer(cols = contains("diff")) %>% 
  filter(value > 0.1) 


ggplot(data = test ) + 
  geom_sf(aes(fill = GRID_ID)) + 
  coord_sf(
    xlim = c(-87.94011, -87.52398), # Approximate longitude bounds of Chicago
    ylim = c(41.64454, 42.02304)    # Approximate latitude bounds of Chicago
  ) 
