rm(list = ls())
source("header.R")

df <- st_read("data/mst/hex_merged_w_black_migr.gpkg")

df_filt <- df %>% 
  filter(bk_res_since_39 != "No Black Residency in 1950" | bk_res_since_48 != "No Black Residency in 1950" ) %>% 
  dplyr::select(-contains("polygon_area"))

sf_object <- st_as_sf(df_filt, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_black_migr.gpkg", layer = "hex_black_migr", append = FALSE)

df_filt_neat <- df_filt %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID, contains("res_since"), contains("cat"))

sf_object <- st_as_sf(df_filt_neat, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_black_migr_neat.gpkg", layer = "hex_black_migr_neat", append = FALSE)



## Testing --------------------------------------------------------------------------


#if 1939 cat shows black res, 1948 agrees
df %>% 
  filter(bk_res_since_39 == "Black Residency Likely Since 1939 & No Black Migration" &  (bk_res_since_48 == "No Black Residency in 1950" | 
                                                                                           bk_res_since_48 == "Likely No Black Residency in 1948 & Black Migration" ))

df %>% 
  filter(bk_res_since_39 == "Black Residency Likely Since 1939 & Black Migration" & (bk_res_since_48 == "No Black Residency in 1950" | 
                                                                                       bk_res_since_48 == "Likely No Black Residency in 1948 & Black Migration" ) )

df %>% 
  filter(bk_res_since_39 == "Likely No Black Residency in 1939 & Black Migration" & (bk_res_since_48 == "No Black Residency in 1950" | 
                                                                                       bk_res_since_48 == "Likely No Black Residency in 1948 & Black Migration" ) )


df %>% 
  filter(est_black_res_39 == 0 & est_black_res_48 != 0) %>% 
  dplyr::select(contains("res")) %>% 
  View()

df %>% 
  filter(bk_res_since_39 == "Likely No Black Residency in 1939 & Black Migration" & 
           bk_res_since_48 == "No Black Residency in 1950" )


#There is no instance where there is no estimated black res in 1939, no black migration since 1939 but black res in 1950
#i.e. if there is black migration in 1948, there must have been black migration or black residency since 1939
df %>% 
  filter( (bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) & est_black_res_39 == 0 & bk_res_4 == 0)


#There are instances where there is estimated black res in 1939, no black migration since 1939, but black migration in 1948
df %>% 
  filter( (bk_res_1 > 0 | bk_res_2 > 0 | bk_res_3 > 0) &  bk_res_4 == 0 &  bk_res_5 != 0)%>% 
  dplyr::select(contains("res")) %>% 
  View()
