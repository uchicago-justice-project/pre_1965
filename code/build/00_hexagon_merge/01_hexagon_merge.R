rm(list = ls())
source("header.R")

# Data Import ---------------------

hex <- st_read("data/intermediate/homicide_hexagons/survivorship")

cha_projects <- read_sf("data/intermediate/housing_projects/cha_projects.geojson") %>% 
  rename(bldst = yr_built_start, bldend = yr_built_end, dmlst = yr_dmlsh_or_cnvrt_start, dmlend = yr_dmlsh_or_cnvrt_end )

riots <- read_sf("data/intermediate/racial_violence/riots.geojson")

landsales <- read_sf("data/intermediate/landsales/sections_with_checkerboard.geojson") %>% 
  rename(in_check = in_checkerboard)

redlines <- read_sf("data/intermediate/redlining/mappinginequality.json") %>% 
  mutate(category = ifelse(category == "Definitely Declining" , "Def Decl", category )) %>% 
  mutate(category = str_sub(str_replace_all(tolower(category), " ", "_"), 0, 10))



for (i in c("1", "2", "3", "4_1", "4_2", "4_3", "5_1", "5_2", "5_3")){
  df <- read_sf(glue("data/raw/black_residency/part{i}"))
  
  df <- st_transform(df, st_crs(hex))
  assign(glue("black_res_{i}"), df)
  
}


# Standardize CRS -----------------------
cha_projects <- st_transform(cha_projects, st_crs(hex))
riots <- st_transform(riots, st_crs(hex))
landsales <- st_transform(landsales, st_crs(hex))
redlines <- st_transform(redlines, st_crs(redlines))

#Join Data -------------------------
#hex %>% st_drop_geometry() %>% group_by(GRID_ID) %>% summarise(n = n()) %>% View()

grids <- hex %>% filter(year == 1876) %>% dplyr::select(GRID_ID) %>%
  mutate(polygon_area = st_area(.))

##Housing Projects --------------------
grid_merged <- st_join(grids, cha_projects %>% 
                              rename_all(~paste0(., "_prj"))
                            , join = st_contains, left = TRUE) %>% 
  st_drop_geometry()

hexhomshp_merged <- left_join(hex, grid_merged, by = "GRID_ID") %>%
  mutate(across(name_prj:id_prj, ~ ifelse((year >= bldst_prj & year <= bldend_prj) | 
                                                (year == bldst_prj & is.na(bldend_prj)) |
                                              (year >= dmlst_prj   & year <= dmlend_prj) | 
                                                (year == dmlst_prj  & is.na(dmlend_prj)) | 
                                                (is.na(dmlst_prj) & year == dmlend_prj), ., NA))) 



# hexhomshp_merged %>% filter(!is.na(name_prj)) %>% View()

## Riots ---------------------------
grid_merged <- st_join(grids, riots %>% 
                              rename_all(~paste0(., "_riot")), join = st_contains, left = TRUE) %>% 
  st_drop_geometry()


hexhomshp_merged <- left_join(hexhomshp_merged, grid_merged, by = "GRID_ID") %>% 
  mutate(across(id_riot:name_riot, ~ ifelse((year == year_riot), ., NA)))

# hexhomshp_merged %>% filter(!is.na(name_riot)) %>% View()


## Landsales -----------------------------------
landsales <- landsales %>% 
  dplyr::select(geometry, in_check)

hexhomshp_merged_land <- st_join(grids, landsales, join = st_intersects, left = TRUE) 

hexhomshp_merged_land <-hexhomshp_merged_land %>%
  st_drop_geometry() %>% 
  distinct(GRID_ID, in_check) %>% 
  mutate(in_check = ifelse(is.na(in_check), FALSE ,in_check )) %>% 
  group_by(GRID_ID) %>% 
  summarise(in_check = max(in_check)) %>% 
  ungroup() 

hexhomshp_merged <- left_join(hexhomshp_merged, hexhomshp_merged_land, by = "GRID_ID") 

rm(hexhomshp_merged_land)
rm(landsales)

## Redlines -------------------------------------------------

## ASK ABOUT THUS
sf_use_s2(FALSE)

intersect_redlines <- st_intersection(grids, redlines) 

intersect_redlines <- intersect_redlines %>%
  mutate(intersect_area = st_area(.)) 
  

intersect_redlines <- intersect_redlines %>% 
  st_drop_geometry() %>% 
  mutate(intersect_area = as.numeric(intersect_area / polygon_area)) %>% 
  distinct(GRID_ID, category, intersect_area) %>% 
  group_by(GRID_ID, category) %>%
  summarise(intersect_area = sum(intersect_area)) %>%
  ungroup() %>%
  pivot_wider(id_cols = GRID_ID, names_from = category, values_from = intersect_area)

sf_use_s2(TRUE )

hexhomshp_merged <- left_join(hexhomshp_merged, intersect_redlines, by= c("GRID_ID")) 

rm(redlines)
rm(intersect_redlines)
## Black Residency ---------------------------------

for (num in c("1", "2", "3", "4_1", "4_2", "4_3", "5_1", "5_2", "5_3")){
  
  i = glue("black_res_{num}")
  df <- get(i)
  
  intersect_black_res <- st_intersection(grids, df) %>%
    mutate(black_residency_area = st_area(.))%>% 
    st_drop_geometry() %>%
    mutate(black_residency_prop = as.numeric(black_residency_area)/as.numeric(polygon_area)) %>%
    dplyr::select(GRID_ID, black_residency_prop) %>% 
    group_by(GRID_ID) %>% 
    summarise_all(sum) %>% 
    ungroup() 
  
  intersect_black_res[glue("bk_res_{num}")] <- intersect_black_res$black_residency_prop
  
  intersect_black_res <- intersect_black_res %>% dplyr::select(GRID_ID, contains(glue("bk_res_{num}")))
  hexhomshp_merged <- left_join(hexhomshp_merged, intersect_black_res, by= c("GRID_ID")) 
  
}


# CLEAN UP 

hexhomshp_merged <- hexhomshp_merged %>% 
  mutate(across(def_decl:bk_res_5_3, ~ifelse(is.na(.), 0, .)))

hexhomshp_merged %>% filter(year== 1950) %>% View()

# SAVE ------------------------------

sf_object <- st_as_sf(hexhomshp_merged, wkt = "geometry") 

st_write(sf_object,"data/mst/hex_merged.gpkg", layer = "hex_merged", append = FALSE)
#test <- st_read("data/mst/hex_merged.gpkg")

#Plots ---------

ggplot(data = hexhomshp_merged %>% filter(year == 1874)) +
  geom_sf(aes(fill = black_res_1_prop)) +
  theme_minimal() + 
  coord_sf(
    xlim = c(-87.94011, -87.52398), # Approximate longitude bounds of Chicago
    ylim = c(41.64454, 42.02304)    # Approximate latitude bounds of Chicago
  ) 



colors <- setNames(unique(redlines$fill), unique(redlines$category))
ggplot(data = redlines %>% filter(city == "Chicago")) +
  geom_sf(aes(fill = category)) +
  scale_fill_manual(values = colors)+
  theme_minimal() + 
  coord_sf(
    xlim = c(-87.94011, -87.52398), # Approximate longitude bounds of Chicago
    ylim = c(41.64454, 42.02304)    # Approximate latitude bounds of Chicago
  ) 

ggplot(data = hex) +
  geom_sf() +
  theme_minimal() + 
  coord_sf(
    xlim = c(-87.94011, -87.52398), # Approximate longitude bounds of Chicago
    ylim = c(41.64454, 42.02304)    # Approximate latitude bounds of Chicago
  ) 


# TESTING --------------

test <- hexhomshp_merged %>% 
  filter(!is.na(year_riot))

test2 <- test %>% 
  filter(!is.na(yr_dmlsh_or_cnvrt_start))
#Testing 

test <- hexhomshp_joined %>% 
  filter(!is.na(yr_built_start)) %>% 
  group_by(name, yr_built_start) %>% 
  summarise(n = n_distinct(geometry)) 
