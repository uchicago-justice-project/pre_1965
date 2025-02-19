rm(list = ls())
source("header.R")

# Data Import ---------------------

hex <- st_read("data/intermediate/homicide_hexagons/survivorship")

cha_projects <- read_sf("data/intermediate/housing_projects/cha_projects.geojson")

riots <- read_sf("data/intermediate/racial_violence/riots.geojson")

landsales <- read_sf("data/intermediate/landsales/sections_with_checkerboard.geojson")

redlines <- read_sf("data/intermediate/redlining/mappinginequality.json")

for (i in c("1", "2", "3", "4_1", "4_2", "4_3", "5_1", "5_2", "5_3")){
  df <- read_sf(glue("data/raw/black_residency/part{i}"))
  
  st_transform(df, st_crs(hex))
  assign(glue("black_res_{i}"), df)
  
}

# Standardize CRS -----------------------
cha_projects <- st_transform(cha_projects, st_crs(hex))
riots <- st_transform(riots, st_crs(hex))
landsales <- st_transform(landsales, st_crs(hex))
redlines <- st_transform(redlines, st_crs(redlines))

#Join Data -------------------------



##Housing Projects --------------------
hexhomshp_merged <- st_join(hex, cha_projects %>% 
                              rename_all(~paste0(., "_prjct"))
                            , join = st_contains, left = TRUE)  %>% 
  mutate(across(name_prjct:id_prjct, ~ ifelse((year >= yr_built_start_prjct & year <= yr_built_end_prjct ) | 
                                                (year == yr_built_start_prjct & is.na(yr_built_end_prjct)) |
                                              (year >= yr_dmlsh_or_cnvrt_start_prjct   & year <= yr_dmlsh_or_cnvrt_end_prjct) | 
                                                (year == yr_dmlsh_or_cnvrt_start_prjct  & is.na(yr_dmlsh_or_cnvrt_end_prjct)) | 
                                                (is.na(yr_dmlsh_or_cnvrt_start_prjct) & year == yr_dmlsh_or_cnvrt_end_prjct), ., NA)))



# hexhomshp_merged %>% filter(!is.na(name_prjct)) %>% View()

## Riots ---------------------------
hexhomshp_merged <- st_join(hexhomshp_merged, riots %>% 
                              rename_all(~paste0(., "_riot")), join = st_contains, left = TRUE) %>% 
  mutate(across(id_riot:name_riot, ~ ifelse((year == year_riot), ., NA)))

# hexhomshp_merged %>% filter(!is.na(name_riot)) %>% View()


## Landsales -----------------------------------
landsales_mod <- landsales %>% 
  dplyr::select(geometry, in_checkerboard)

hexhomshp_merged <- hexhomshp_merged %>% 
  mutate(id_merge = row_number())

hexhomshp_merged_land <- st_join(hexhomshp_merged, landsales_mod, join = st_intersects, left = TRUE) 

hexhomshp_merged_land <-hexhomshp_merged_land %>%
  st_drop_geometry() %>% 
  distinct(id_merge, in_checkerboard) %>% 
  mutate(in_checkerboard = ifelse(is.na(in_checkerboard), FALSE ,in_checkerboard ))

hexhomshp_merged <- left_join(hexhomshp_merged, hexhomshp_merged_land) 


## Redlines -------------------------------------------------

## ASK ABOUT THUS
sf_use_s2(FALSE)

intersect_redlines <- st_intersection(hexhomshp_merged, redlines) 

intersect_redlines <- intersect_redlines %>%
  mutate(intersect_area = st_area(.))

intersect_redlines <- intersect_redlines %>% 
  st_drop_geometry() %>% 
  distinct(id_merge, category, intersect_area) %>% 
  mutate(category = str_replace_all(category, " ", "_")) %>% 
  # group_by(id_merge, category) %>% 
  # summarise(intersect_area = sum(intersect_area)) %>% 
  # ungroup() %>% 
  pivot_wider(id_cols = id_merge, names_from = category, values_from = intersect_area) 

intersect_redlines <- intersect_redlines %>% 
  mutate(Best = map_dbl(Best, ~ sum(unlist(.x))), 
         Still_Desirable = map_dbl(Still_Desirable, ~ sum(unlist(.x))), 
         Definitely_Declining = map_dbl(Definitely_Declining, ~ sum(unlist(.x))),
         Hazardous = map_dbl(Hazardous, ~ sum(unlist(.x))),
         Industrial = map_dbl(Industrial, ~ sum(unlist(.x))),
         Commercial = map_dbl(Commercial, ~ sum(unlist(.x))))


sf_use_s2(TRUE )

hexhomshp_merged <- left_join(hexhomshp_merged, intersect_redlines, by= c("id_merge")) 

hexhomshp_merged <- hexhomshp_merged %>% 
  mutate(polygon_area = st_area(.)) %>% 
  mutate(across(Best:Commercial, ~round(./as.numeric(polygon_area), 2))) %>% 
  mutate(sum_check=  Best + Still_Desirable + Definitely_Declining + Hazardous + Industrial + Commercial)


#Plots ---------

ggplot(data = test %>% filter(year == 1874)) +
  geom_sf(aes(fill = sum_check)) +
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
