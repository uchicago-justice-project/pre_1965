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

#Housing Projects 
hexhomshp_merged <- st_join(hex, cha_projects, join = st_contains, left = TRUE)  %>% 
  mutate(across(name:id, ~ ifelse((year >= yr_built_start & year <= yr_built_end) | 
           (year >= yr_dmlsh_or_cnvrt_start & year <= yr_dmlsh_or_cnvrt_end), ., NA)))


#Riots
hexhomshp_merged <- st_join(hexhomshp_merged, riots, join = st_contains, left = TRUE, suffix =c("", "_riot")) %>% 
  mutate(across(id_riot:year_riot, ~ ifelse((year == year_riot), ., NA)))

#Landsales 

hexhomshp_merged_land <- st_join(hexhomshp_merged, landsales, join = st_intersects, left = TRUE)


#Plots ---------

ggplot(data = landsales %>% filter(in_checkerboard == TRUE) %>% head(30)) +
  geom_sf(aes(fill = in_checkerboard)) +
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
