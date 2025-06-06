rm(list = ls())
source("header.R")
library(geosphere)
library(lubridate)
library(tidygeocoder)
library(mapview)
library(mapedit)

homs <- st_read("data/raw/homicides_1940-1965_geocoded_v3") %>% 
  mutate(year = as.integer(year)) %>%
  mutate(category_clean = case_when(str_detect(tolower(cod), "police") ~ "Police Killing",
                                    str_detect(tolower(cod), "gun") |  str_detect(tolower(cod), "shot") 
                                    |  str_detect(tolower(cod), "bullet") ~ "Gunshot", 
                                    str_detect(tolower(cod), "stab") | str_detect(tolower(cod), "cut") ~ "Knife", 
                                    TRUE ~ "Other"), 
         date_clean = as_date(date), 
         category = paste0(tolower(cod), ": ", format(date_clean, "%m/%d"))) %>% 
  filter(!st_is_empty(geometry)) %>% 
  filter(!str_detect(category, "burn"))%>% 
  filter(!str_detect(category, "overdose"))%>% 
  filter(!str_detect(category, "abortion"))%>% 
  filter(!str_detect(category, "fire"))

westside <- read_sf("data/intermediate/westside.geojson")
westside <- st_transform(westside, st_crs(homs))

map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(data = westside,  opacity = 0)

draw <- drawFeatures(map = map)
print(draw)
st_write(draw, dsn = "data/mst/neighborhoods/sc2.shp",append = FALSE)

#mapshot(map, file = "data/mst/neighborhoods/westside_map.png")

#Harrison - Western Ave - Cermak - Halsted
# 
# harrison_cermak <- drawFeatures(map = map)
# 
# # Inspect it
# print(harrison_cermak)
# 
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addPolygons(data = westside,  opacity = 0) %>% 
#   addPolygons(data = harrison_cermak,  opacity = 0.1)
# 
# st_write(harrison_cermak, dsn = "data/mst/neighborhoods/harrison_cermak.shp",append = FALSE)
# 
# #North west corner
# 
# nortwest <- drawFeatures(map = map)
# 
# # Inspect it
# print(nortwest)
# 
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addPolygons(data = westside,  opacity = 0) %>% 
#   addPolygons(data = nortwest,  opacity = 0.1)
# 
# st_write(nortwest, dsn = "data/mst/neighborhoods/nortwest.shp",append = FALSE)
# 
# 
# #South West Eisenhower
# 
# southwest_roos <- drawFeatures(map = map)
# 
# # Inspect it
# print(southwest_roos)
# 
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addPolygons(data = westside,  opacity = 0) %>% 
#   addPolygons(data = southwest_roos,  opacity = 0.1)
# 
# st_write(southwest_roos, dsn = "data/mst/neighborhoods/southwest_roos.shp",append = FALSE)
# 
# #North of Eisenhower
# north_middle_eis <- drawFeatures(map = map)
# # Inspect it
# print(north_middle_eis)
# 
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addPolygons(data = westside,  opacity = 0) %>% 
#   addPolygons(data = north_middle_eis,  opacity = 0.1)
# 
# st_write(north_middle_eis, dsn = "data/mst/neighborhoods/north_middle_eis.shp",append = FALSE)
# 
# #Lower South West 
# 
# lower_south_west <- drawFeatures(map = map)
# # Inspect it
# print(lower_south_west)
# 
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addPolygons(data = westside,  opacity = 0) %>% 
#   addPolygons(data = lower_south_west,  opacity = 0.1)
# 
# st_write(lower_south_west, dsn = "data/mst/neighborhoods/lower_south_west.shp",append = FALSE)
# 
# #Upper North East
# 
# upper_north_east <- drawFeatures(map = map)
# # Inspect it
# print(upper_north_east)
# 
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addPolygons(data = westside,  opacity = 0) %>% 
#   addPolygons(data = upper_north_east,  opacity = 0.1)
# 
# st_write(upper_north_east, dsn = "data/mst/neighborhoods/upper_north_east.shp",append = FALSE)
# 
#   
#   