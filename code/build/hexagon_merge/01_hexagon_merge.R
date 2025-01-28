rm(list = ls())
source("header.R")

# Data Import ---------------------

hex <- st_read("data/intermediate/homicide_hexagons/survivorship")

cha_projects <- read_sf("data/intermediate/housing_projects/cha_projects.geojson")

riots <- read_sf("data/intermediate/racial_violence/riots.geojson")

cha_projects <- st_transform(cha_projects, st_crs(hex))
riots <- st_transform(riots, st_crs(hex))


hexhomshp_joined <- st_join(hex, cha_projects, join = st_within)
