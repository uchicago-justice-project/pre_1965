library(sf)
library(dplyr)

# Load your westside polygon and Chicago boundary (ensure same CRS)

hex <- st_read("data/intermediate/homicide_hexagons/survivorship")%>% 
  mutate(year = as.integer(year))

westside <- read_sf("data/intermediate/westside.geojson")
westside <- st_transform(westside, st_crs(hex))

chicago <- read_sf("https://data.cityofchicago.org/api/geospatial/qqq8-j68g?method=export&format=GeoJSON") %>% 
  st_transform(st_crs(westside))

# Create western extension area
extension_box <- st_bbox(c(
  st_bbox(chicago)$xmin,
  st_bbox(westside)$ymin,
  st_bbox(westside)$xmax,
  st_bbox(westside)$ymax
), crs = st_crs(westside)) %>% 
  st_as_sfc()

# Clip extension area to city boundaries and combine
west_extension <- st_intersection(extension_box, chicago)
westside_extended <- st_union(westside, west_extension) %>% 
  st_make_valid()


ggplot() +
  geom_sf(data = westside_extended, fill  = "green") + 
  geom_sf(data = chicago, alpha  = 0.2 ) + 
  geom_sf(data = westside, fill = "red", alpha = 0.3)
# Save result
st_write(westside_extended, "data/intermediate/westside_extended.geojson")
