
rm(list = ls())
source("header.R")
library(spatstat.geom)
library(MASS)
library(stars)

# Read data -----------

census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson")

homs_west <- st_read("data/mst/for_paper/homs_west.geojson")

ellipses_homs <- st_read(glue("data/mst/for_paper/homs_ellipses_all_years.geojson"))


point_name <- "n_ppr_share"
year <- 1940
points <- st_read(glue("output/westside_paper/ellipses/map_images_1940_65/points_{point_name}_{year}.shp"))


set.seed(1)
points_thin <- points |> dplyr::slice_sample(prop = 0.05)


p <- ggplot() +
  geom_sf(data = census_west_outline, fill = NA, color = "black") +
  geom_sf(data = points_thin , size = 0.001) + 
  theme_void() +
  labs(title = glue("{year}"), fill = NULL, color = NULL) +
  theme(
    plot.title  = element_text(size = 24),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "right"
  ) +
  coord_sf(expand = TRUE)

p


outline_m <- st_transform(census_west_outline, 26916)  # UTM 16N
points_m  <- st_transform(points, 26916)

# Extract coordinates for ggplot
pts_df <- cbind(
  st_coordinates(points_m),
  st_drop_geometry(points_m)
)

ggplot() +
  geom_sf(data = outline_m, fill = NA, color = "black", linewidth = 0.25) +
  geom_sf(data = census_west_outline, fill = NA, color = "black") +
  stat_density_2d(
    data = pts_df,
    aes(x = X, y = Y, fill = after_stat(density)),
    geom = "raster",
    contour = FALSE,
    adjust = 1.2        # smoothing parameter; try 0.8–2.0
  ) +
  geom_sf(data = ellipses_homs %>% filter(year == !!year), color = "red", alpha = 0.2) +
  scale_fill_viridis_c(trans = "sqrt", name = "Density") +
  coord_sf(crs = st_crs(outline_m), expand = FALSE) +
  theme_void() +
  labs(title = as.character(year)) +
  theme(
    plot.title = element_text(size = 24),
    legend.position = "right"
  )
