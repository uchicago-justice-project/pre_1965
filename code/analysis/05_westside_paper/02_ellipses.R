
rm(list = ls())
source("header.R")

# Read data -----------
census_west_homs_geom <- st_read("data/mst/for_paper/census_west_homs_geom.geojson")
census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson")

homs_west <- st_read("data/mst/for_paper/homs_west.geojson")

# Census Data Ellipses ----------------

## Make ellipses -------------
point_var <- "n_black_share"
make_ellipse_maps <- function(point_var,
                              years   = c(1940, 1950, 1960, 1970),
                              sf_data = census_west_homs_geom,
                              outline = census_west_outline,
                              out_dir = "output/westside_paper/ellipses/map_images_1940_65") {
  
  # point_var can be "n_black_share", "n_ppr_share", "n_20_29_share" or bare name
  point_sym  <- rlang::ensym(point_var)
  point_name <- rlang::as_string(point_sym)
  
  # filter tracts with positive counts for this variable
  tracts_with_points <- sf_data %>%
    filter(!!point_sym > 0)
  
  
  # loop over years
  for (year in years) {
    
    df_year <- tracts_with_points %>% filter(year == !!year)
    
    # generate random points per tract
    pts_list <- map2(
      df_year$geometry,
      df_year[[point_name]],
      ~ st_sample(.x, size = .y, type = "random")
    )
    
    # flatten list of sfc objects into one sfc
    pts_sfc <- do.call(c, pts_list)
    
    # index to replicate attributes
    idx <- rep(seq_len(nrow(df_year)),
               times = df_year[[point_name]])
    
    # create sf POINT object
    points <- st_sf(
      df_year %>% st_drop_geometry() %>% slice(idx),
      geometry = pts_sfc
    ) %>% 
      transmute(ID = GISJOIN_1970, yr = year, geometry)
    
    st_crs(points) <- st_crs(df_year)
    
    # standard deviation ellipse
    sde_data <- std_dev_ellipse(points)
    sde <- st_ellipse(
      geometry = sde_data,
      sx       = sde_data$sx,
      sy       = sde_data$sy,
      rotation = -sde_data$theta
    )
    
    sde_polygon <- st_cast(sde, "POLYGON")
    
    sde_sf <- st_sf(
      geometry = sde_polygon
    ) %>%
      mutate(year = !!year)
    
    # map
    map <- ggplot() +
      geom_sf(data = outline, fill = NA, color = "black") +
      geom_sf(data = sde_sf, fill = "black", alpha = 0.2, color = "black") +
      theme_void() +
      labs(title = glue("  {year}")) +
      theme(
        plot.title  = element_text(size = 24),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      coord_sf(expand = TRUE)
    
    # filenames that reflect the variable used
    png_file <- file.path(
      out_dir,
      glue("map_{point_name}_{year}.png")
    )
    
    shp_file <- file.path(
      out_dir,
      glue("ellipse_{point_name}_{year}.shp")
    )
    
    shp_file_pts <- file.path(
      out_dir, 
      glue("points_{point_name}_{year}.shp")
    )
    ggsave(filename = png_file, plot = map, dpi = 300, width = 6, height = 6)
    st_write(sde_sf, dsn = shp_file, append = FALSE, quiet = TRUE)
    st_write(points, dsn = shp_file_pts, append = FALSE, quiet = TRUE)
    cat("Created map and shapefile for", point_name, "year", year, "\n")
  }
  
  invisible(points)
}


make_ellipse_maps("n_black_share")
make_ellipse_maps("n_pop_share")
make_ellipse_maps("n_renter_occ_share")
make_ellipse_maps("n_ppr_share")
make_ellipse_maps("n_20_29_share")
make_ellipse_maps("n_male_20_29_share")
make_ellipse_maps("n_black_total")
make_ellipse_maps("n_pop_total")
make_ellipse_maps("n_renter_occ_total")
make_ellipse_maps("n_ppr_total")
make_ellipse_maps("n_20_29_total")
make_ellipse_maps("n_male_20_29_total")
make_ellipse_maps("n_15_24_total")
make_ellipse_maps("n_15_24_share")
make_ellipse_maps("n_15_19_total")
make_ellipse_maps("n_15_19_share")
make_ellipse_maps("n_male_15_19_total")
make_ellipse_maps("n_male_15_19_share")

# Homicide Ellipses -------------

years <-  c(1940, 1950, 1960, 1970)

year <- 1970
for (year in years) {
  # Filter data for the current year
  homs_year <- homs_west %>% filter(year == !!year)
  
  # Calculate standard deviation ellipse for the current year
  sde_data <- std_dev_ellipse(homs_year)
  sde <- st_ellipse(geometry = sde_data, sx = sde_data$sx, sy = sde_data$sy, rotation = -sde_data$theta) 
  
  sde_polygon <- st_cast(sde, "POLYGON")
  
  sde_sf <- st_sf(
    geometry = sde_polygon  # Or replace with `year` if you want to pass it externally
  ) %>% 
    mutate(year = !!year)
  
  
  map <- ggplot() +
    # Add the transparent census layer
    geom_sf(data =census_west_outline,  fill = NA, color = "black") +
    # Add the semi-transparent sde polygons
    geom_sf(data = sde_sf, fill = "black", alpha = 0.2, color = "black") +
    theme_void() +
    labs(title = glue("  {year}") )+
    theme(plot.title = element_text(size=24)) +
    coord_sf(expand = TRUE) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Save the map as a PNG image using mapshot
  file_name <- paste0("output/westside_paper/ellipses/map_images_1940_65/map_homs_", year, ".png")
  ggsave(filename = file_name, plot = map)
  file_name <- paste0("output/westside_paper/ellipses/map_images_1940_65/ellipse_homs_", year, ".shp")
  st_write(sde_sf, dsn = file_name,append = FALSE)
  # Print progress
  cat("Created homicide map for year", year, "\n")
}


