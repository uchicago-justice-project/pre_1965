
library(sf)
library(dplyr)
library(ggplot2)
library(glue)
library(scales)
library(patchwork)
library(ggspatial)

# Calculates and plots density maps for homicides and census variables
# Census variables blocked out because no longer used 

# --- Configuration ---
CRS_M <- 26916 # UTM 16N (Meters)

# Mapping UI labels to file stubs
POINT_VARS <- c(
  "Share Units w/ >1 PPR" = "n_ppr_share",
  "Share Black"        = "n_black_share",
  "Share Population"   = "n_pop_share", 
  "Share Renter Occ. Units" = "n_renter_occ_share",
  "Share Age 20–29"    = "n_20_29_share",
  "Share Age 15–24"    = "n_15_24_share",
  "Share Age 15–19"    = "n_15_19_share",
  "Share Males 15–19"    = "n_males_15_19_share",
  "Share Males 20–29"   = "n_male_20_29_share", 
  "Total Black"        = "n_black_total",
  "Total Population"   = "n_pop_total", 
  "Total Renter Occ. Units" = "n_renter_occ_total",
  "Total Units w/ > 1 PPR"   = "n_ppr_total",
  "Total Age 20–29"    = "n_20_29_total",
  "Total Males 20–29" = "n_male_20_29_total",
  "Total Age 15-24" = "n_15_24_total", 
  "Total Age 15-19" = "n_15_19_total",
  "Total Males 15-19" = "n_male_15_19_total",
  "Homicides" = "homs"
)

# Load base layers once
census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson", quiet = TRUE) %>% 
  st_transform(CRS_M)

ellipses_homs <- st_read("data/mst/for_paper/homs_ellipses_all_years.geojson", quiet = TRUE) %>% 
  st_transform(CRS_M)

homs <- st_read("data/mst/for_paper/homs_west.geojson")

#Density limits
get_density_limits <- function(var_name, 
                               years = c(1940, 1950, 1960, 1970), 
                               bandwidth_m = 100, 
                               homs_data = homs,
                               upper_quantile = 0.75) {
  bb <- st_bbox(census_west_outline)
  
  if (var_name == "homs") {
    # Transform homs data to match the CRS
    homs_transformed <- st_transform(homs_data, CRS_M)
   
    coords <- st_coordinates(homs_transformed)
    kde <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth_m, n = 200,
                       lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
    all_vals <- as.vector(kde$z)
    
  } else {
    # For other variables, calculate KDE for each year
    all_densities <- map(years, function(y) {
      fp <- glue("output/westside_paper/ellipses/map_images_1940_65/points_{var_name}_{y}.shp")
      if (!file.exists(fp)) return(NULL)

      pts <- st_read(fp, quiet = TRUE) %>% st_transform(CRS_M)
      coords <- st_coordinates(pts)
      kde <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth_m, n = 200,
                         lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
      as.vector(kde$z)
    })

    all_vals <- unlist(all_densities)
  }
  
  c(min(all_vals, na.rm = TRUE), 
    quantile(all_vals, upper_quantile, na.rm = TRUE))
}

# --- Main Function ---
plot_density_map <- function(var_name, 
                             year, 
                             homs_data = homs,
                             bandwidth_m = 100,
                             show_var_ellipse = TRUE,
                             var_alpha = 0,
                             show_homs = TRUE,
                             save_path = "output/westside_paper/ellipses/density_maps/", 
                             density_limits = NULL) {
  
  # Get point data
  if (var_name != "homs") {
    fp <- glue("output/westside_paper/ellipses/map_images_1940_65/points_{var_name}_{year}.shp")
    pts <- st_read(fp, quiet = TRUE) %>% st_transform(CRS_M)
    fill_color <- "blue"
  } else {
    pts <- homs_data %>% 
      filter(year_decade == !!year) %>%
      st_transform(CRS_M)
    fill_color <- "red"
  }
  
  # Calculate KDE
  bb <- st_bbox(census_west_outline)
  coords <- st_coordinates(pts)
  kde <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth_m, n = 200,
                     lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
  
  df <- expand.grid(X = kde$x, Y = kde$y) %>% 
    mutate(density = as.vector(kde$z))
  
  # Start plot
  p <- ggplot() +
    geom_sf(data = census_west_outline, fill = NA, color = "black", linewidth = 0.2) +
    geom_raster(data = df, aes(X, Y, fill = density), interpolate = TRUE)
  
  # Add variable ellipse
  if (show_var_ellipse) {
    ell_fp <- glue("data/mst/for_paper/{var_name}_ellipses_all_years.geojson")
    if (file.exists(ell_fp)) {
      v_ell <- st_read(ell_fp, quiet = TRUE) %>% 
        st_transform(CRS_M) %>%
        filter(year == !!year)
      p <- p + geom_sf(data = v_ell, fill = fill_color, linewidth = 1, 
                       color = fill_color, alpha = var_alpha)
    }
  }
  
  # Add homicide ellipse
  if (show_homs) {
    h_ell <- filter(ellipses_homs, year == !!year)
    p <- p + geom_sf(data = h_ell, fill = NA, color = "red", linewidth = 1, linetype = "dashed")
  }
  
  # Finalize plot
  var_label <- names(POINT_VARS)[POINT_VARS == var_name]
  p <- p + 
    scale_fill_gradient(low = "white", high = fill_color, na.value = "white", 
                        limits = density_limits, oob = squish) +
    theme_void() + 
    labs(title = glue("{var_label} ({year})")) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    geom_sf(data = census_west_outline, fill = NA, color = "grey50", linewidth = 0.2)
  
  # Save if requested
  if (!is.null(save_path)) {
    filename <- glue("{var_name}_{year}.png")
    ggsave(paste0(save_path, filename), p, width = 8, height = 10, dpi = 300)
    message("Saved to ", save_path)
  }
  
  return(p)
}

# limits <- get_density_limits("n_black_share", bandwidth_m = 100, upper_quantile = 0.98)
# 
# 
# p1 <- plot_density_map("n_black_share", 1940, bandwidth_m = 150, density_limits = limits)
# p2 <- plot_density_map("n_black_share", 1950, bandwidth_m = 150, density_limits = limits)
# p3 <- plot_density_map("n_black_share", 1960, bandwidth_m = 150, density_limits = limits)
# p4 <- plot_density_map("n_black_share", 1970, bandwidth_m = 150, density_limits = limits)
# 
# 
# combined <- (p1 + p2) / (p3 + p4) +
#   plot_layout(guides = "collect")
# 
# ggsave("output/westside_paper/ellipses/density_maps/black_share_density_panel.png", combined, width = 12, height = 10, dpi = 300)
# 
# limits <- get_density_limits("n_ppr_share", bandwidth_m = 100, upper_quantile = 0.98)
# 
# plot_density_map("n_ppr_share", 1940, density_limits = limits)
# plot_density_map("n_ppr_share", 1950, density_limits = limits)
# plot_density_map("n_ppr_share", 1960, density_limits = limits)
# plot_density_map("n_ppr_share", 1970, density_limits = limits)
# 
# limits <- get_density_limits("n_20_29_share", bandwidth_m = 100, upper_quantile = 0.98)
# 
# plot_density_map("n_20_29_share", 1940, density_limits = limits)
# plot_density_map("n_20_29_share", 1950, density_limits = limits)
# plot_density_map("n_20_29_share", 1960, density_limits = limits)
# plot_density_map("n_20_29_share", 1970, density_limits = limits)
# 
# limits <- get_density_limits("n_pop_share", bandwidth_m = 200, upper_quantile = 0.98)
# 
# plot_density_map("n_pop_share", 1940, bandwidth_m = 200, density_limits = limits)
# plot_density_map("n_pop_share", 1950, bandwidth_m = 200, density_limits = limits)
# plot_density_map("n_pop_share", 1960, bandwidth_m = 200, density_limits = limits)
# plot_density_map("n_pop_share", 1970, bandwidth_m = 200, density_limits = limits)


limits <- get_density_limits("homs", bandwidth_m = 150, upper_quantile = 0.99)

p1 <- plot_density_map("homs", 1940, bandwidth_m = 150, density_limits = limits)
p2 <- plot_density_map("homs", 1950, bandwidth_m = 150, density_limits = limits)
p3 <- plot_density_map("homs", 1960, bandwidth_m = 150, density_limits = limits)
p4 <- plot_density_map("homs", 1970, bandwidth_m = 150, density_limits = limits)


combined <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect")

ggsave("output/westside_paper/ellipses/density_maps/homicide_density_panel.png", combined, width = 12, height = 10, dpi = 300)

=======
library(sf)
library(dplyr)
library(ggplot2)
library(glue)
library(scales)
library(patchwork)
library(ggspatial)

# Calculates and plots density maps for homicides and census variables
# Census variables blocked out because no longer used 

# --- Configuration ---
CRS_M <- 26916 # UTM 16N (Meters)

# Mapping UI labels to file stubs
POINT_VARS <- c(
  "Share Units w/ >1 PPR" = "n_ppr_share",
  "Share Black"        = "n_black_share",
  "Share Population"   = "n_pop_share", 
  "Share Renter Occ. Units" = "n_renter_occ_share",
  "Share Age 20–29"    = "n_20_29_share",
  "Share Age 15–24"    = "n_15_24_share",
  "Share Age 15–19"    = "n_15_19_share",
  "Share Males 15–19"    = "n_males_15_19_share",
  "Share Males 20–29"   = "n_male_20_29_share", 
  "Total Black"        = "n_black_total",
  "Total Population"   = "n_pop_total", 
  "Total Renter Occ. Units" = "n_renter_occ_total",
  "Total Units w/ > 1 PPR"   = "n_ppr_total",
  "Total Age 20–29"    = "n_20_29_total",
  "Total Males 20–29" = "n_male_20_29_total",
  "Total Age 15-24" = "n_15_24_total", 
  "Total Age 15-19" = "n_15_19_total",
  "Total Males 15-19" = "n_male_15_19_total",
  "Homicides" = "homs"
)

# Load base layers once
census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson", quiet = TRUE) %>% 
  st_transform(CRS_M)

ellipses_homs <- st_read("data/mst/for_paper/homs_ellipses_all_years.geojson", quiet = TRUE) %>% 
  st_transform(CRS_M)

homs <- st_read("data/mst/for_paper/homs_west.geojson")

#Density limits
get_density_limits <- function(var_name, 
                               years = c(1940, 1950, 1960, 1970), 
                               bandwidth_m = 100, 
                               homs_data = homs,
                               upper_quantile = 0.75) {
  bb <- st_bbox(census_west_outline)
  
  if (var_name == "homs") {
    # Transform homs data to match the CRS
    homs_transformed <- st_transform(homs_data, CRS_M)
   
    coords <- st_coordinates(homs_transformed)
    kde <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth_m, n = 200,
                       lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
    all_vals <- as.vector(kde$z)
    
  } else {
    # For other variables, calculate KDE for each year
    all_densities <- map(years, function(y) {
      fp <- glue("output/westside_paper/ellipses/map_images_1940_65/points_{var_name}_{y}.shp")
      if (!file.exists(fp)) return(NULL)

      pts <- st_read(fp, quiet = TRUE) %>% st_transform(CRS_M)
      coords <- st_coordinates(pts)
      kde <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth_m, n = 200,
                         lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
      as.vector(kde$z)
    })

    all_vals <- unlist(all_densities)
  }
  
  c(min(all_vals, na.rm = TRUE), 
    quantile(all_vals, upper_quantile, na.rm = TRUE))
}

# --- Main Function ---
plot_density_map <- function(var_name, 
                             year, 
                             homs_data = homs,
                             bandwidth_m = 100,
                             show_var_ellipse = TRUE,
                             var_alpha = 0,
                             show_homs = TRUE,
                             save_path = "output/westside_paper/ellipses/density_maps/", 
                             density_limits = NULL) {
  
  # Get point data
  if (var_name != "homs") {
    fp <- glue("output/westside_paper/ellipses/map_images_1940_65/points_{var_name}_{year}.shp")
    pts <- st_read(fp, quiet = TRUE) %>% st_transform(CRS_M)
    fill_color <- "blue"
  } else {
    pts <- homs_data %>% 
      filter(year_decade == !!year) %>%
      st_transform(CRS_M)
    fill_color <- "red"
  }
  
  # Calculate KDE
  bb <- st_bbox(census_west_outline)
  coords <- st_coordinates(pts)
  kde <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth_m, n = 200,
                     lims = c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
  
  df <- expand.grid(X = kde$x, Y = kde$y) %>% 
    mutate(density = as.vector(kde$z))
  
  # Start plot
  p <- ggplot() +
    geom_sf(data = census_west_outline, fill = NA, color = "black", linewidth = 0.2) +
    geom_raster(data = df, aes(X, Y, fill = density), interpolate = TRUE)
  
  # Add variable ellipse
  if (show_var_ellipse) {
    ell_fp <- glue("data/mst/for_paper/{var_name}_ellipses_all_years.geojson")
    if (file.exists(ell_fp)) {
      v_ell <- st_read(ell_fp, quiet = TRUE) %>% 
        st_transform(CRS_M) %>%
        filter(year == !!year)
      p <- p + geom_sf(data = v_ell, fill = fill_color, linewidth = 1, 
                       color = fill_color, alpha = var_alpha)
    }
  }
  
  # Add homicide ellipse
  if (show_homs) {
    h_ell <- filter(ellipses_homs, year == !!year)
    p <- p + geom_sf(data = h_ell, fill = NA, color = "red", linewidth = 1, linetype = "dashed")
  }
  
  # Finalize plot
  var_label <- names(POINT_VARS)[POINT_VARS == var_name]
  p <- p + 
    scale_fill_gradient(low = "white", high = fill_color, na.value = "white", 
                        limits = density_limits, oob = squish) +
    theme_void() + 
    labs(title = glue("{var_label} ({year})")) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    geom_sf(data = census_west_outline, fill = NA, color = "grey50", linewidth = 0.2)
  
  # Save if requested
  if (!is.null(save_path)) {
    filename <- glue("{var_name}_{year}.png")
    ggsave(paste0(save_path, filename), p, width = 8, height = 10, dpi = 300)
    message("Saved to ", save_path)
  }
  
  return(p)
}

# limits <- get_density_limits("n_black_share", bandwidth_m = 100, upper_quantile = 0.98)
# 
# 
# p1 <- plot_density_map("n_black_share", 1940, bandwidth_m = 150, density_limits = limits)
# p2 <- plot_density_map("n_black_share", 1950, bandwidth_m = 150, density_limits = limits)
# p3 <- plot_density_map("n_black_share", 1960, bandwidth_m = 150, density_limits = limits)
# p4 <- plot_density_map("n_black_share", 1970, bandwidth_m = 150, density_limits = limits)
# 
# 
# combined <- (p1 + p2) / (p3 + p4) +
#   plot_layout(guides = "collect")
# 
# ggsave("output/westside_paper/ellipses/density_maps/black_share_density_panel.png", combined, width = 12, height = 10, dpi = 300)
# 
# limits <- get_density_limits("n_ppr_share", bandwidth_m = 100, upper_quantile = 0.98)
# 
# plot_density_map("n_ppr_share", 1940, density_limits = limits)
# plot_density_map("n_ppr_share", 1950, density_limits = limits)
# plot_density_map("n_ppr_share", 1960, density_limits = limits)
# plot_density_map("n_ppr_share", 1970, density_limits = limits)
# 
# limits <- get_density_limits("n_20_29_share", bandwidth_m = 100, upper_quantile = 0.98)
# 
# plot_density_map("n_20_29_share", 1940, density_limits = limits)
# plot_density_map("n_20_29_share", 1950, density_limits = limits)
# plot_density_map("n_20_29_share", 1960, density_limits = limits)
# plot_density_map("n_20_29_share", 1970, density_limits = limits)
# 
# limits <- get_density_limits("n_pop_share", bandwidth_m = 200, upper_quantile = 0.98)
# 
# plot_density_map("n_pop_share", 1940, bandwidth_m = 200, density_limits = limits)
# plot_density_map("n_pop_share", 1950, bandwidth_m = 200, density_limits = limits)
# plot_density_map("n_pop_share", 1960, bandwidth_m = 200, density_limits = limits)
# plot_density_map("n_pop_share", 1970, bandwidth_m = 200, density_limits = limits)


limits <- get_density_limits("homs", bandwidth_m = 150, upper_quantile = 0.99)

p1 <- plot_density_map("homs", 1940, bandwidth_m = 150, density_limits = limits)
p2 <- plot_density_map("homs", 1950, bandwidth_m = 150, density_limits = limits)
p3 <- plot_density_map("homs", 1960, bandwidth_m = 150, density_limits = limits)
p4 <- plot_density_map("homs", 1970, bandwidth_m = 150, density_limits = limits)


combined <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect")

ggsave("output/westside_paper/ellipses/density_maps/homicide_density_panel.png", combined, width = 12, height = 10, dpi = 300)

>>>>>>> Stashed changes
