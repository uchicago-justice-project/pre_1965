<<<<<<< Updated upstream
rm(list = ls())
source("header.R")
library(ggspatial)
library(prettymapr)
library(ggrepel)

# Data visualization plots 

# Read data -----------
census_west_homs_geom <- st_read("data/mst/for_paper/census_west_homs_geom.geojson")
census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson")
ellipses_homs <- st_read(glue("data/mst/for_paper/homs_ellipses_all_years.geojson"))

homs_west <- st_read("data/mst/for_paper/homs_west.geojson")
homs_nonwest <- st_read("data/mst/for_paper/homs_nonwest.geojson")


ellipses <- ellipses_homs %>% 
  mutate(center = st_centroid(geometry)) %>% 
  mutate(
  prev_center = lag(center),
  # Extract X coordinates (longitude)
  current_x = st_coordinates(center)[,1],
  prev_x = lag(current_x),
  # Calculate distance with sign
  distance_center_prev = ifelse(
    !is.na(prev_center),
    st_distance(center, prev_center, by_element = TRUE) * 
      ifelse(current_x > prev_x, -1, 1),  # Negative for eastward movement
    NA_real_
  )
) %>%
  # Remove temporary coordinate columns
  dplyr::select( -prev_x) %>% 
  mutate(decade = floor(year/10)*10) 



counts <- homs_west %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  ungroup() 

counts_nonwest <- homs_nonwest %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarise(count_ch = n()) %>% 
  ungroup() 

counts_all <- cbind(counts, counts_nonwest %>% dplyr::select(count_ch)) 

write_csv(counts_all, "output/westside_paper/data_vis/counts_homicides.csv")

map <- ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +  
  
  geom_sf(data = census_west_outline , 
          fill = NA, color = "black", size = 1, linewidth = 2) +
  theme_minimal()

ggsave(filename = "output/westside_paper/data_vis/map_westside.png", plot = map, width = 10, height = 8, dpi = 300)


p <- ggplot(data = ellipses, aes(x = year, y = -current_x)) +
  geom_line() + 
  geom_point(size = 2) +
  geom_label(aes(label = year), size = 3) +
  labs(y = "Latititude of Ellipse Center", x = "Year") +
  scale_y_continuous(labels = function(x) paste0(format(x, nsmall  = 2), "°W"), limits = c(87.665, 87.711), expand = c(0, 0)) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  theme_classic()

ggsave(filename = "output/westside_paper/data_vis/movement_plot.png", plot = p, width = 10, height = 8, dpi = 300)


p <- ggplot(data = counts, aes(x = year, y = count)) +
  geom_line() + 
  geom_point(size = 2) +
  geom_label(aes(label = paste0("'", year-1900, ":\n", count)), size = 3) +
  labs(y = "Number of Homicides in Chicago West Side", x = "Year") +
  scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  theme_classic()

ggsave(filename = "output/westside_paper/data_vis/number_homicides.png", plot = p, width = 10, height = 8, dpi = 300)

scale_factor <- counts_all$count_ch[1] / counts_all$count[1]

p <- ggplot(data = counts_all, aes(x = year)) +
  # West Side
  geom_line(aes(y = count)) +
  geom_point(aes(y = count), size = 2) +
  geom_label(aes(y = count, label = paste0("'", year - 1900, ":\n", count)), size = 3) +
  # Non-West (rescaled to fit primary axis)
  geom_line(aes(y = count_ch / scale_factor), linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_point(aes(y = count_ch / scale_factor), size = 2, color = "grey50", alpha = 0.5) +
  # Axes
  scale_y_continuous(
    limits = c(0, 300), expand = c(0, 1),
    sec.axis = sec_axis(~ . * scale_factor, name = "Number of Homicides in Chicago Excl. West Side")
  ) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  labs(y = "Number of Homicides in Chicago West Side", x = "Year") +
  theme_classic()

ggsave(filename = "output/westside_paper/data_vis/number_homicides_dual.png", plot = p, width = 10, height = 8, dpi = 300)

p <- ggplot(data = counts_all, aes(x = year)) +
  # West Side
  geom_line(aes(y = count)) +
  geom_point(aes(y = count), size = 2) +
  # Non-West (on same axis)
  geom_line(aes(y = count_ch), linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_point(aes(y = count_ch), size = 2, color = "grey50", alpha = 0.5) +
  geom_label(
    aes(y = count_ch, label = count_ch),
    size = 3, color = "grey50", alpha = 0.8, nudge_y = 15
  ) +
  # All other non-West labels
  geom_label(
    aes(y = count, label = count),
    size = 3, color = "black", alpha = 1
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  labs(y = "Number of Homicides", x = "Year") +
  theme_classic()
p
ggsave(filename = "output/westside_paper/data_vis/number_homicides_both_primary.png", plot = p, width = 10, height = 8, dpi = 300)

# 
# p <- ggplot(data = counts_nonwest, aes(x = year, y = count)) +
#   geom_line() + 
#   geom_point(size = 2) +
#   geom_label(aes(label = paste0("'", year-1900, ":\n", count)), size = 3) +
#   labs(y = "Number of Homicides in Chicago excl. West Side", x = "Year") +
#   scale_y_continuous(limits = c(0, 710), labels = seq(0, 700, 100), breaks = seq(0, 700, 100), expand = c(0, 0.4)) +
#   scale_x_continuous(labels = seq(1940, 1974, 5), breaks = seq(1940, 1975, 5)) +
#   theme_classic()
# 
# ggsave(filename = "output/westside_paper/data_vis/number_homicides_chicago.png", plot = p, width = 10, height = 8, dpi = 300)
=======
rm(list = ls())
source("header.R")
library(ggspatial)
library(prettymapr)
library(ggrepel)

# Data visualization plots 

# Read data -----------
census_west_homs_geom <- st_read("data/mst/for_paper/census_west_homs_geom.geojson")
census_west_outline <- st_read("data/mst/for_paper/census_west_outline.geojson")
ellipses_homs <- st_read(glue("data/mst/for_paper/homs_ellipses_all_years.geojson"))

homs_west <- st_read("data/mst/for_paper/homs_west.geojson")
homs_nonwest <- st_read("data/mst/for_paper/homs_nonwest.geojson")


ellipses <- ellipses_homs %>% 
  mutate(center = st_centroid(geometry)) %>% 
  mutate(
  prev_center = lag(center),
  # Extract X coordinates (longitude)
  current_x = st_coordinates(center)[,1],
  prev_x = lag(current_x),
  # Calculate distance with sign
  distance_center_prev = ifelse(
    !is.na(prev_center),
    st_distance(center, prev_center, by_element = TRUE) * 
      ifelse(current_x > prev_x, -1, 1),  # Negative for eastward movement
    NA_real_
  )
) %>%
  # Remove temporary coordinate columns
  dplyr::select( -prev_x) %>% 
  mutate(decade = floor(year/10)*10) 



counts <- homs_west %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  ungroup() 

counts_nonwest <- homs_nonwest %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarise(count_ch = n()) %>% 
  ungroup() 

counts_all <- cbind(counts, counts_nonwest %>% dplyr::select(count_ch)) 

write_csv(counts_all, "output/westside_paper/data_vis/counts_homicides.csv")

map <- ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +  
  
  geom_sf(data = census_west_outline , 
          fill = NA, color = "black", size = 1, linewidth = 2) +
  theme_minimal()

ggsave(filename = "output/westside_paper/data_vis/map_westside.png", plot = map, width = 10, height = 8, dpi = 300)


p <- ggplot(data = ellipses, aes(x = year, y = -current_x)) +
  geom_line() + 
  geom_point(size = 2) +
  geom_label(aes(label = year), size = 3) +
  labs(y = "Latititude of Ellipse Center", x = "Year") +
  scale_y_continuous(labels = function(x) paste0(format(x, nsmall  = 2), "°W"), limits = c(87.665, 87.711), expand = c(0, 0)) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  theme_classic()

ggsave(filename = "output/westside_paper/data_vis/movement_plot.png", plot = p, width = 10, height = 8, dpi = 300)


p <- ggplot(data = counts, aes(x = year, y = count)) +
  geom_line() + 
  geom_point(size = 2) +
  geom_label(aes(label = paste0("'", year-1900, ":\n", count)), size = 3) +
  labs(y = "Number of Homicides in Chicago West Side", x = "Year") +
  scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  theme_classic()

ggsave(filename = "output/westside_paper/data_vis/number_homicides.png", plot = p, width = 10, height = 8, dpi = 300)

scale_factor <- counts_all$count_ch[1] / counts_all$count[1]

p <- ggplot(data = counts_all, aes(x = year)) +
  # West Side
  geom_line(aes(y = count)) +
  geom_point(aes(y = count), size = 2) +
  geom_label(aes(y = count, label = paste0("'", year - 1900, ":\n", count)), size = 3) +
  # Non-West (rescaled to fit primary axis)
  geom_line(aes(y = count_ch / scale_factor), linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_point(aes(y = count_ch / scale_factor), size = 2, color = "grey50", alpha = 0.5) +
  # Axes
  scale_y_continuous(
    limits = c(0, 300), expand = c(0, 1),
    sec.axis = sec_axis(~ . * scale_factor, name = "Number of Homicides in Chicago Excl. West Side")
  ) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  labs(y = "Number of Homicides in Chicago West Side", x = "Year") +
  theme_classic()

ggsave(filename = "output/westside_paper/data_vis/number_homicides_dual.png", plot = p, width = 10, height = 8, dpi = 300)

p <- ggplot(data = counts_all, aes(x = year)) +
  # West Side
  geom_line(aes(y = count)) +
  geom_point(aes(y = count), size = 2) +
  # Non-West (on same axis)
  geom_line(aes(y = count_ch), linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_point(aes(y = count_ch), size = 2, color = "grey50", alpha = 0.5) +
  geom_label(
    aes(y = count_ch, label = count_ch),
    size = 3, color = "grey50", alpha = 0.8, nudge_y = 15
  ) +
  # All other non-West labels
  geom_label(
    aes(y = count, label = count),
    size = 3, color = "black", alpha = 1
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_continuous(labels = seq(1940, 1975, 5), breaks = seq(1940, 1975, 5)) +
  labs(y = "Number of Homicides", x = "Year") +
  theme_classic()
p
ggsave(filename = "output/westside_paper/data_vis/number_homicides_both_primary.png", plot = p, width = 10, height = 8, dpi = 300)

# 
# p <- ggplot(data = counts_nonwest, aes(x = year, y = count)) +
#   geom_line() + 
#   geom_point(size = 2) +
#   geom_label(aes(label = paste0("'", year-1900, ":\n", count)), size = 3) +
#   labs(y = "Number of Homicides in Chicago excl. West Side", x = "Year") +
#   scale_y_continuous(limits = c(0, 710), labels = seq(0, 700, 100), breaks = seq(0, 700, 100), expand = c(0, 0.4)) +
#   scale_x_continuous(labels = seq(1940, 1974, 5), breaks = seq(1940, 1975, 5)) +
#   theme_classic()
# 
# ggsave(filename = "output/westside_paper/data_vis/number_homicides_chicago.png", plot = p, width = 10, height = 8, dpi = 300)
>>>>>>> Stashed changes
