rm(list = ls())

library(modelsummary)
library(fixest)
library(webshot2)

#Regression data in 1b is by decade due to census data - this creates a yearly dataframe that can be used for regressions
# Pull Data -----------------

homs_west <- st_read("data/mst/for_paper/homs_west.geojson")
census_walk <- sf::st_read("data/mst/for_paper/census_west_homs_geom.geojson")
select_hways_west <- st_read("data/mst/for_paper/select_hways.geojson")
cha_projects_west <- st_read("data/mst/for_paper/projects.geojson")

# Create yearly counts per census tract 

census_walk_yearly <- census_walk %>% 
  st_drop_geometry() %>% 
  dplyr::select(GISJOIN_1970, year) %>% 
  complete(GISJOIN_1970, year = seq(min(year), max(year), by = 1)) 

tract_geometry <- census_walk %>% 
  filter(year == 1940) %>%
  dplyr::select(GISJOIN_1970)

census_walk_yearly <- tract_geometry %>% 
  full_join(census_walk_yearly)

census_west_homs <- homs_west %>% 
  dplyr::select(year) %>%
  st_join(census_walk_yearly, join = st_within)%>% 
  filter(year.x == year.y) %>% 
  dplyr::rename(year = year.x) %>% 
  dplyr::select(-year.y) %>%
  st_drop_geometry() %>% 
  group_by(GISJOIN_1970, year) %>% 
  mutate(homicides = n()) %>% 
  ungroup() %>% 
  arrange(GISJOIN_1970, year) 

census_rest <- anti_join(census_walk_yearly %>% st_drop_geometry() , census_west_homs %>% distinct(GISJOIN_1970, year)) %>%
  mutate(homicides = 0) 

census_west_homs_full <- rbind(census_west_homs, census_rest) 

census_west_homs_full <- tract_geometry %>% 
  full_join(census_west_homs_full)


# Eisenhower ----------------------------

# Create distances from highway 
buffer_distance <- 0.1 * 5280  
highway_buffer_tenthmile <- st_buffer(select_hways_west, dist = buffer_distance)

buffer_distance <- 0.25 * 5280  
highway_buffer_quartmile <- st_buffer(select_hways_west, dist = buffer_distance)

buffer_distance <- 0.5 * 5280  
highway_buffer_halfmile <- st_buffer(select_hways_west, dist = buffer_distance)

buffer_distance <- 1 * 5280  
highway_buffer_1mile <- st_buffer(select_hways_west, dist = buffer_distance)


tracts_tenthmile_highway <- census_west_homs_full  %>% 
  filter(year == 1950) %>% 
  dplyr::select(GISJOIN_1970) %>% 
  st_join(highway_buffer_tenthmile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GISJOIN_1970)

tracts_quartmile_highway <- census_west_homs_full %>% 
  filter(year == 1950) %>% 
  dplyr::select(GISJOIN_1970) %>% 
  st_join(highway_buffer_quartmile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GISJOIN_1970)

tracts_halfmile_highway <- census_west_homs_full %>% 
  filter(year == 1950) %>% 
  dplyr::select(GISJOIN_1970) %>% 
  st_join(highway_buffer_halfmile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GISJOIN_1970)

tracts_1mile_highway <- census_west_homs_full %>% 
  filter(year == 1950) %>% 
  dplyr::select(GISJOIN_1970) %>% 
  st_join(highway_buffer_1mile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GISJOIN_1970)


reg_data <- census_west_homs_full %>% 
  # Eisenhower flags
  mutate(flag_tenthmile_hway = 
           ifelse(GISJOIN_1970 %in% c(tracts_tenthmile_highway$GISJOIN_1970), 1, 0), 
         flag_quartmile_hway = 
           ifelse(GISJOIN_1970 %in% c(tracts_quartmile_highway$GISJOIN_1970), 1, 0),
         flag_halfmile_hway = 
           ifelse(GISJOIN_1970 %in% c(tracts_halfmile_highway$GISJOIN_1970), 1, 0), 
         flag_1mile_hway = 
           ifelse(GISJOIN_1970 %in% c(tracts_1mile_highway$GISJOIN_1970), 1, 0)) %>% 
  mutate(flag_cat = case_when(flag_tenthmile_hway == 1 ~ "0.1 mile", 
                              flag_quartmile_hway == 1 ~ "0.25 mile", 
                              flag_halfmile_hway == 1 ~ "0.5 mile", 
                              flag_1mile_hway == 1 ~ "1 mile",
                              TRUE ~ "> 1 mile"))  %>%
  # Treatment timing
  mutate(flag_constr = ifelse(year > 1949, 1, 0))


# Housing Projects --------------------------------

project_lookup <- tibble(
  name = c("12th and Washentaw", "Adams and Wood", "Brooks Homes Extension",
           "Grace Abbott Homes", "Harrison Courts", "Henry Horner Extensions",
           "Henry Horner Homes", "Jane Addams", "Lawndale Gardens",
           "Loomis Courts", "Maplewood Courts", "Ogden Courts",
           "Robert Brooks", "Rockwell Gardens"),
  varname = c("12th_washentaw", "adams_wood", "brooks_ext",
              "grace_abbott", "harrison_courts", "horner_ext",
              "horner_homes", "jane_addams", "lawndale_gardens",
              "loomis_courts", "maplewood_courts", "ogden_courts",
              "robert_brooks", "rockwell_gardens")
)
# Define buffer distances (in miles) with label names
buffer_defs <- tibble(
  miles = c(1/20, 1/10, 1/4, 1/2),
  label = c("twentieth", "tenth", "quarter", "half")
)

# Base tracts (1970 geometry)
tracts_1970 <- census_west_homs_full %>%
  filter(year == 1950) %>%
  dplyr::select(GISJOIN_1970)

# Pre-extract opened years to avoid repeated filtering
project_lookup <- project_lookup %>%
  mutate(opened_year = map_dbl(name, ~ {
    as.numeric(cha_projects_west %>% filter(name == .x) %>% pull(opened))
  }))

# Loop over buffer distances
for (b in 1:nrow(buffer_defs)) {
  
  buffer_distance <- buffer_defs$miles[b] * 5280
  buffer_label    <- buffer_defs$label[b]
  
  # Build tract lists for each project at this buffer distance
  project_tract_list <- list()
  for (i in 1:nrow(cha_projects_west)) {
    proj <- cha_projects_west[i, ]
    proj_buffer <- st_buffer(proj, dist = buffer_distance)
    
    matched_tracts <- tracts_1970 %>%
      st_join(proj_buffer, join = st_intersects, left = FALSE) %>%
      st_drop_geometry() %>%
      distinct(GISJOIN_1970) %>%
      pull(GISJOIN_1970)
    
    project_tract_list[[proj$name]] <- matched_tracts
  }
  
  # Create only the "any housing project open" flag
  flag_varname <- paste0("flag_", buffer_label, "mile_any_hp_open")
  
  reg_data <- reg_data %>%
    mutate(!!flag_varname := {
      flag <- rep(0L, n())
      for (i in 1:nrow(project_lookup)) {
        flag <- ifelse(
          GISJOIN_1970 %in% project_tract_list[[project_lookup$name[i]]] &
            year >= project_lookup$opened_year[i],
          1L, flag
        )
      }
      flag
    })
}


st_write(reg_data, "data/mst/for_paper/reg_data_yearly.geojson", append = FALSE )

