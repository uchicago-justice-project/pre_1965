# Regression Analysis Summary: Eisenhower Expressway and Homicides in Chicago (1940-1960)
# ======================================================================================

# Research Question: Does proximity to the Eisenhower Expressway construction 
# (begun 1949) causally increase homicide rates in surrounding neighborhoods, 
# and does this effect vary by racial composition?

# ======================================================================================
# DATA & GEOGRAPHIC SCOPE
# ======================================================================================

# Study Period: 1940-1960
# Treatment timing: Construction began December 1949
# Outcome measure: Homicide counts per hexagonal grid cell
# Geographic Units: Hexagonal grid cells (150m)
# Study area: Chicago, with focus on areas within 1 mile of Eisenhower Expressway
# Comparison highway: Dan Ryan Expressway (for placebo/robustness checks)

# Key Timeline:
# - 1940: Eisenhower Expressway officially authorized
# - December 1949: Construction begins
# - 1955: First segment opens (Ashland to Laramie)
# - 1956: Downtown extension completed

# ======================================================================================
# SETUP
# ======================================================================================

rm(list = ls())
source("header.R")
library(fixest)
library(modelsummary)

# Load data
hex <- st_read("data/intermediate/homicide_hexagons/survivorship") %>% 
  mutate(year = as.integer(year))

streets <- st_read("data/intermediate/highways.gpkg")
streets <- st_transform(streets, st_crs(hex))

yearly_panel <- read_csv("data/raw/yearly_panel_dataset.csv")

yearly_panel <- hex %>% 
  dplyr::select(GRID_ID, year) %>% 
  left_join(yearly_panel, by = c("GRID_ID", "year")) %>% 
  mutate(year = as.integer(year))

# ======================================================================================
# TREATMENT & CONTROL GROUP DEFINITIONS
# ======================================================================================

# ---------------------------------------------------------------------------------------
# Proximity-Based Treatment: Eisenhower Expressway
# ---------------------------------------------------------------------------------------
# Create multiple distance buffers from Eisenhower Expressway:
# - 1/10 mile buffer (primary treatment group)
# - 1/4 mile buffer
# - 1/2 mile buffer 
# - 1 mile buffer (often used as control)

highway <- streets %>% filter(str_detect(DEDICATED_, "EISENHOWER EXPY"))

# Create distances from highway 
buffer_distance <- 0.1 * 5280  
highway_buffer_tenthmile <- st_buffer(highway, dist = buffer_distance)

buffer_distance <- 0.25 * 5280  
highway_buffer_quartmile <- st_buffer(highway, dist = buffer_distance)

buffer_distance <- 0.5 * 5280  
highway_buffer_halfmile <- st_buffer(highway, dist = buffer_distance)

buffer_distance <- 1 * 5280  
highway_buffer_1mile <- st_buffer(highway, dist = buffer_distance)

# Intersect with polygons
polygons_tenthmile_highway <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_tenthmile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

polygons_quartmile_highway <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_quartmile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

polygons_halfmile_highway <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_halfmile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

polygons_1_highway <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_1mile, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

# ---------------------------------------------------------------------------------------
# Proximity-Based Treatment: Dan Ryan Expressway (Comparison)
# ---------------------------------------------------------------------------------------

highway_danry <- streets %>% filter(str_detect(DEDICATED_, "DAN RYAN EXPY"))

# Create distances from Dan Ryan
buffer_distance <- 0.1 * 5280  
highway_buffer_tenthmile_danry <- st_buffer(highway_danry, dist = buffer_distance)

buffer_distance <- 0.25 * 5280  
highway_buffer_quartmile_danry <- st_buffer(highway_danry, dist = buffer_distance)

buffer_distance <- 0.5 * 5280  
highway_buffer_halfmile_danry <- st_buffer(highway_danry, dist = buffer_distance)

buffer_distance <- 1 * 5280  
highway_buffer_1mile_danry <- st_buffer(highway_danry, dist = buffer_distance)

# Intersect with polygons
polygons_tenthmile_highway_danry <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_tenthmile_danry, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

polygons_quartmile_highway_danry <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_quartmile_danry, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

polygons_halfmile_highway_danry <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_halfmile_danry, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

polygons_1_highway_danry <- yearly_panel %>% 
  filter(year == 1950) %>% 
  dplyr::select(GRID_ID) %>% 
  st_join(highway_buffer_1mile_danry, join = st_intersects, left = FALSE) %>% 
  st_drop_geometry() %>%
  distinct(GRID_ID)

# ---------------------------------------------------------------------------------------
# Create Analysis Dataset with Treatment Flags
# ---------------------------------------------------------------------------------------

# Create flags in data 
reg_data <- yearly_panel %>% 
  filter(year >= 1940 & year <= 1960) %>% 
  # Eisenhower flags
  mutate(flag_tenthmile_hway = 
           ifelse(GRID_ID %in% c(polygons_tenthmile_highway$GRID_ID), 1, 0), 
         flag_quartmile_hway = 
           ifelse(GRID_ID %in% c(polygons_quartmile_highway$GRID_ID), 1, 0),
         flag_halfmile_hway = 
           ifelse(GRID_ID %in% c(polygons_halfmile_highway$GRID_ID), 1, 0), 
         flag_1mile_hway = 
           ifelse(GRID_ID %in% c(polygons_1_highway$GRID_ID), 1, 0)) %>% 
  mutate(flag_cat = case_when(flag_tenthmile_hway == 1 ~ "0.1 mile", 
                              flag_quartmile_hway == 1 ~ "0.25 mile", 
                              flag_halfmile_hway == 1 ~ "0.5 mile", 
                              flag_1mile_hway == 1 ~ "1 mile",
                              TRUE ~ "> 1 mile")) %>% 
  # Dan Ryan flags
  mutate(flag_tenthmile_hway_danry = 
           ifelse(GRID_ID %in% c(polygons_tenthmile_highway_danry$GRID_ID), 1, 0), 
         flag_quartmile_hway_danry = 
           ifelse(GRID_ID %in% c(polygons_quartmile_highway_danry$GRID_ID), 1, 0),
         flag_halfmile_hway_danry = 
           ifelse(GRID_ID %in% c(polygons_halfmile_highway_danry$GRID_ID), 1, 0), 
         flag_1mile_hway_danry = 
           ifelse(GRID_ID %in% c(polygons_1_highway_danry$GRID_ID), 1, 0)) %>% 
  mutate(flag_cat_danry = case_when(flag_tenthmile_hway_danry == 1 ~ "0.1 mile", 
                                    flag_quartmile_hway_danry == 1 ~ "0.25 mile", 
                                    flag_halfmile_hway_danry == 1 ~ "0.5 mile", 
                                    flag_1mile_hway_danry == 1 ~ "1 mile",
                                    TRUE ~ "> 1 mile")) %>% 
  # Treatment timing
  mutate(flag_constr = ifelse(year > 1949, 1, 0))

# ---------------------------------------------------------------------------------------
# Racial Composition Measures
# ---------------------------------------------------------------------------------------
# Binary indicators based on 1950 census:
# - flag_black_30: Share Black > 30%
# - flag_black_50: Share Black > 50%
# - flag_black_70: Share Black > 70%

black_50 <- yearly_panel %>%
  st_drop_geometry() %>% 
  filter(year == 1950) %>% 
  mutate(flag_black_30 = ifelse(share_black > 0.3, 1, 0), 
         flag_black_50 = ifelse(share_black > 0.5, 1, 0), 
         flag_black_70 = ifelse(share_black > 0.7, 1, 0)) %>% 
  dplyr::select(GRID_ID, flag_black_30, flag_black_50, flag_black_70)

reg_data <- reg_data %>% 
  left_join(black_50)

# ======================================================================================
# PARALLEL TRENDS ANALYSIS
# ======================================================================================

# Event study specification:
# feglm(homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year)

# Researcher notes: "Issues in 1940, 1941, and 1946"

# ---------------------------------------------------------------------------------------
# Sample 1: Full Chicago
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs Chicago ===\n")
ev_chicago <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data 
  , family = "poisson"
)

# ---------------------------------------------------------------------------------------
# Sample 2: Full Chicago, Black Residency > 50%
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs Chicago for Black Residency > 50% ===\n")
ev_chicago_black50 <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data %>% filter(flag_black_50 == 1)
  , family = "poisson"
)

# ---------------------------------------------------------------------------------------
# Sample 3: 1 Mile from Eisenhower
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs 1 mile from Eisenhower ===\n")
ev_eisenhower <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data %>% filter(flag_1mile_hway == 1)
  , family = "poisson"
)

# ---------------------------------------------------------------------------------------
# Sample 4: 1 Mile from Eisenhower, Black Residency > 50%
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs 1 mile from Eisenhower for Black Residency > 50% ===\n")
ev_eisenhower_black50 <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data %>% filter(flag_1mile_hway == 1 & flag_black_50 == 1)
  , family = "poisson"
)

# ---------------------------------------------------------------------------------------
# Sample 5: Combined Eisenhower + Dan Ryan Buffers
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs 1 mile Eisenhower & 1 mile Dan Ryan ===\n")
ev_combined <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data %>% filter(flag_1mile_hway == 1 | flag_1mile_hway_danry == 1)
  , family = "poisson"
)

# ---------------------------------------------------------------------------------------
# Sample 6: Combined Buffers, Black Residency > 50%
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs 1 mile Eisenhower & 1 mile Dan Ryan, Black Residency > 50% ===\n")
ev_combined_black50 <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data %>% filter((flag_1mile_hway == 1 | flag_1mile_hway_danry == 1) & flag_black_50 == 1)
  , family = "poisson"
)

# ---------------------------------------------------------------------------------------
# Sample 7: Combined Buffers, Black Residency > 30%
# ---------------------------------------------------------------------------------------
cat("\n=== PARALLEL TRENDS: 1/10 mile vs 1 mile Eisenhower & 1 mile Dan Ryan, Black Residency > 30% ===\n")
ev_combined_black30 <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1949) | GRID_ID + year 
  , data = reg_data %>% filter((flag_1mile_hway == 1 | flag_1mile_hway_danry == 1) & flag_black_30 == 1)
  , family = "poisson"
)

# ======================================================================================
# DESCRIPTIVE TIME SERIES ANALYSES
# ======================================================================================

# ---------------------------------------------------------------------------------------
# Homicide Trends: 1-Mile Buffer Comparison
# ---------------------------------------------------------------------------------------
# Finding: "Homicide trends look very similar [between Eisenhower and Dan Ryan areas]"
# Finding: "Most homicides for high black residency areas happen in these two areas"

for_plot_1mile <- reg_data %>% 
  filter(flag_black_30 == 1) %>%
  mutate(category = case_when(flag_1mile_hway == 1 ~ "1 Mile Eisenhower", 
                              flag_1mile_hway_danry == 1 ~ "1 Mile Dan Ryan", 
                              flag_1mile_hway == 0 &  flag_1mile_hway_danry == 0 ~ "Rest of Chicago")) %>% 
  group_by(category, year) %>% 
  summarise(homicides = sum(homicides)) %>% 
  ungroup()

ggplot(data = for_plot_1mile, aes(x = year, y = homicides)) + 
  geom_line(aes(color = category)) + 
  labs(title = "1 Mile Eisenhower vs 1 Mile Dan Ryan (excl Eisenhower) vs Chicago",
       subtitle = "Black Residency > 30%") + 
  geom_vline(xintercept = 1949)

# ---------------------------------------------------------------------------------------
# Homicide Trends: 1/10 Mile Comparison, Black Residency > 50%
# ---------------------------------------------------------------------------------------
# Finding: "Number of homicides immediately surrounding the highway is much lower for the Eisenhower"
# Finding: "Since the number of homicides is so low near the Eisenhower, those slight 
#          increases in 41, 42, and 46 seem to matter much more"
# Conclusion: "Need to look into how to make the two areas more comparable. 
#             We may not want to use 1/10 mile distance for each group."

for_plot_tenth_black50 <- reg_data %>% 
  filter(flag_black_50 == 1 & (flag_tenthmile_hway_danry == 1 | flag_tenthmile_hway == 1)) %>%
  mutate(category = case_when(flag_tenthmile_hway == 1 ~ "1/10 Mile Eisenhower", 
                              flag_tenthmile_hway_danry == 1 ~ "1/10 Mile Dan Ryan")) %>% 
  group_by(category, year) %>% 
  summarise(homicides = sum(homicides)) %>% 
  ungroup()

ggplot(data = for_plot_tenth_black50, aes(x = year, y = homicides)) + 
  geom_line(aes(color = category)) + 
  labs(title = "1/10 Mile Eisenhower vs 1/10 Mile Dan Ryan (excl Eisenhower) vs Chicago",
       subtitle = "Black Residency > 50%") + 
  geom_vline(xintercept = 1949)

# ---------------------------------------------------------------------------------------
# Homicide Trends: 1/10 Mile Comparison, Black Residency > 30%
# ---------------------------------------------------------------------------------------
# Note: "Reducing the flag to 30% helps a little but not much"

for_plot_tenth_black30 <- reg_data %>% 
  filter(flag_black_30 == 1 & (flag_tenthmile_hway_danry == 1 | flag_tenthmile_hway == 1)) %>%
  mutate(category = case_when(flag_tenthmile_hway == 1 ~ "1/10 Mile Eisenhower", 
                              flag_tenthmile_hway_danry == 1 ~ "1/10 Mile Dan Ryan")) %>% 
  group_by(category, year) %>% 
  summarise(homicides = sum(homicides)) %>% 
  ungroup()

ggplot(data = for_plot_tenth_black30, aes(x = year, y = homicides)) + 
  geom_line(aes(color = category)) + 
  labs(title = "1/10 Mile Eisenhower vs 1/10 Mile Dan Ryan (excl Eisenhower) vs Chicago",
       subtitle = "Black Residency > 30%") + 
  geom_vline(xintercept = 1949)

# ======================================================================================
# MAIN REGRESSIONS
# ======================================================================================

# ======================================================================================
# TABLE 1: Distance Gradient Effects (No Controls)
# ======================================================================================
# Finding: "Effect is most pronounced the closer we get to highway"

cat("\n=== TABLE 1: Distance Gradient Effects ===\n")

reg_halfmile_no_controls <- feglm(homicides ~ flag_constr:flag_halfmile_hway
                                  | year + GRID_ID
                                  , data = reg_data
                                  , family = "poisson")

reg_quartmile_no_controls <- feglm(homicides ~ flag_constr:flag_quartmile_hway
                                   | year + GRID_ID
                                   , data = reg_data
                                   , family = "poisson")

reg_tenthmile_no_controls <- feglm(homicides ~ flag_constr:flag_tenthmile_hway
                                   | year + GRID_ID
                                   , data = reg_data
                                   , family = "poisson")

modelsummary(list("Half Mile" = reg_halfmile_no_controls,
                  "Quarter Mile" = reg_quartmile_no_controls,
                  "Tenth Mile" = reg_tenthmile_no_controls),
             stars = TRUE,
             title = "Table 1: Distance Gradient Effects (No Controls)",
             output = "output/westside_paper/regressions/hway_regs/table_1_distance_gradient.html")

# ======================================================================================
# TABLE 2: Proximity with Black Residency Control
# ======================================================================================
# Finding: "Controlling only for the share of black residency, we still see some 
#          effect of highway proximity"

cat("\n=== TABLE 2: Proximity with Black Residency Control ===\n")

reg_halfmile_black <- feglm(homicides ~ flag_constr:flag_halfmile_hway + share_black
                            | year + GRID_ID
                            , data = reg_data
                            , family = "poisson")

reg_quartmile_black <- feglm(homicides ~ flag_constr:flag_quartmile_hway + share_black
                             | year + GRID_ID
                             , data = reg_data
                             , family = "poisson")

reg_tenthmile_black <- feglm(homicides ~ flag_constr:flag_tenthmile_hway + share_black
                             | year + GRID_ID
                             , data = reg_data
                             , family = "poisson")

modelsummary(list("Half Mile" = reg_halfmile_black,
                  "Quarter Mile" = reg_quartmile_black,
                  "Tenth Mile" = reg_tenthmile_black),
             stars = TRUE,
             coef_map = c("flag_constr:flag_halfmile_hway",
                          "flag_constr:flag_quartmile_hway",
                          "flag_constr:flag_tenthmile_hway",
                          "share_black"),
             title = "Table 2: Proximity with Black Residency Control",
             output = "output/westside_paper/regressions/hway_regs/table_2_proximity_black_control.html")

# ======================================================================================
# TABLE 3: Full Controls Specification (1 Mile Buffer)
# ======================================================================================
# Finding: "Adding more controls, the effect persists (but standard error grows)"

cat("\n=== TABLE 3: Full Controls Specification ===\n")

reg_full_controls <- feglm(homicides ~ flag_constr * flag_tenthmile_hway 
                           + share_white + share_black + share_foreign 
                           + share_unemployed + share_college_grad 
                           + log(total_pop)
                           | year + GRID_ID
                           , data = reg_data %>% filter(flag_1mile_hway == 1) 
                           , family = "poisson")

modelsummary(list("Full Controls" = reg_full_controls),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", "flag_constr:flag_tenthmile_hway",
                          "share_white", "share_black", "share_foreign",
                          "share_unemployed", "share_college_grad", "log(total_pop)"),
             title = "Table 3: Full Controls Specification (1 Mile Buffer)",
             output = "output/westside_paper/regressions/hway_regs/table_3_full_controls.html")

# ======================================================================================
# TABLE 4: Housing Project Interactions
# ======================================================================================

cat("\n=== TABLE 4: Housing Project Interactions ===\n")

reg_housing_interact <- feglm(homicides ~ flag_constr * flag_tenthmile_hway 
                              + share_white + share_black + share_foreign 
                              + share_unemployed + share_college_grad
                              + has_housing * share_unemployed 
                              + has_housing * share_black
                              + log(total_pop)
                              | year + GRID_ID
                              , data = reg_data %>% filter(flag_1mile_hway == 1)
                              , family = "poisson")

modelsummary(list("Housing Interactions" = reg_housing_interact),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", "flag_constr:flag_tenthmile_hway",
                          "share_white", "share_black", "share_foreign",
                          "share_unemployed", "share_college_grad",
                          "has_housing", "has_housing:share_unemployed", "has_housing:share_black",
                          "log(total_pop)"),
             title = "Table 4: Housing Project Interactions",
             output = "output/westside_paper/regressions/hway_regs/table_4_housing_interactions.html")

# ======================================================================================
# TABLE 5: Vacant Homes Sensitivity
# ======================================================================================
# CRITICAL FINDING: "Adding vacant homes as an interactive control destroys the effect"

cat("\n=== TABLE 5: Vacant Homes Sensitivity ===\n")

reg_vacant_homes <- feglm(homicides ~ flag_constr * flag_tenthmile_hway 
                          + share_white + share_foreign 
                          + share_unemployed + share_college_grad 
                          + has_housing * share_black 
                          + has_housing * share_unemployed 
                          + share_vacant_homes * share_black
                          + log(total_pop)
                          | year + GRID_ID
                          , data = reg_data %>% filter(flag_1mile_hway == 1)
                          , family = "poisson")

reg_triple_vacant <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * share_vacant_homes
                           + share_white + share_foreign 
                           + share_unemployed + share_college_grad 
                           + has_housing * share_black 
                           + has_housing * share_unemployed 
                           + share_vacant_homes * share_black
                           + log(total_pop)
                           | year + GRID_ID
                           , data = reg_data %>% filter(flag_1mile_hway == 1)
                           , family = "poisson")

modelsummary(list("Vacant Homes Interactive" = reg_vacant_homes,
                  "Triple Interaction" = reg_triple_vacant),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", "flag_constr:flag_tenthmile_hway",
                          "share_vacant_homes", 
                          "flag_constr:share_vacant_homes",
                          "flag_tenthmile_hway:share_vacant_homes",
                          "flag_constr:flag_tenthmile_hway:share_vacant_homes",
                          "share_white", "share_foreign",
                          "share_unemployed", "share_college_grad",
                          "has_housing", "share_black",
                          "has_housing:share_black", "has_housing:share_unemployed",
                          "share_vacant_homes:share_black",
                          "log(total_pop)"),
             title = "Table 5: Vacant Homes Sensitivity Analysis",
             output = "output/westside_paper/regressions/hway_regs/table_5_vacant_homes_sensitivity.html")

# ======================================================================================
# TABLE 6: Comparison Across Control Groups - No Controls
# ======================================================================================

cat("\n=== TABLE 6: Comparison Across Control Groups - No Controls ===\n")

# Control Group 1: All of Chicago
reg_fe_chicago_no_controls <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                                    | GRID_ID + year
                                    , data = reg_data 
                                    , family = "poisson")

# Control Group 2: 1 mile from Eisenhower & 1 mile from Dan Ryan
reg_fe_danry_no_controls <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                                  | GRID_ID + year
                                  , data = reg_data %>% filter(flag_1mile_hway == 1 | flag_1mile_hway_danry == 1)
                                  , family = "poisson")

# Control Group 3: 1/10 mile from Dan Ryan
reg_fe_tenth_danry_no_controls <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                                        | GRID_ID + year
                                        , data = reg_data %>% filter(flag_tenthmile_hway == 1 | flag_tenthmile_hway_danry == 1)
                                        , family = "poisson")

# Control Group 4: 1 mile from Eisenhower
reg_fe_eis_no_controls <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                                | GRID_ID + year
                                , data = reg_data %>% filter(flag_1mile_hway == 1)
                                , family = "poisson")

modelsummary(list("Chicago" = reg_fe_chicago_no_controls, 
                  "Eis+Dan Ryan" = reg_fe_danry_no_controls, 
                  "Dan Ryan Only" = reg_fe_tenth_danry_no_controls, 
                  "Eisenhower Only" = reg_fe_eis_no_controls),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", "flag_black_50",
                          "flag_constr:flag_tenthmile_hway",
                          "flag_constr:flag_black_50",
                          "flag_tenthmile_hway:flag_black_50",
                          "flag_constr:flag_tenthmile_hway:flag_black_50"),
             title = "Table 6: Comparison Across Control Groups (No Controls)",
             output = "output/westside_paper/regressions/hway_regs/table_6_control_groups_no_controls.html")

# ======================================================================================
# TABLE 7: Comparison Across Control Groups - All Controls
# ======================================================================================

cat("\n=== TABLE 7: Comparison Across Control Groups - All Controls ===\n")

# Control Group 1: All of Chicago
reg_fe_chicago_all <- feglm(homicides ~ flag_constr * flag_tenthmile_hway 
                            + share_white + share_foreign 
                            + share_unemployed + share_college_grad 
                            + has_housing + share_vacant_homes 
                            + log(total_pop)
                            | GRID_ID + year
                            , data = reg_data 
                            , family = "poisson")

# Control Group 2: 1 mile from Eisenhower & 1 mile from Dan Ryan
reg_fe_danry_all <- feglm(homicides ~ flag_constr * flag_tenthmile_hway
                          + share_white + share_foreign 
                          + share_unemployed + share_college_grad 
                          + has_housing + share_vacant_homes 
                          + log(total_pop)
                          | GRID_ID + year
                          , data = reg_data %>% filter(flag_1mile_hway == 1 | flag_1mile_hway_danry == 1)
                          , family = "poisson")

# Control Group 3: 1/10 mile from Dan Ryan
reg_fe_tenth_danry_all <- feglm(homicides ~ flag_constr * flag_tenthmile_hway
                                + share_white + share_foreign 
                                + share_unemployed + share_college_grad 
                                + has_housing + share_vacant_homes 
                                + log(total_pop)
                                | GRID_ID + year
                                , data = reg_data %>% filter(flag_tenthmile_hway == 1 | flag_tenthmile_hway_danry == 1)
                                , family = "poisson")

# Control Group 4: 1 mile from Eisenhower
reg_fe_eis_all <- feglm(homicides ~ flag_constr * flag_tenthmile_hway
                        + share_white + share_foreign + 
                          share_unemployed + share_college_grad 
                        + has_housing + share_vacant_homes
                        + log(total_pop)
                        | GRID_ID + year
                        , data = reg_data %>% filter(flag_1mile_hway == 1)
                        , family = "poisson")

modelsummary(list("Chicago" = reg_fe_chicago_all, 
                  "Eis+Dan Ryan" = reg_fe_danry_all, 
                  "Dan Ryan Only" = reg_fe_tenth_danry_all, 
                  "Eisenhower Only" = reg_fe_eis_all),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", "flag_constr:flag_tenthmile_hway",
                          "share_white", "share_foreign",
                          "share_unemployed", "share_college_grad",
                          "has_housing", "share_vacant_homes",
                          "log(total_pop)"),
             title = "Table 7: Comparison Across Control Groups (All Controls)",
             output = "output/westside_paper/regressions/hway_regs/table_7_control_groups_all_controls.html")

# ======================================================================================
# TABLE 8: Comparison Across Control Groups - Black Residency Interaction
# ======================================================================================

cat("\n=== TABLE 8: Comparison Across Control Groups - Black Residency Interaction ===\n")

`# Control Group 1: All of Chicago
reg_fe_chicago_share_50 <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                                 + share_white + share_foreign 
                                 + share_unemployed + share_college_grad 
                                 + has_housing + share_vacant_homes 
                                 + log(total_pop)
                                 | GRID_ID + year
                                 , data = reg_data 
                                 , family = "poisson")

# Control Group 2: 1 mile from Eisenhower & 1 mile from Dan Ryan
reg_fe_danry <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                      + share_white + share_foreign 
                      + share_unemployed + share_college_grad 
                      + has_housing + share_vacant_homes 
                      + log(total_pop)
                      | GRID_ID + year
                      , data = reg_data %>% filter(flag_1mile_hway == 1 | flag_1mile_hway_danry == 1)
                      , family = "poisson")

# Control Group 3: 1/10 mile from Dan Ryan
reg_fe_tenth_danry <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                            + share_white + share_foreign 
                            + share_unemployed + share_college_grad 
                            + has_housing + share_vacant_homes 
                            + log(total_pop)
                            | GRID_ID + year
                            , data = reg_data %>% filter(flag_tenthmile_hway == 1 | flag_tenthmile_hway_danry == 1)
                            , family = "poisson")

# Control Group 4: 1 mile from Eisenhower
reg_fe_eis <- feglm(homicides ~ flag_constr * flag_tenthmile_hway * flag_black_50
                    + share_white + share_foreign + 
                      share_unemployed + share_college_grad 
                    + has_housing + share_vacant_homes
                    + log(total_pop)
                    | GRID_ID + year
                    , data = reg_data %>% filter(flag_1mile_hway == 1)
                    , family = "poisson")

modelsummary(list("Chicago" = reg_fe_chicago_share_50, 
                  "Eis+Dan Ryan" = reg_fe_danry, 
                  "Dan Ryan Only" = reg_fe_tenth_danry, 
                  "Eisenhower Only" = reg_fe_eis),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", "flag_black_50",
                          "flag_constr:flag_tenthmile_hway",
                          "flag_constr:flag_black_50",
                          "flag_tenthmile_hway:flag_black_50",
                          "flag_constr:flag_tenthmile_hway:flag_black_50",
                          "share_white", "share_foreign",
                          "share_unemployed", "share_college_grad",
                          "has_housing", "share_vacant_homes",
                          "log(total_pop)"),
             title = "Table 8: Comparison Across Control Groups (Black Residency Interaction)",
             output = "output/westside_paper/regressions/hway_regs/table_8_control_groups_black_interaction.html")
`
# ======================================================================================
# TABLE 9: Main Results Summary (All 12 Specifications)
# ======================================================================================
# Finding: "Effect is more pronounced when we compare it against all of Chicago"

cat("\n=== TABLE 9: Main Results Summary (All 12 Specifications) ===\n")

modelsummary(list("Chicago: No Controls" = reg_fe_chicago_no_controls, 
                  "Chicago: All Controls" = reg_fe_chicago_all, 
                  "Chicago: Black Interaction" = reg_fe_chicago_share_50, 
                  "Eis+Dan Ryan: No Controls" = reg_fe_danry_no_controls, 
                  "Eis+Dan Ryan: All Controls" = reg_fe_danry_all, 
                  "Eis+Dan Ryan: Black Interaction" = reg_fe_danry, 
                  "Dan Ryan: No Controls" = reg_fe_tenth_danry_no_controls, 
                  "Dan Ryan: All Controls" = reg_fe_tenth_danry_all, 
                  "Dan Ryan: Black Interaction" = reg_fe_tenth_danry, 
                  "Eisenhower: No Controls" = reg_fe_eis_no_controls, 
                  "Eisenhower: All Controls" = reg_fe_eis_all, 
                  "Eisenhower: Black Interaction" = reg_fe_eis), 
             stars = TRUE,
             title = "Table 9: Main Results Summary - All Control Groups and Specifications",
             output = "output/westside_paper/regressions/hway_regs/table_9_main_results_all_specifications.html")

# ======================================================================================
# OUTSTANDING QUESTIONS & NEXT STEPS
# ======================================================================================

cat("\n")
cat("======================================================================================\n")
cat("OUTSTANDING QUESTIONS (from researcher notes):\n")
cat("======================================================================================\n")
cat("\n")
cat("1. Temporal patterns:\n")
cat("   - What makes something a top increase or decrease?\n")
cat("   - Look into when which segment of Eisenhower was built\n")
cat("   - Investigate 1940, 1941, 1946 anomalies\n")
cat("\n")
cat("2. Measurement:\n")
cat("   - How do we account for fluctuation?\n")
cat("   - Should we use proportional vs. absolute measures?\n")
cat("   - Best approach to handle high-variance areas?\n")
cat("\n")
cat("3. Comparison groups:\n")
cat("   - How to make Eisenhower and Dan Ryan areas comparable?\n")
cat("   - Should treatment definition vary by highway?\n")
cat("   - Is citywide comparison appropriate?\n")
cat("\n")
cat("4. Mechanisms:\n")
cat("   - Why does vacant homes control eliminate effect?\n")
cat("   - Is vacancy a mediator or confounder?\n")
cat("   - Role of public housing construction?\n")
cat("   - Displacement pathways?\n")
cat("\n")
cat("5. Specification:\n")
cat("   - Should we use grid-specific time trends?\n")
cat("   - Alternative de-meaning strategies?\n")
cat("   - Tri-hexagon approach for border areas?\n")
cat("\n")
cat("======================================================================================\n")

cat("\n")
cat("All regression tables saved to output/eisenhower_regressions/\n")
cat("======================================================================================\n")

# ======================================================================================
# END OF ANALYSIS
# ======================================================================================