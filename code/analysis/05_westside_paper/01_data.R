rm(list = ls())
source("header.R")

#Loads all raw data and processes data into intermediate or master files 
#Data loaded here is how data was provided to me


#Homicide data to 65 ------------------
homs <- st_read("data/raw/homicides_1940-1965_geocoded_v3") %>% 
  mutate(year = as.integer(year), 
         date_clean = as_date(ifelse(date == "NaT", paste0(year, "-07-01"), date)), 
         category = paste0(tolower(cod), ": ", format(date_clean, "%m/%d"))) %>% 
  filter(!st_is_empty(geometry)) 

homs_clean <- homs %>% 
  dplyr::select(year) 


#Homicide data post 65 --------------

#for CRS
grid <- st_read("data/raw/Fishnet_InsideChicago_150m")

post_65 <- read_csv("data/raw/IndividualHomicides_1965_2022.csv") %>% 
  filter(!is.na(X_MetCent) & !is.na(Y_MetCent))
post_65 <- st_as_sf(post_65, coords = c("X_MetCent", "Y_MetCent"), crs = st_crs(grid))
post_65 <- st_transform(post_65, crs = st_crs(homs)) 

post_65_clean <- post_65 %>% 
  dplyr::select(year)

##combine ----------

homs_all <- rbind(homs_clean, post_65_clean) %>% 
  filter(year < 1975)

# Census --------------

census_walk <- read_sf("data/mst/chicago_cenus_tract_data_crosswalked.geojson")%>% 
  dplyr::select(geometry, GISJOIN_1970, year)
census_walk <- st_transform(census_walk, st_crs(homs))

census_walk_data  <- read.csv("data/mst/census_data_crosswalked_1970_tracts.csv") %>% 
  rename(GISJOIN_1970 = ID)

geom_lookup <- census_walk %>% 
  group_by(GISJOIN_1970) %>% 
  mutate(n = n()) %>% 
  filter(n < 7, year == 1960) %>% 
  mutate(year = 1970) %>% 
  dplyr::select(-n)

census_walk <- census_walk %>% 
  rbind(geom_lookup) %>%
  left_join(census_walk_data) 
census_walk %>% filter(is.na(total_pop)) %>% View()


#Neighborhoods --------------------
westside <- read_sf("data/intermediate/westside.geojson")
westside <- st_transform(westside, st_crs(homs))

#Projects ------------------------------
cha_projects <- read_sf("data/raw/housing_projects/housing_v3") 
cha_projects <- st_transform(cha_projects, st_crs(homs))


#Streets ------------------------------------
streets <- st_read("data/raw/streets")
highway_cons <- read_csv("data/raw/highway_construction.csv")

hways <- streets %>% 
  filter(DEDICATED_ %in% c("EISENHOWER EXPY")) %>% 
  st_union() %>% 
  st_cast("LINESTRING")

lines_sf <- st_sf(geometry = hways)
lines_sf <- st_transform(lines_sf, st_crs(homs)) 


#Create west side data --------------------
cha_projects_west <- st_filter(cha_projects, westside)
select_hways_west<- st_intersection(lines_sf, westside)
census_west <- st_filter(census_walk, westside)

homs_west <- st_filter(homs_all, census_west) %>% 
  mutate(year_decade = floor((year + 5) / 10) * 10) %>%
  group_by(year_decade) %>% 
  mutate(years = n_distinct(year)) %>%
  ungroup()

homs_not_west <- homs_all %>% 
  filter(lengths(st_intersects(., census_west)) == 0) %>% 
  mutate(year_decade = floor((year + 5) / 10) * 10) %>%
  group_by(year_decade) %>% 
  mutate(years = n_distinct(year)) %>%
  ungroup()

census_west_outline <-census_west  %>%
  st_union() %>%        # dissolve all tracts into one geometry
  st_cast("POLYGON")

census_west_homs <- homs_west %>% 
  dplyr::select(year_decade, years) %>%
  st_join(census_west , join = st_within) %>% 
  st_drop_geometry() %>% 
  filter(year == year_decade) %>% 
  dplyr::select(-year_decade) %>% 
  group_by(GISJOIN_1970, year) %>% 
  mutate(homicides = n() / years) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(GISJOIN_1970, year) %>% 
  dplyr::select(-years)

census_rest <- anti_join(census_west %>% st_drop_geometry(), census_west_homs %>% distinct(GISJOIN_1970, year)) %>%
  mutate(homicides = 0) 

census_west_homs_full <- rbind(census_west_homs, census_rest) 

census_west_homs_geom <- census_west %>%
  dplyr::select(GISJOIN_1970, year) %>% 
  left_join(census_west_homs_full) %>% 
  mutate(area = st_area(geometry)) %>%
  group_by(year) %>% 
  mutate(total_pop_west = sum(total_pop)) %>% 
  ungroup() %>%
  mutate(
    n_black_total = floor(black),
    n_pop_total = floor(total_pop),
    n_renter_occ_total = floor(renter_occupied_units), 
    n_ppr_total = floor(over_1_ppr), 
    n_20_29_total = floor(male_20_24 + male_25_29 + female_20_24 + female_25_29),
    n_15_19_total = floor((male_15_19 + female_15_19)), 
    n_male_15_19_total = floor((male_15_19)), 
    n_male_20_29_total = floor(male_20_24 + male_25_29), 
    n_15_24_total = floor((male_15_19 + female_15_19 + male_20_24 + female_20_24)), 
    n_black_share = floor(pmax(ifelse(is.na(share_black), 0, share_black*1000), 0)), 
    n_pop_share = floor(ifelse(is.na(total_pop), 0, (total_pop/total_pop_west)*1000)),
    n_renter_occ_share = floor(pmax(ifelse(is.na(share_renter_occupied), 0, share_renter_occupied*1000), 0)), 
    n_ppr_share = floor(pmax(ifelse(is.na(share_over_1_ppr), 0, share_over_1_ppr*1000), 0)), 
    n_20_29_share = floor(pmax(ifelse(is.na(share_20_29), 0, share_20_29*1000), 0)), 
    n_15_19_share = floor(pmax(ifelse(is.na((male_15_19 + female_15_19)/total_pop), 0, ((male_15_19 + female_15_19)/total_pop)*1000), 0)),
    n_male_15_19_share = floor(pmax(ifelse(is.na((male_15_19)/total_pop), 0, ((male_15_19)/total_pop)*1000), 0)),
    n_male_20_29_share = floor(pmax(ifelse(is.na((male_20_24 + male_25_29)/total_pop), 0, ((male_20_24 + male_25_29)/total_pop)*1000), 0)),
    n_15_24_share = floor(pmax(ifelse(is.na(male_15_19 + female_15_19 + male_20_24 + female_20_24), 0, 
                                      ((male_15_19 + female_15_19 + male_20_24 + female_20_24)/total_pop)*1000)))
  ) %>% 
  filter(year <= 1970)



#Census walk all Chicago with Westside indicator 

census_walk <- census_walk %>% 
  mutate(flag_west_side = ifelse(GISJOIN_1970 %in% 
                                   (census_west %>% distinct(GISJOIN_1970) %>% pull())
                                 , 1, 0)) %>%
  group_by(year) %>% 
  mutate(total_pop_west = sum(total_pop)) %>% 
  ungroup() %>%
  mutate(
    n_black_total = floor(black),
    n_pop_total = floor(total_pop),
    n_renter_occ_total = floor(renter_occupied_units), 
    n_ppr_total = floor(over_1_ppr), 
    n_20_29_total = floor(male_20_24 + male_25_29 + female_20_24 + female_25_29),
    n_15_19_total = floor((male_15_19 + female_15_19)), 
    n_male_15_19_total = floor((male_15_19)), 
    n_male_20_29_total = floor(male_20_24 + male_25_29), 
    n_15_24_total = floor((male_15_19 + female_15_19 + male_20_24 + female_20_24)), 
    n_black_share = floor(pmax(ifelse(is.na(share_black), 0, share_black*1000), 0)), 
    n_pop_share = floor(ifelse(is.na(total_pop), 0, (total_pop/total_pop_west)*1000)),
    n_renter_occ_share = floor(pmax(ifelse(is.na(share_renter_occupied), 0, share_renter_occupied*1000), 0)), 
    n_ppr_share = floor(pmax(ifelse(is.na(share_over_1_ppr), 0, share_over_1_ppr*1000), 0)), 
    n_20_29_share = floor(pmax(ifelse(is.na(share_20_29), 0, share_20_29*1000), 0)), 
    n_15_19_share = floor(pmax(ifelse(is.na((male_15_19 + female_15_19)/total_pop), 0, ((male_15_19 + female_15_19)/total_pop)*1000), 0)),
    n_male_15_19_share = floor(pmax(ifelse(is.na((male_15_19)/total_pop), 0, ((male_15_19)/total_pop)*1000), 0)),
    n_male_20_29_share = floor(pmax(ifelse(is.na((male_20_24 + male_25_29)/total_pop), 0, ((male_20_24 + male_25_29)/total_pop)*1000), 0)),
    n_15_24_share = floor(pmax(ifelse(is.na(male_15_19 + female_15_19 + male_20_24 + female_20_24), 0, 
                                      ((male_15_19 + female_15_19 + male_20_24 + female_20_24)/total_pop)*1000)))
  )%>% 
  filter(year <= 1970)

st_write(census_west_homs_geom, "data/mst/for_paper/census_west_homs_geom.geojson", append = FALSE)
st_write(census_west_outline, "data/mst/for_paper/census_west_outline.geojson", append = FALSE)
st_write(homs_west, "data/mst/for_paper/homs_west.geojson", append = FALSE )
st_write(homs_not_west, "data/mst/for_paper/homs_nonwest.geojson", append = FALSE )
st_write(census_walk, "data/mst/for_paper/census_walk_w_ws_flag.geojson", append = FALSE )
st_write(cha_projects_west, "data/mst/for_paper/projects.geojson", append = FALSE )
st_write(select_hways_west, "data/mst/for_paper/select_hways.geojson", append = FALSE)
