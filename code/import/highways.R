rm(list = ls())
source("header.R")

# Data Import ---------------------

streets <- st_read("data/raw/streets")
highway_cons <- read_csv("data/raw/highway_construction.csv")

# Selection -------------------------
select_hways <- streets %>% 
  filter(DEDICATED_ %in% c("ADLAI STEVENSON EXPY", 
                           "BISHOP FORD FREEWAY", 
                           "CHICAGO SKWY", 
                           "DAN RYAN EXPY", 
                           "EAST-WEST TLWY", 
                           "EDENS EXPY", 
                           "EISENHOWER EXPY", 
                           "KENNEDY EXPY", 
                           "NORTHWEST TLWY", 
                           "TRI-STATE TLWY")) 

merge <- select_hways %>% 
  left_join(highway_cons, by = c("DEDICATED_" = "name"))

# Save -------------------------------------

sf_object <- st_as_sf(merge, wkt = "geometry") 

st_write(sf_object,"data/intermediate/highways.gpkg", layer = "highways", append = FALSE)

# Testing ------------------------------

hex <- st_read("data/intermediate/homicide_hexagons/survivorship") %>% 
  filter(year == 1950)

ggplot() + 
  geom_sf(data = hex) +
  geom_sf(data = select_hways , aes(color = as.factor(DEDICATED_))) 


highways <- streets %>% 
  filter(  (TYPE %in% c(1, 2, 3, 4, 5, 6) & (is.na(HWYTYPE) | !is.na(HWYTYPE)) & (is.na(St_PreTyp) | !is.na(St_PreTyp))) |
             str_detect(HWYTYPE, "INTERSTATE") | 
             str_detect(St_PreTyp, "EXPRESSWAY") | 
             str_detect(St_PreTyp, "TOLLWAY") | 
             str_detect(St_PreTyp, "HIGHWAY"))

highways <- streets %>% 
  filter(  (TYPE %in% c(1, 2, 3, 4) ))

ggplot(data = highways %>% filter(DEDICATED_ == "I290EB")) + 
  geom_sf(aes(color = as.factor(TYPE)))


highways %>% 
  st_drop_geometry() %>% 
  distinct(TYPE, FULLSTNA, HWYTYPE, St_PosTyp) %>% 
  View()

highways %>% 
  st_drop_geometry() %>% 
  distinct(TYPE,HWYTYPE,  St_PosTyp, DEDICATED_) %>% 
  View()

colnames(streets)
