# Note: I seem to have lost the "highway_construction.csv" file. This was a file that indicated the year a highway was built. 

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


