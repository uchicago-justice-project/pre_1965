rm(list = ls())

source("header.R")
folder <- "housing_projects/"
files <- list.files(paste0("data/raw/", folder))

file <- files[1]

df <- st_read(paste0("data/raw/", folder, file))

write_csv(df, paste0("data/intermediate/", folder, "cha_projects.csv"))
