rm(list = ls())

source("header.R")
folder <- "racial_violence/"
files <- list.files(paste0("data/raw/", folder))

file <- files[1]

cha_projects <- st_read(paste0("data/raw/", folder, file))

write_csv(cha_projects, paste0("data/intermediate/", folder, "riots.csv"))
