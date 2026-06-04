
rm(list = ls())
source("header.R")
library(modelsummary)
library(fixest)

# same as 7a but yearly

census_walk <- st_read("data/mst/for_paper/reg_data_yearly.geojson")

# ---- Change this line to switch buffer distance ----
flag_col <- "flag_twentiethmile_any_hp_open"
# flag_col <- "flag_tenthmile_any_hp_open"
# flag_col <- "flag_quartermile_any_hp_open"
# flag_col <- "flag_halfmile_any_hp_open"

df <- census_walk %>%
  group_by(GISJOIN_1970) %>%
  mutate(
    project_open_year = min(year[.data[[flag_col]] == 1], na.rm = TRUE),
    time_to_treat = ifelse((year - project_open_year) == -Inf, 1000, year - project_open_year)
  ) %>%
  ungroup()


#Highway

ev <- feglm(
  homicides ~ i(year, flag_tenthmile_hway, ref=1950) 
  | GISJOIN_1970 + year,
  data = census_walk ,
  family = "poisson",
  cluster = ~GISJOIN_1970
)

summary(ev)
iplot(ev, 
      main = "Effect of Eisenhower Construction on Homicide Count\nin Nearby Census Tracts (No Controls)", 
      xlab = "Decade")



#Housing Project
ev <- feglm(
  homicides ~ i(time_to_treat, ref = 0) 
  | GISJOIN_1970 + year ,
  data = df%>% filter(abs(time_to_treat) < 11),
  family = "poisson",
  cluster = ~GISJOIN_1970
)


summary(ev)
iplot(ev,
      drop = "1000", 
      main = "Effect of Public Housing Construction on Homicide Count\nin Nearby Census Tracts (No Controls)", 
      xlab = "Years from First Housing Project Opening in Given Tract")


