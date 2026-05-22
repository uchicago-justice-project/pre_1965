rm(list = ls())
source("header.R")
library(modelsummary)
library(fixest)

census_walk <- st_read("data/mst/for_paper/reg_data.geojson") %>% 
  filter(!if_any(c(dis_idx, z_share_black, z_share_over_1_ppr, z_share_20_29, 
                   z_share_vacant, z_share_unemployed, z_share_college_grad, z_median_home_value, flag_twentiethmile_any_hp_open, 
                   flag_constr, flag_tenthmile_hway, total_pop), is.na))


reg5 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + dis_idx + flag_twentiethmile_any_hp_open + flag_constr:flag_tenthmile_hway
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx + flag_twentiethmile_any_hp_open + flag_constr:flag_tenthmile_hway
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list( 
                  "No Interactions" = reg5, 
                  "With Interactions" = reg6 )
             
             , stars = TRUE
             , coef_map = c("dis_idx", "z_share_black", "z_share_black:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "z_share_black:z_share_20_29"
                            , "z_share_black:z_share_over_1_ppr:z_share_20_29"
                            , "flag_twentiethmile_any_hp_open"
                            , "flag_constr:flag_tenthmile_hway"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value")
             , title = "Table 1: Interaction Regressions with Fixed Effects & Dissimilarity Index"
             , output = "output/westside_paper/regressions/table_1.html")


census_walk_reg <- census_walk[obs(reg6), ]


stats_z <- census_walk_reg %>% 
  st_drop_geometry() %>%
  summarize(across(c(homicides, dis_idx, z_share_black, z_share_over_1_ppr, z_share_20_29,
                        z_share_vacant, z_share_unemployed, z_share_college_grad, z_median_home_value), 
                   .fns = list(min = ~min(., na.rm = T), max = ~max(., na.rm = T), mean = ~mean(., na.rm = T), median = ~median(., na.rm = T), stddev = ~sd(., na.rm = T))))

write_csv(stats_z, "output/westside_paper/regressions/stats_z.csv")

stats <- census_walk_reg %>% 
  st_drop_geometry() %>%
  summarize(across(c(homicides, dis_idx, flag_twentiethmile_any_hp_open, flag_tenthmile_hway, flag_constr, share_black, share_over_1_ppr, share_20_29, 
                     share_vacant, share_unemployed, share_college_grad, median_home_value), 
                   .fns = list(min = ~min(., na.rm = T), max = ~max(., na.rm = T), mean = ~mean(., na.rm = T), median = ~median(., na.rm = T), stddev = ~sd(., na.rm = T))))
write_csv(stats, "output/westside_paper/regressions/stats.csv")


# anti <- anti_join(census_walk %>% st_drop_geometry(), census_walk_reg%>% st_drop_geometry()) %>% 
#   dplyr::select(GISJOIN_1970, year, homicides, dis_idx, flag_twentiethmile_any_hp_open, flag_constr, total_pop, 
#                 share_black, share_over_1_ppr, share_20_29, 
#                 share_vacant, share_unemployed, share_college_grad, median_home_value, z_share_black, z_share_over_1_ppr, z_share_20_29,
#                 z_share_vacant, z_share_unemployed, z_share_college_grad, z_median_home_value)
# 
# left <- census_walk_reg %>% 
#   filter(GISJOIN_1970 %in% (anti %>% distinct(GISJOIN_1970) %>% pull())) %>% 
#   dplyr::select(GISJOIN_1970, year, homicides, dis_idx, flag_twentiethmile_any_hp_open, flag_constr, total_pop, 
#                 share_black, share_over_1_ppr, share_20_29, 
#                 share_vacant, share_unemployed, share_college_grad, median_home_value, z_share_black, z_share_over_1_ppr, z_share_20_29,
#                 z_share_vacant, z_share_unemployed, z_share_college_grad, z_median_home_value)


# write_csv(anti, "output/westside_paper/regressions/anti.csv")


reg7 <- feglm(homicides ~ dis_idx*z_share_black*z_share_over_1_ppr*z_share_20_29
              + flag_twentiethmile_any_hp_open + flag_constr:flag_tenthmile_hway
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list( 
  "No Interactions" = reg5, 
  "With Interactions" = reg6,
  "Dissimilarity Interactions" = reg7)
  
  , stars = TRUE
  , coef_map = c("dis_idx"
                 , "z_share_black", "z_share_over_1_ppr", "z_share_20_29"
                 , "z_share_black:z_share_over_1_ppr"
                 , "z_share_over_1_ppr:z_share_20_29"
                 , "z_share_black:z_share_20_29"
                 , "z_share_black:z_share_over_1_ppr:z_share_20_29"
                 , "dis_idx:z_share_black"
                 , "dis_idx:z_share_over_1_ppr"
                 , "dis_idx:z_share_20_29"
                 , "dis_idx:z_share_black:z_share_over_1_ppr"
                 , "dis_idx:z_share_black:z_share_20_29"
                 , "dis_idx:z_share_over_1_ppr:z_share_20_29"
                 , "dis_idx:z_share_black:z_share_over_1_ppr:z_share_20_29"
                 , "z_lag_share_20_29"
                 , "flag_twentiethmile_any_hp_open"
                 , "flag_constr:flag_tenthmile_hway"
                 , "z_share_vacant", "z_share_unemployed" 
                 , "z_share_college_grad", "z_median_home_value")
  , title = "Table 1: Interaction Regressions with Fixed Effects & Dissimilarity Index"
  , output = "output/westside_paper/regressions/table_1_t.html")

                       