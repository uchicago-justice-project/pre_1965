rm(list = ls())

library(modelsummary)
library(fixest)
library(webshot2)

census_walk <- sf::st_read("data/mst/for_paper/census_west_homs_geom.geojson")|> 
  dplyr::filter(year <= 1970)|> 
  dplyr::mutate(all_20_29 = sum(male_20_24 + male_25_29 + female_20_24 + female_25_29))|> 
  dplyr::arrange(year)|> 
  dplyr::group_by(GISJOIN_1970)|> 
  dplyr::mutate(lag_share_20_29 = dplyr::lag(share_20_29, 1))|>
  dplyr::ungroup()|>
  dplyr::group_by(year)|> 
  dplyr::mutate(pop_white = sum(white, na.rm = T), 
                pop_black = sum(black, na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::mutate(dis_idx = abs(black/pop_black - white/pop_white))|>
  dplyr::mutate(across(
    .cols = c(share_black, 
              share_over_1_ppr, 
              share_20_29, 
              lag_share_20_29,
              share_vacant, 
              share_unemployed, 
              share_college_grad, 
              median_home_value),   
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
    .names = "z_{.col}"         
  )) 

share_20_29_1940 <- census_walk|> 
  dplyr::filter(year == 1940)|>
  sf::st_drop_geometry()|> 
  dplyr::select(GISJOIN_1970, share_20_29_1940 = share_20_29)

census_walk <- census_walk|> 
  dplyr::left_join(share_20_29_1940)|> 
  dplyr::mutate(z_share_20_29_1940 = (share_20_29_1940 - mean(share_20_29_1940, na.rm = T) / sd(share_20_29_1940, na.rm = T)))

# Individual 

reg1 <- feglm(homicides ~ z_share_black
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ z_share_black
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ z_share_over_1_ppr
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ z_share_over_1_ppr
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg5 <- feglm(homicides ~ z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 


modelsummary(list("Black Share" = reg1, 
                  "Black Share w/ FE" = reg2, 
                  "PPR" = reg3, 
                  "PPR w/ FE" = reg4, 
                  "20-29" = reg5, 
                  "20-29 w/ FE" = reg6)
             , stars = TRUE
             , coef_map = c("z_share_black", "z_share_over_1_ppr", "z_share_20_29"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value") 
             , title = "Table 1: Individual Variable Regressions"
             , output = "output/westside_paper/regressions/table_1.html")


# Interaction Regressions
reg1 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29

              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29 
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 


reg5 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk 
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk 
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list("No Interactions No Lag" = reg1, 
                  "With Interactions No Lag" = reg2, 
                  "No Interactions w/ Lag" = reg3, 
                  "With Interactions w/ Lag" = reg4, 
                  "No Interactions w/ 1940 No Lag" = reg5, 
                  "With Interactions w/ 1940 No Lag" = reg6)
             
             , stars = TRUE
             , coef_map = c("z_share_black", "z_share_black:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "z_share_black:z_share_20_29"
                            , "z_share_black:z_share_over_1_ppr:z_share_20_29"
                            , "z_lag_share_20_29"
                              , "z_share_vacant", "z_share_unemployed" 
                              , "z_share_college_grad", "z_median_home_value")
             , title = "Table 2: Interaction Regressions (No Fixed Effects)"
             , output = "output/westside_paper/regressions/table_2.html")

# Interaction Regressions w/ Fixed Effects
reg1 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29 
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg5 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list("No Interactions No Lag" = reg1, 
                  "With Interactions No Lag" = reg2, 
                  "No Interactions w/ Lag" = reg3, 
                  "With Interactions w/ Lag" = reg4, 
                  "No Interactions w/1940 No Lag" = reg5, 
                   "With Interactions w/ 1940 No Lag" = reg6 )
             
             , stars = TRUE
             , coef_map = c("z_share_black", "z_share_black:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "z_share_black:z_share_20_29"
                            , "z_share_black:z_share_over_1_ppr:z_share_20_29"
                            , "z_lag_share_20_29"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value")
             , title = "Table 3: Interaction Regressions with Fixed Effects"
             , output = "output/westside_paper/regressions/table_3.html")


# Interaction Regressions w/ Dissimilarity Index
reg1 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + dis_idx
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29 
              + dis_idx
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 


reg5 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + dis_idx
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk 
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk 
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list("No Interactions No Lag" = reg1, 
                  "With Interactions No Lag" = reg2, 
                  "No Interactions w/ Lag" = reg3, 
                  "With Interactions w/ Lag" = reg4, 
                  "No Interactions w/ 1940 No Lag" = reg5, 
                  "With Interactions w/ 1940 No Lag" = reg6)
             
             , stars = TRUE
             , coef_map = c("z_share_black", "z_share_black:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "z_share_black:z_share_20_29"
                            , "z_share_black:z_share_over_1_ppr:z_share_20_29"
                            , "z_lag_share_20_29", "dis_idx"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value")
             , title = "Table 2: Interaction Regressions (No Fixed Effects)"
             , output = "output/westside_paper/regressions/table_4.html")

# Interaction Regressions w/ Fixed Effects
reg1 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + dis_idx
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29 
              + dis_idx
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg5 <- feglm(homicides ~ z_share_black + z_share_over_1_ppr + z_share_20_29
              + dis_idx
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ z_share_black*z_share_over_1_ppr*z_share_20_29
              + dis_idx
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list("No Interactions No Lag" = reg1, 
                  "With Interactions No Lag" = reg2, 
                  "No Interactions w/ Lag" = reg3, 
                  "With Interactions w/ Lag" = reg4, 
                  "No Interactions w/1940 No Lag" = reg5, 
                  "With Interactions w/ 1940 No Lag" = reg6 )
             
             , stars = TRUE
             , coef_map = c("z_share_black", "z_share_black:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "z_share_black:z_share_20_29"
                            , "z_share_black:z_share_over_1_ppr:z_share_20_29"
                            , "z_lag_share_20_29", "dis_idx"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value")
             , title = "Table 3: Interaction Regressions with Fixed Effects"
             , output = "output/westside_paper/regressions/table_5.html")


# Interaction Regressions w/ Dissimilarity Index No Share Black
reg1 <- feglm(homicides ~ dis_idx + z_share_over_1_ppr + z_share_20_29

              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ dis_idx*z_share_over_1_ppr*z_share_20_29

              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ dis_idx + z_share_over_1_ppr + z_share_20_29 

              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk %>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ dis_idx*z_share_over_1_ppr*z_share_20_29
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 


reg5 <- feglm(homicides ~ dis_idx + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk 
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ dis_idx*z_share_over_1_ppr*z_share_20_29
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              
              , family = poisson()
              , data = census_walk 
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list("No Interactions No Lag" = reg1, 
                  "With Interactions No Lag" = reg2, 
                  "No Interactions w/ Lag" = reg3, 
                  "With Interactions w/ Lag" = reg4, 
                  "No Interactions w/ 1940 No Lag" = reg5, 
                  "With Interactions w/ 1940 No Lag" = reg6)
             
             , stars = TRUE
             , coef_map = c("dis_idx", "dis_idx:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "dis_idx:z_share_20_29"
                            , "dis_idx:z_share_over_1_ppr:z_share_20_29"
                            , "z_lag_share_20_29"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value")
             , title = "Table 2: Interaction Regressions (No Fixed Effects)"
             , output = "output/westside_paper/regressions/table_6.html")

# Interaction Regressions w/ Fixed Effects
reg1 <- feglm(homicides ~ dis_idx + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg2 <- feglm(homicides ~ dis_idx*z_share_over_1_ppr*z_share_20_29
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg3 <- feglm(homicides ~ dis_idx + z_share_over_1_ppr + z_share_20_29 
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg4 <- feglm(homicides ~ dis_idx*z_share_over_1_ppr*z_share_20_29
              + z_lag_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk%>% dplyr::filter(year > 1940)
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg5 <- feglm(homicides ~ dis_idx + z_share_over_1_ppr + z_share_20_29
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

reg6 <- feglm(homicides ~ dis_idx*z_share_over_1_ppr*z_share_20_29
              
              + z_share_vacant + z_share_unemployed 
              + z_share_college_grad + z_median_home_value
              |  GISJOIN_1970 + year
              , family = poisson()
              , data = census_walk
              , offset = ~log(total_pop)
              , vcov = ~ GISJOIN_1970) 

modelsummary(list("No Interactions No Lag" = reg1, 
                  "With Interactions No Lag" = reg2, 
                  "No Interactions w/ Lag" = reg3, 
                  "With Interactions w/ Lag" = reg4, 
                  "No Interactions w/1940 No Lag" = reg5, 
                  "With Interactions w/ 1940 No Lag" = reg6 )
             
             , stars = TRUE
             , coef_map = c("dis_idx", "dis_idx:z_share_over_1_ppr"
                            , "z_share_over_1_ppr", "z_share_over_1_ppr:z_share_20_29"
                            , "z_share_20_29", "dis_idx:z_share_20_29"
                            , "dis_idx:z_share_over_1_ppr:z_share_20_29"
                            , "z_lag_share_20_29"
                            , "z_share_vacant", "z_share_unemployed" 
                            , "z_share_college_grad", "z_median_home_value")
             , title = "Table 3: Interaction Regressions with Fixed Effects"
             , output = "output/westside_paper/regressions/table_7.html")

