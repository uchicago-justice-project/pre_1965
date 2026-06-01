
rm(list = ls())

library(modelsummary)
library(fixest)

# Test regressions runs for robustness



reg_halfmile_no_controls <- feglm(homicides ~ flag_constr:flag_halfmile_hway
                                  | year + GISJOIN_1970
                                  , data = reg_data
                                  , family = "poisson"
                                  , vcov = ~ GISJOIN_1970
                                  , offset = ~log(total_pop))

reg_quartmile_no_controls <- feglm(homicides ~ flag_constr:flag_quartmile_hway
                                   | year + GISJOIN_1970
                                   , data = reg_data
                                   , family = "poisson"
                                   , vcov = ~ GISJOIN_1970
                                   , offset = ~log(total_pop))

reg_tenthmile_no_controls <- feglm(homicides ~ flag_constr:flag_tenthmile_hway
                                   | year + GISJOIN_1970
                                   , data = reg_data
                                   , family = "poisson"
                                   , vcov = ~ GISJOIN_1970
                                   , offset = ~log(total_pop))

modelsummary(list("Half Mile" = reg_halfmile_no_controls,
                  "Quarter Mile" = reg_quartmile_no_controls,
                  "Tenth Mile" = reg_tenthmile_no_controls),
             stars = TRUE,
             title = "Table 1: Distance Gradient Effects (No Controls)",
             output = "output/westside_paper/regressions/hway_regs/table_1_distance_gradient.html")


reg_halfmile_black <- feglm(homicides ~ flag_constr:flag_halfmile_hway + share_black
                            | year + GISJOIN_1970
                            , data = reg_data
                            , family = "poisson"
                            , vcov = ~ GISJOIN_1970
                            , offset = ~log(total_pop))

reg_quartmile_black <- feglm(homicides ~ flag_constr:flag_quartmile_hway + share_black
                             | year + GISJOIN_1970
                             , data = reg_data
                             , family = "poisson"
                             , vcov = ~ GISJOIN_1970
                             , offset = ~log(total_pop))

reg_tenthmile_black <- feglm(homicides ~ flag_constr:flag_tenthmile_hway + share_black
                             | year + GISJOIN_1970
                             , data = reg_data
                             , family = "poisson"
                             , vcov = ~ GISJOIN_1970
                             , offset = ~log(total_pop))

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

# Control Group 1: All of West Side
reg_fe_chicago_share_50 <- feglm(homicides ~ flag_constr * flag_tenthmile_hway  + share_black
                                 + share_white + share_foreign_born + 
                                   share_unemployed + share_college_grad 
                                 + median_home_value + share_vacant
                                 
                                 | GISJOIN_1970 + year
                                 , data = reg_data 
                                 , family = "poisson"
                                 , vcov = ~ GISJOIN_1970
                                 , offset = ~log(total_pop))


# Control Group 2: 1 mile from Eisenhower
reg_fe_eis <- feglm(homicides ~ flag_constr * flag_tenthmile_hway  + share_black
                    + share_white + share_foreign_born + 
                      share_unemployed + share_college_grad 
                    + median_home_value + share_vacant
                    
                    | GISJOIN_1970 + year
                    , data = reg_data %>% filter(flag_1mile_hway == 1)
                    , family = "poisson"
                    , vcov = ~ GISJOIN_1970
                    , offset = ~log(total_pop))

modelsummary(list("West Side" = reg_fe_chicago_share_50, 
                  "1 Mile Radius Eisenhower Only" = reg_fe_eis),
             stars = TRUE,
             coef_map = c("flag_constr", "flag_tenthmile_hway", 
                          "flag_constr:flag_tenthmile_hway",
                          "share_black", 
                          "share_white", "share_foreign_born",
                          "share_unemployed", "share_college_grad",
                          "median_home_value", "share_vacant"),
             title = "Table 3: Full Control Variables (Two Control Groups)",
             output = "output/westside_paper/regressions/hway_regs/table_3_all_controls_control_groups.html")


