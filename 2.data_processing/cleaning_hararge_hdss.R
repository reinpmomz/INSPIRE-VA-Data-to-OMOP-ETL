library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

df_hararge_clean <- df_list[["VA_Hararghe_HDSS - Ethiopia"]][["VA_Data_2015_2023_Final_Final2.dta"]] %>%
  dplyr::mutate(dob = as.Date("1960-01-01") + months(dob, abbreviate = FALSE) #Stata, a %tm variable is a monthly date
                , death_date = as.Date("1960-01-01") + months(death_date, abbreviate = FALSE) #Stata, a %tm variable is a monthly date
                , age_at_death = round(lubridate::time_length(difftime(death_date, dob, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , site = ifelse(site == "HU_HU", "Haramaya",
                                     ifelse(site == "H", "Harar",
                                            ifelse(site == "K", "Kersa", site
                                                   )
                                            )
                                     )
                , residence = ifelse(site == "Haramaya", "Peri-Urban",
                                     ifelse(site == "Harar", "Urban",
                                            ifelse(site == "Kersa", "Rural", site
                                                   )
                                            )
                                     )
                , cause1 = ifelse(cause1 == "", "va not done", cause1)
                , weath_sc = forcats::fct_recode(weath_sc
                                                 , "lower" = "poorest" #rename factor level "poorest"  to "lower"
                                                 , "middle lower" = "poor" #rename factor level "poor"  to "middle lower"
                                                 , "middle upper" = "rich" #rename factor level "rich"  to "middle upper"
                                                 , "upper" = "richest" #rename factor level "richest"  to "upper"
                                                 )
                , recent_highest_gr = ifelse(educ_statu %in% c("Neither read nor write") & recent_highest_gr == 99, 0,
                                             ifelse(educ_statu %in% c("Neither read nor write") & is.na(recent_highest_gr), 0, recent_highest_gr
                                                    )
                                             )
                , education_level = ifelse(recent_highest_gr %in% c(0), "no education",
                                           ifelse(recent_highest_gr %in% c(1,2,3,4,5,6,7,8), "primary",
                                                  ifelse(recent_highest_gr %in% c(9,10,11,12), "secondary",
                                                         ifelse(recent_highest_gr %in% c(13,14,15,16,17,18,19,20), "higher education",
                                                                recent_highest_gr
                                                                )
                                                         )
                                                  )
                                           )
                , education_level = ifelse(is.na(education_level) & educ_statu %in% c("Neither read nor write"), "no education",
                                           ifelse(is.na(education_level) & educ_statu %in% c("Read only"), "primary",
                                                  ifelse(is.na(education_level) & educ_statu %in% c("Can read and write"), "secondary",
                                                         ifelse(is.na(education_level) & educ_statu %in% c("Literate"), "higher education",
                                                                education_level
                                                                )
                                                         )
                                                  )
                                           )
                , education_level = ifelse(education_level %in% c(99) & educ_statu %in% c("Neither read nor write"), "no education",
                                           ifelse(education_level %in% c(99) & educ_statu %in% c("Read only"), "primary",
                                                  ifelse(education_level %in% c(99) & educ_statu %in% c("Can read and write"), "secondary",
                                                         ifelse(education_level %in% c(99) & educ_statu %in% c("Literate"), "higher education",
                                                                education_level
                                                                )
                                                         )
                                                  )
                                           )
                , education_level = ifelse(is.na(education_level) & age_at_death < 4, "no education", education_level)
                , education_level = ifelse(age_at_death < 4 & education_level %in% c("primary", "secondary", "higher education"), "no education",
                                           ifelse(dplyr::between(age_at_death, 4, 14) & education_level %in% c("secondary", "higher education"), "primary",
                                                  ifelse(dplyr::between(age_at_death, 15, 18) & education_level %in% c("higher education"), "secondary",
                                                         education_level
                                                         )))
                , education_level = factor(education_level, levels = c("no education", "primary", "secondary", "higher education")
                                           )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["hararge_hdss_rename_vars_df"]]) #rename variable names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["hararge_hdss_rename_vars_df"]][names(new_hdss_labels[["hararge_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) %>%
  dplyr::select(any_of(select_common_vars_df$new_variable)
                )


