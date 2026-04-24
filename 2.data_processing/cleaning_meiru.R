library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

df_meiru_clean <- df_list[["VA_MEIRU - Malawi"]][["meiru_inspireAug23- version 2.dta"]] %>%
  tidyr::drop_na(insilico_cod1) %>%
  dplyr::mutate(assetscore = as.factor(assetscore)
                , across(where(is.factor), ~ forcats::fct_recode(.x, 
                                                               NULL = "Missing"
                                                               , NULL = "99"
                                                               )
                         ) #recode levels to NULL
                , residence = "Rural"
                , location_name = "Karonga"
                , year_death = lubridate::year(dod)
                , age_at_death = round(lubridate::time_length(difftime(dod, birth_date, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , ses_quintile = forcats::fct_collapse(dwellscore
                                                       , "lower" = c("1", "2") # collapse levels to "lower"
                                                       , "middle lower" = c("3", "4") #collapse levels to "middle lower"
                                                       , "middle" = c("5", "6") #collapse levels to "middle"
                                                       , "middle upper" = c("7", "8") #collapse levels to "middle upper"
                                                       , "upper" = c("9", "10") #collapse levels to "upper"
                                                       )
                , school = forcats::fct_collapse(school
                                                 , "no education" = c("None", "too young (u6)") # collapse levels to "no education"
                                                 , "primary" = c("1-3y primary", "4-7y primary",
                                                                 "Primary completed") #collapse levels to "primary"
                                                 , "secondary" = c("JCE completed", "MSCE completed") #collapse levels to "secondary"
                                                 , "higher education" = c("Tertiary") #collapse levels to "higher education"
                                                 )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["meiru_hdss_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["meiru_hdss_rename_vars_df"]][names(new_hdss_labels[["meiru_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) %>%
  dplyr::select(any_of(select_common_vars_df$new_variable)
                )
  
