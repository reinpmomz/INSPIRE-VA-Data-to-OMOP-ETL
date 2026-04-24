library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

df_ouagadougou_clean <- df_list[["VA_Ouagadougou_HDSS - Burkina Faso"]][["population_dynamics_Ouaga.dta"]] %>%
  dplyr::filter(event_code == "DTH") %>%
  dplyr::mutate(residence = "Urban"
                , location_name = "Ouagadougou"
                , id = as.character(id)
                , year_death = lubridate::year(event_date)
                , age_at_death = round(lubridate::time_length(difftime(event_date, do_b, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , causen = forcats::fct_recode(causen
                                               , "Autopsy not done" = "0" #rename factor level "0"  to "Autopsy not done"
                                               )
                #, ses_quintile = forcats::fct_recode(type_zone
                #                                     , "lower" = "Informal" #rename factor level "Informal"  to "lower"
                #                                     , "middle upper" = "Formal" #rename factor level "Formal"  to "middle upper"
                #                                     )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["ouagadougou_hdss_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["ouagadougou_hdss_rename_vars_df"]][names(new_hdss_labels[["ouagadougou_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) %>%
  dplyr::select(any_of(select_common_vars_df$new_variable)
                )
  
