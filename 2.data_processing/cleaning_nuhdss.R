library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset 

df_nuhdss_clean <- df_list[["VA_NUHDSS - Kenya"]][["NUHDSS_Verbalautopsy_2002-2015.dta"]] %>%
  dplyr::mutate(across(where(is.factor), ~ forcats::fct_recode(.x, 
                                                               NULL = "missing:impute"
                                                               )
                       ) #recode levels to NULL
                , residence = "Urban"
                , vau_yeardeath = as.numeric(as.character(vau_yeardeath))
                , age_at_death = round(lubridate::time_length(difftime(vau_datedeath, vau_datebirth, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                #, vau_slumarea = forcats::fct_na_value_to_level(vau_slumarea, level = "viwandani")
                #, ses_quintile = forcats::fct_recode(vau_slumarea
                #                                     , "lower" = "korogocho" #rename factor level "korogocho"  to "lower"
                #                                     , "middle lower" = "viwandani" #rename factor level "viwandani"  to "middle lower"
                #                                     )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["nuhdss_hdss_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["nuhdss_hdss_rename_vars_df"]][names(new_hdss_labels[["nuhdss_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) %>%
  dplyr::select(any_of(select_common_vars_df$new_variable)
                )
  
