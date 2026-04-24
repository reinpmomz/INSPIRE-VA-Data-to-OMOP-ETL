library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

df_niakhar_clean <- df_list[["VA_Niakhar_HDSS - Senegal"]][["Niakhar_DEATHS_final.dta"]] %>%
  dplyr::mutate(residence = "Rural"
                , location_name = "Niakhar"
                , dss = ifelse(is.na(dss), paste0(dplyr::row_number(),"-n") , dss)
                , dss = as.character(dss)
                , year_of_death = lubridate::year(dateofdeath)
                , age_at_death = round(lubridate::time_length(difftime(dateofdeath, dateofbirth, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::distinct(dss, .keep_all = TRUE) %>%
  dplyr::rename(any_of(new_hdss_var_names[["niakhar_hdss_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["niakhar_hdss_rename_vars_df"]][names(new_hdss_labels[["niakhar_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) %>%
  dplyr::select(any_of(select_common_vars_df$new_variable)
                )

unique(df_niakhar_clean$gender)  
