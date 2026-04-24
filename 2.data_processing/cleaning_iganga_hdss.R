library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

### Link data
df_iganga_link <- df_list[["VA_IgangaMayuge_HDSS - Uganda"]][["CODwithSES1.dta"]] %>%
  dplyr::arrange(birthdate, dodyear, deathdate, gender) 

### Main data
df_iganga_clean <- df_list[["VA_IgangaMayuge_HDSS - Uganda"]][["CoD3.dta"]] %>%
  dplyr::mutate(deathdate_new = if_else(is.na(deathdate), birthdate %m+% lubridate::years(ageyrs) , deathdate)
                , death_date = if_else(is.na(deathdate), 
                                       lubridate::ymd(paste0(dodyear, "-", lubridate::month(deathdate_new),
                                                     "-", lubridate::day(deathdate_new))
                                                   )
                                       , deathdate_new
                                       )
                , year_death = lubridate::year(death_date)
                , ageyrs_new = round(lubridate::time_length(difftime(death_date, birthdate, units = "auto")
                                                              , unit = "year"
                                                              ),0 #calculating age
                                       )
                , ageyrs_new = if_else(is.na(ageyrs_new), ageyrs, ageyrs_new)
                , ageyrs_new = if_else(ageyrs_new < 0, ageyrs, ageyrs_new)
                , birthdate_new = if_else(is.na(birthdate), death_date %m-% lubridate::years(ageyrs_new), birthdate)
                , dobyear = na_if(dobyear, "")
                , dobyear = na_if(dobyear, ".")
                , dobyear = ifelse(dobyear == "1031", "1931", dobyear)
                , dobyear_new = lubridate::year(death_date %m-% lubridate::years(ageyrs_new))
                , birthdate_final = lubridate::ymd(paste0(dobyear_new, "-", lubridate::month(birthdate_new),
                                                     "-", lubridate::day(birthdate_new))
                                                   )
                , birthdate_final = if_else(is.na(birthdate_final), birthdate, birthdate_final)
                , ageyrs_final = round(lubridate::time_length(difftime(death_date, birthdate_final, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , birth_date = if_else(ageyrs_final < 0, birthdate_new, birthdate_final)
                , age_at_death = round(lubridate::time_length(difftime(death_date, birth_date, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , location_name = "Iganga/Mayuge"
                , residence = NA_character_
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::arrange(birthdate, dodyear, deathdate, sex) %>%
  dplyr::mutate(ses_quintile = as.factor(df_iganga_link$quintile2)
                , ses_quintile = forcats::fct_recode(ses_quintile
                                                     , "lower" = "poorest" #rename factor level "poorest"  to "lower"
                                                     , "middle lower" = "poorer" #rename factor level "poorer"  to "middle lower"
                                                     , "middle" = "poor" #rename factor level "poor"  to "middle"
                                                     , "middle upper" = "less poor" #rename factor level "less poor"  to "middle upper"
                                                     , "upper" = "least poor" #rename factor level "least poor"  to "upper"
                                                     )
                , education_level = stringr::str_to_lower(df_iganga_link$educlevel)
                , education_level = factor(education_level, levels = c("none", "primary", "secondary", "higher")
                                           )
                , education_level = forcats::fct_recode(education_level
                                                        , "no education" = "none"
                                                        , "higher education" = "higher"
                                                        )
                , individual_id = as.character(row_number())
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["iganga_hdss_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["iganga_hdss_rename_vars_df"]][names(new_hdss_labels[["iganga_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) %>%
  dplyr::select(any_of(select_common_vars_df$new_variable)
                )
  
unique(df_iganga_link$educlevel)
