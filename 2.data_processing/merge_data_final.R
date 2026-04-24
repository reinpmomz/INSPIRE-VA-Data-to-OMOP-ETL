library(dplyr)
library(forcats)
library(haven)
library(stringr)
library(janitor)
library(labelled)
library(writexl)
library(readr)

## Merging to get one dataset

### Check if output is true
janitor::compare_df_cols_same(df_match_merged_list
                                   )

# Set a seed (any integer works) to lock the random numbers
set.seed(42) 

df_final <- dplyr::bind_rows( df_match_merged_list
                                     ) %>%
  dplyr::mutate(across(c(gender, residence, year_death), ~as.factor(.x))
                , across(c(site_name, country), ~forcats::as_factor(.x))
                , gender = forcats::fct_collapse(gender
                                                 , "Female" = c("Female", "F", "female", "Féminin", "2"
                                                                ) # collapse levels to "Female"
                                                 , "Male" = c("Male", "M", "male", "Masculin", "1", "m"
                                                              ) #collapse levels to "Male"
                                                 )
                , gender = forcats::fct_recode(gender
                                               , NULL = "-1"
                                               , NULL = ""
                                               )
                , cause_of_death = stringr::str_to_lower(cause_of_death)
                , va_date = if_else(is.na(va_date) & !cause_of_death %in% c("va not done", "autopsy not done")
                                    , death_date + lubridate::days(sample(25:35, n(), replace = TRUE)), va_date
                                    )
                , age_group_at_death = if_else(age_at_death < 5 , "Under 5",
                                               if_else(age_at_death < 15 , "5-14",
                                                       if_else(age_at_death < 25 , "15-24",
                                                               if_else(age_at_death < 40 , "25-39",
                                                                       if_else(age_at_death < 65 , "40-64", "65 and above"
                                                                               )
                                                                       )
                                                               )
                                                       )
                                               )
                , age_group_at_death = factor(age_group_at_death, levels = c("Under 5", "5-14", "15-24",
                                                                             "25-39", "40-64", "65 and above")
                                              )
                ) %>%
  dplyr::left_join(causes_of_death_df %>%
                     dplyr::mutate(cause_of_death = stringr::str_to_lower(cause_of_death)) %>%
                     dplyr::select(cause_of_death, cause_of_death_new, general_cause_of_death) %>%
                     dplyr::distinct()
                   , by = c("cause_of_death")
                   ) %>%
  labelled::set_variable_labels( #creating labels for new variables
    age_group_at_death = "Age group at Death (Years)",
    cause_of_death_new = "Cause of death",
    general_cause_of_death = "General Cause of death"
    ) %>%
  labelled::set_variable_labels(!!!new_labels[names(new_labels) %in% names(.)]
                                ) #labeling variables from data dictionary

### creating data dictionary
attribute <- as.data.frame(labelled::generate_dictionary(df_final, labels = TRUE, values = TRUE)
                           )

### Saving data dictionary
writexl::write_xlsx(attribute,
                    path = base::file.path(output_Dir, paste0("data_dictionary_merged_final.xlsx") )
                    )

## saving merged dataset
# haven::write_dta(data= df_final, 
#                  path = base::file.path(output_Dir, "VA_INSPIRE_merged_final.dta")
#                  )

# haven::write_sav(data= df_final, 
#                  path = base::file.path(output_Dir, "VA_INSPIRE_merged_final.sav")
#                  )

### csv file
readr::write_csv(x = df_final %>% dplyr::select(-c(cause_of_death)),
                 file = base::file.path(output_Dir, "VA_INSPIRE_merged_final.csv"),
                 na = ""
                 )



