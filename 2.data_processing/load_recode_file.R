library(dplyr)
library(readxl)
library(tibble)
library(stringr)

working_directory

## Reading the recode file sheet

recode_file <- read_excel_allsheets("./2.data_processing/verbal_autopsy_recode_file.xlsx")

study_details <- recode_file[["study"]]

hararge_hdss_rename_vars_df <- recode_file[["hararge_rename_vars"]] #df for renaming variable labels
meiru_hdss_rename_vars_df <- recode_file[["meiru_rename_vars"]] #df for renaming variable labels
nuhdss_hdss_rename_vars_df <- recode_file[["nuhdss_rename_vars"]] #df for renaming variable labels
iganga_hdss_rename_vars_df <- recode_file[["iganga_rename_vars"]] #df for renaming variable labels
ouagadougou_hdss_rename_vars_df <- recode_file[["ouagadougou_rename_vars"]] #df for renaming variable labels
niakhar_hdss_rename_vars_df <- recode_file[["niakhar_rename_vars"]] #df for renaming variable labels

select_common_vars_df <- recode_file[["select_common_vars"]] #df for selecting common variables

merged_common_rename_vars_df <- recode_file[["merged_common_rename_vars"]] #df for renaming variable labels

selected_vars_df <- recode_file[["selected_vars"]] #df for choosing variables for analysis and plots

drop_selected_vars_df <- recode_file[["drop_selected_vars"]] #df for dropping analysis variables not needed for modelling

causes_of_death_df <- recode_file[["causes_of_death_all"]] #df for collating causes of death


## Creating a named vector to quickly assign variable names and labels
rename_vars_df <- sapply(ls(pattern = "_hdss_rename_vars_df$"), function(x){
  nn <- x
  df_new <- get(x)
  
  out <- df_new %>%
    dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
  
}, simplify=FALSE)


new_hdss_var_names <-  sapply(names(rename_vars_df), function(x){ 
  out <- rename_vars_df[[x]] %>%
  dplyr::select(new_variable, new_names_janitor) %>%
  tibble::deframe()
  
}, simplify=FALSE)

new_hdss_labels <-  sapply(names(rename_vars_df), function(x){ 
  out <- rename_vars_df[[x]] %>%
  dplyr::select(new_variable, new_label) %>%
  tibble::deframe()
  
}, simplify=FALSE)

new_labels <- merged_common_rename_vars_df %>%
  dplyr::select(new_variable, new_label) %>%
  tidyr::drop_na() %>%
  tibble::deframe()

