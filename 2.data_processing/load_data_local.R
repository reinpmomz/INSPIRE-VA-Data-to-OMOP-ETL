library(dplyr)
library(haven)
library(janitor)
library(tidyr)
library(writexl)
library(labelled)
library(stringr)

working_directory

## Reading data from local folder

data_files <- list.files(path = data_Dir, pattern = "^VA_",  full.names = F)

df_list <- sapply(data_files, function(x){
  nn <- x
  folder_name <- gsub("VA_", "", nn)
  data_subDir <- file.path(data_Dir, nn)
  data_subfiles <- list.files(path = data_subDir, full.names = FALSE)
  
  out <- sapply(data_subfiles, function(y){
    
    df_raw <- haven::read_dta(file.path(data_subDir, y))
    
    df <- df_raw %>%
      janitor::clean_names() %>%
      dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                                  ) #converts only labelled columns to factors
                    , name = folder_name
                    ) %>% 
      tidyr::separate(name, into = c("site_name", "country"), sep = " - ", remove = TRUE) %>%
      dplyr::mutate(site_name = str_to_lower(site_name)) %>%
      labelled::set_variable_labels(site_name = 'Site Name'
                                    , country = "Country"
                                    )
    
  }, simplify=FALSE)
  
  out
  
}, simplify=FALSE)

## creating data dictionary

raw_attribute <- sapply(names(df_list), function(x){
  nn <- x
  list <- names(df_list[[nn]])
  
  out <- sapply(list, function(y) {
    
    df <- base::as.data.frame(labelled::look_for(df_list[[nn]][[y]], labels = TRUE, values = TRUE)) %>%
      dplyr::mutate(file = y
                    #, across(c(levels, value_labels), ~as.character(.x))
                    ) 
    #df <- base::as.data.frame(labelled::generate_dictionary(df_list[[nn]][[y]], labels = TRUE, values = TRUE))
    
  }, simplify=FALSE)
  
  out <- dplyr::bind_rows(out)
  
}, simplify=FALSE)
  


## Save raw dictionary

writexl::write_xlsx(raw_attribute,
                    path = base::file.path(output_Dir, paste0("raw_attributes_dictionary.xlsx") )
                    )

