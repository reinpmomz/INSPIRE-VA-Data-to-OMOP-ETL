library(RPostgres)
library(DBI)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

## Death CDM table Transformation

death_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  death_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    tidyr::pivot_longer(cols = cause_of_death_new
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    ##Get ConceptID for Causes of death
    dplyr::left_join(df_usagi_merge_approved
                     , by = c("value" = "sourceName", "name" = "ADD_INFO:variable_name")
                     ) %>%
    dplyr::inner_join( person_cdm_table[[nn]] %>%
                         dplyr::select(person_id, person_source_value),
                       by = c("individual_id" = "person_source_value")
                       ) %>%
    dplyr::mutate( death_datetime = lubridate::as_datetime(death_date, tz = "UTC")
                   , death_type_concept_id = 32880
                   , cause_source_concept_id = 0
                   ) %>%
    dplyr::rename(cause_concept_id = conceptId
                  , cause_source_value = value
                  ) %>%
    tidyr::drop_na(death_date) %>% #If no death date is available, the record should be dropped from the CDM instance.
    dplyr::select(person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value,
                  cause_source_concept_id	
                  ) %>%
    dplyr::mutate(cause_source_value = strtrim(cause_source_value, 49)
                  )
  
}, simplify = FALSE
)


## Loading to CDM tables

death_cdm_load <- sapply(names(death_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "death"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = death_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(person_id="integer", death_date="date", death_datetime="timestamp without time zone",
                                        death_type_concept_id="integer", cause_concept_id= "integer", 
                                        cause_source_value="character varying (50)", cause_source_concept_id="integer"
                                        )
                      )
    
}, simplify = FALSE
)
