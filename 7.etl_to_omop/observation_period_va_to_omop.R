library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

## Observation Period CDM table Transformation

observation_period_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  observation_period_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    dplyr::inner_join( person_cdm_table[[nn]] %>%
                         dplyr::select( person_id, person_source_value),
                       by = c("individual_id"= "person_source_value")
                       ) %>%
    tidyr::drop_na(va_date) %>% #If no va_date is available, the record should be dropped from the CDM instance.
    tidyr::drop_na(death_date) %>% #If no death date is available, the record should be dropped from the CDM instance.
    dplyr::mutate(observation_period_id = dplyr::row_number()
                  , period_type_concept_id = 32883
                  ) %>%
    dplyr::rename( observation_period_start_date = death_date
                   , observation_period_end_date = va_date
                   ) %>%
    dplyr::select( observation_period_id, person_id, observation_period_start_date, observation_period_end_date,
                   period_type_concept_id
                   )
    
}, simplify = FALSE
)


## Loading to CDM tables

observation_period_cdm_load <- sapply(names(observation_period_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "observation_period"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = observation_period_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(observation_period_id="integer", person_id= "integer", 
                                        observation_period_start_date="date", observation_period_end_date="date",
                                        period_type_concept_id="integer"
                                        )
                      )
    
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
    DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.observation_period ADD CONSTRAINT xpk_observation_period PRIMARY KEY (observation_period_id);
                                                          ")
                     )
  
}, simplify = FALSE
)
