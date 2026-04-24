library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

## Visit Occurrence CDM table Transformation

visit_occurrence_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  visit_occurrence_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    tidyr::drop_na(va_date) %>% #If no va_date is available, the record should be dropped from the CDM instance.
    dplyr::inner_join( person_cdm_table[[nn]] %>%
                         dplyr::select(person_id, person_source_value, provider_id, care_site_id),
                       by = c("individual_id" = "person_source_value")
                       ) %>%
    dplyr::mutate(visit_occurrence_id = dplyr::row_number()
                  , visit_concept_id = 38004193
                  , visit_type_concept_id = 32883
                  , visit_start_datetime = NA
                  , visit_end_date = va_date
                  , visit_end_datetime = NA
                  , visit_source_value = "Verbal Autopsy Interview"
                  , visit_source_concept_id = 0
                  , admitted_from_concept_id = 0
                  , admitted_from_source_value = NA
                  , discharged_to_concept_id = 0
                  , discharged_to_source_value = NA
                  , preceding_visit_occurrence_id = NA
                  ) %>%
    dplyr::rename( visit_start_date = va_date) %>%
    dplyr::select( visit_occurrence_id, person_id, visit_concept_id, visit_start_date, visit_start_datetime,
                   visit_end_date, visit_end_datetime, visit_type_concept_id, provider_id, care_site_id,
                   visit_source_value, visit_source_concept_id, admitted_from_concept_id, admitted_from_source_value
                   , discharged_to_concept_id, discharged_to_source_value, preceding_visit_occurrence_id
                   )
  
}, simplify = FALSE
)


## Loading to CDM tables

visit_occurrence_cdm_load <- sapply(names(visit_occurrence_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "visit_occurrence"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = visit_occurrence_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(visit_occurrence_id="integer", person_id= "integer", visit_concept_id="integer",
                                        visit_start_date="date", visit_start_datetime="timestamp without time zone",
                                        visit_end_date="date", visit_end_datetime="timestamp without time zone",
                                        visit_type_concept_id="integer", provider_id= "integer", care_site_id="integer",
                                        visit_source_value= "character varying (50)", visit_source_concept_id="integer",
                                        admitted_from_concept_id="integer", admitted_from_source_value="character varying (50)", 
                                        discharged_to_concept_id="integer", discharged_to_source_value="character varying (50)",
                                        preceding_visit_occurrence_id="integer"
                                        )
                      )
    
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
    DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.visit_occurrence ADD CONSTRAINT xpk_visit_occurrence PRIMARY KEY (visit_occurrence_id);
                                                          ")
                     )
  
}, simplify = FALSE
)
