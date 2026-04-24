library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

## Visit Detail CDM table Transformation

visit_detail_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  visit_detail_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    dplyr::inner_join( person_cdm_table[[nn]] %>%
                         dplyr::select(person_id, person_source_value, provider_id, care_site_id),
                       by = c("individual_id" = "person_source_value")
                       ) %>%
    dplyr::inner_join( visit_occurrence_cdm_table[[nn]] %>%
                         dplyr::select(visit_occurrence_id, person_id, visit_start_date, provider_id, care_site_id),
                       by = c("person_id" = "person_id", "va_date" = "visit_start_date",
                              "provider_id" = "provider_id", "care_site_id" = "care_site_id"
                              )
                       ) %>%
    dplyr::mutate(visit_detail_id = dplyr::row_number()
                  , visit_detail_concept_id = 38004193
                  , visit_detail_start_datetime = NA
                  , visit_detail_end_date = va_date
                  , visit_detail_end_datetime = NA
                  , visit_detail_type_concept_id = 32883
                  , visit_detail_source_value = "Verbal Autopsy Interview"
                  , visit_detail_source_concept_id = 0
                  , admitted_from_concept_id = 0
                  , admitted_from_source_value = NA
                  , discharged_to_concept_id = 0
                  , discharged_to_source_value = NA
                  , preceding_visit_detail_id = NA
                  , parent_visit_detail_id = NA
                  ) %>%
    dplyr::rename( visit_detail_start_date = va_date) %>%
    dplyr::select( visit_detail_id, person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_start_datetime,
                   visit_detail_end_date, visit_detail_end_datetime, visit_detail_type_concept_id, provider_id, care_site_id,
                   visit_detail_source_value, visit_detail_source_concept_id, admitted_from_concept_id,
                   admitted_from_source_value, discharged_to_source_value, discharged_to_concept_id,
                   preceding_visit_detail_id, parent_visit_detail_id, visit_occurrence_id
                   )
    
}, simplify = FALSE
)


## Loading to CDM tables

visit_detail_cdm_load <- sapply(names(visit_detail_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "visit_detail"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = visit_detail_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(visit_detail_id="integer", person_id= "integer", visit_detail_concept_id="integer",
                                        visit_detail_start_date="date", visit_detail_start_datetime="timestamp without time zone",
                                        visit_detail_end_date="date", visit_detail_end_datetime="timestamp without time zone",
                                        visit_detail_type_concept_id="integer", provider_id= "integer", care_site_id="integer",
                                        visit_detail_source_value= "character varying (50)", visit_detail_source_concept_id="integer",
                                        admitted_from_concept_id="integer", admitted_from_source_value="character varying (50)", 
                                        discharged_to_source_value="character varying (50)", discharged_to_concept_id="integer",
                                        preceding_visit_detail_id="integer", parent_visit_detail_id="integer", visit_occurrence_id="integer"
                                        )
                      )
    
    #to accomodate inspire concepts visit_detail_source_concept_id="bigint" supposed to be visit_detail_source_concept_id="integer"
    
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
    DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.visit_detail ADD CONSTRAINT xpk_visit_detail PRIMARY KEY (visit_detail_id);
                                                          ")
                     )
  
}, simplify = FALSE
)
