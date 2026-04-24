library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

## CDM Source table Transformation

cdm_source_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  cdm_source_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    ##latest event date in the source data i.e death date
    dplyr::mutate(min_death_date = min(death_date, na.rm = TRUE)
                  , max_death_date = max(death_date, na.rm = TRUE)
                  ) %>%
    ##Distinct Site Name
    dplyr::distinct(site_name, min_death_date, max_death_date) %>%
    dplyr::mutate( cdm_holder = "APHRC"
                   , cdm_source_abbreviation = site_name
                   , cdm_version_concept_id = 756265
                   , vocabulary_version = "v5.0 27-AUG-25"
                   , cdm_version = "5.4"
                   , cdm_etl_reference = "https://github.com/APHRC-DSE/INSPIRE-VA-Data-to-OMOP-ETL"
                   , cdm_release_date = Sys.Date()
                   , source_documentation_reference = NA
                   , source_description = paste0("Verbal Autopsy Data from ", min_death_date, " to ", max_death_date)
                   ) %>%
    dplyr::rename( cdm_source_name = site_name
                   ,source_release_date = max_death_date
                   ) %>%
    dplyr::select( cdm_source_name, cdm_source_abbreviation, cdm_holder, source_description, source_documentation_reference
                   , cdm_etl_reference, source_release_date, cdm_release_date, cdm_version, cdm_version_concept_id
                   , vocabulary_version
                   )
  
}, simplify = FALSE
)



## Loading to CDM tables

cdm_source_cdm_load <- sapply(names(cdm_source_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "cdm_source"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = cdm_source_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(cdm_source_name="character varying (255)", cdm_source_abbreviation="character varying (25)"
                                        , cdm_holder="character varying (255)", source_description="text"
                                        , source_documentation_reference="character varying (255)", cdm_etl_reference="character varying (255)"
                                        , source_release_date="date", cdm_release_date="date", cdm_version="character varying (10)"
                                        , cdm_version_concept_id="integer", vocabulary_version="character varying (20)"
                                        )
                      )
  
}, simplify = FALSE
)
