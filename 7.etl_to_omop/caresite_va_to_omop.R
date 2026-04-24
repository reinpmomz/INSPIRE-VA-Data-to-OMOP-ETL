library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)

## Care Site CDM table Transformation

caresite_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  caresite_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    ##Distinct Location, sitename
    dplyr::distinct(location_name, site_name) %>%
    ##Drop NA in location name
    tidyr::drop_na(location_name) %>%
    dplyr::inner_join(location_cdm_table[[nn]] %>%
                        dplyr::select(location_id, location_source_value),
                      by = c("location_name" = "location_source_value")
                      ) %>%
    dplyr::mutate(care_site_id = dplyr::row_number()
                  , place_of_service_concept_id = 581476 #Homevisit Changed from NA
                  , place_of_service_source_value = NA
                  ) %>%
    dplyr::rename(care_site_name = site_name
                  , care_site_source_value = location_name
                  ) %>%
    dplyr::select(care_site_id, care_site_name, place_of_service_concept_id, location_id, care_site_source_value,
                  place_of_service_source_value
                  ) %>%
    dplyr::mutate(care_site_source_value = strtrim(care_site_source_value, 49)
                  )
  
}, simplify = FALSE
)


## Loading to CDM tables

caresite_cdm_load <- sapply(names(caresite_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "care_site"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = caresite_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(care_site_id="integer", care_site_name= "character varying (255)",
                                        place_of_service_concept_id="integer", location_id="integer",
                                        care_site_source_value="character varying (50)", 
                                        place_of_service_source_value= "character varying (50)"
                                        )
                      )
    
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
    DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.care_site  ADD CONSTRAINT xpk_care_site PRIMARY KEY (care_site_id);
                                                          ")
                     )
  
}, simplify = FALSE
)
