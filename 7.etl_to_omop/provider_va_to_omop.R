library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)

## Provider CDM table Transformation

provider_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  provider_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    ##Distinct Location, sitename
    dplyr::distinct(location_name, site_name) %>%
    ##Drop NA in location name
    tidyr::drop_na(location_name) %>%
    dplyr::right_join(caresite_cdm_table[[nn]] %>%
                        dplyr::select(care_site_id, care_site_name, care_site_source_value),
                      by = c("location_name" = "care_site_source_value")
                      ) %>%
    #dplyr::distinct(care_site_id, .keep_all = TRUE) %>%
    dplyr::mutate(provider_id = 1:n()
                  , provider_name = NA
                  , npi = NA
                  , dea = NA
                  , specialty_concept_id = 4210301
                  , year_of_birth = NA
                  , gender_concept_id = 0
                  , provider_source_value = NA
                  , specialty_source_value = "Verbal Autopsy"
                  , specialty_source_concept_id = 0
                  , gender_source_value = NA
                  , gender_source_concept_id = 0
                  ) %>%
    dplyr::select(provider_id, provider_name, npi, dea, specialty_concept_id, care_site_id, year_of_birth, gender_concept_id,
                  provider_source_value, specialty_source_value, specialty_source_concept_id, gender_source_value,
                  gender_source_concept_id
                  )
  
}, simplify = FALSE
)


## Loading to CDM tables

provider_cdm_load <- sapply(names(provider_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "provider"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = provider_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(provider_id="integer", provider_name= "character varying (255)", npi="character varying (20)",
                                        dea="character varying (20)", specialty_concept_id="integer", care_site_id="integer",
                                        year_of_birth="integer", gender_concept_id= "integer",
                                        provider_source_value="character varying (50)", specialty_source_value= "character varying (50)",
                                        specialty_source_concept_id="integer", gender_source_value="character varying (50)",
                                        gender_source_concept_id="integer"
                                        )
                      )
    
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
    DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.provider ADD CONSTRAINT xpk_provider PRIMARY KEY (provider_id);
                                                          ")
                     )
  
}, simplify = FALSE
)
