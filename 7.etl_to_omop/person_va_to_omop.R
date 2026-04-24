library(RPostgres)
library(DBI)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

## Person CDM table Transformation

person_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  person_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    ##Distinct individual ID
    dplyr::distinct(individual_id, .keep_all = TRUE) %>%
    #If no year of birth is available all the person’s data should be dropped from the CDM instance.
    tidyr::drop_na(birth_date) %>% 
    tidyr::pivot_longer(cols = gender
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    ##Get ConceptID for Gender
    dplyr::left_join(df_usagi_merge_approved
                     , by = c("value" = "sourceName", "name" = "ADD_INFO:variable_name")
                     ) %>%
    dplyr::inner_join( caresite_cdm_table[[nn]] %>%
                         dplyr::select(care_site_id, location_id, care_site_source_value),
                       by = c("location_name" = "care_site_source_value")
                       ) %>%
    dplyr::inner_join( provider_cdm_table[[nn]] %>%
                         dplyr::select(care_site_id, provider_id),
                       by = c("care_site_id")
                       ) %>%
    dplyr::rename(gender_concept_id = conceptId
                  , gender_source_value = value
                  ) %>%
    #If only year of birth is given, use the 15th of June of that year.
    dplyr::mutate( person_id = dplyr::row_number()
                   , year_of_birth = lubridate::year(birth_date)
                   , month_of_birth = lubridate::month(birth_date)
                   , day_of_birth = lubridate::day(birth_date)
                   , birth_datetime = lubridate::as_datetime(birth_date, tz = "UTC")
                   , race_concept_id = 38003600
                   , ethnicity_concept_id = 1547281
                   , gender_source_concept_id = 0
                   , race_source_value = "African"
                   , race_source_concept_id = 0
                   , ethnicity_source_value = "Subsaharan Africa"
                   , ethnicity_source_concept_id = 0
                   , across(c(gender_concept_id), ~tidyr::replace_na(.x, 4214687))
                   ) %>%
    dplyr::rename(person_source_value = individual_id
                  ) %>%
    dplyr::select(person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth, birth_datetime, race_concept_id
                  , ethnicity_concept_id, location_id, provider_id, care_site_id, person_source_value, gender_source_value
                  , gender_source_concept_id, race_source_value, race_source_concept_id, ethnicity_source_value
                  , ethnicity_source_concept_id
                  )
  
}, simplify = FALSE
)


## Loading to CDM tables

person_cdm_load <- sapply(names(person_cdm_table), function(x){
  nn <- x 
  
  interest_table <- "person"
  
  ## Inserting data to specific schema and table
    DBI::dbWriteTable(con
                      , name = Id(schema = nn, table = interest_table)
                      , value = person_cdm_table[[nn]]
                      , overwrite = TRUE
                      , row.names = FALSE
                      , field.types = c(person_id="integer", gender_concept_id= "integer", year_of_birth="integer",
                                        month_of_birth="integer", day_of_birth="integer", birth_datetime="timestamp without time zone",
                                        race_concept_id="integer", ethnicity_concept_id= "integer", location_id="integer",
                                        provider_id= "integer", care_site_id="integer", person_source_value="character varying (50)",
                                        gender_source_value="character varying (50)", gender_source_concept_id="integer",
                                        race_source_value="character varying (50)", race_source_concept_id="integer",
                                        ethnicity_source_value="character varying (50)", ethnicity_source_concept_id="integer"
                                        )
                      )
    
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
    DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.person ADD CONSTRAINT xpk_person PRIMARY KEY (person_id);
                                                          ")
                     )
  
}, simplify = FALSE
)
