library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

## Observation CDM table Transformation

observation_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  ## Age and age group
  observation_table_a <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    dplyr::select(individual_id, age_at_death, va_date, age_group_at_death) %>%
    tidyr::drop_na(age_at_death) %>% 
    tidyr::pivot_longer(cols = age_group_at_death
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    ##Get ConceptID for age group
    dplyr::left_join(df_usagi_merge_approved
                     , by = c("value" = "sourceName", "name" = "ADD_INFO:variable_name")
                     ) %>%
    #observation concept_id for age at death
    dplyr::mutate(observation_concept_id = 3038421) %>%
    dplyr::rename(value_as_concept_id = conceptId
                  , value_as_string = value
                  , value_as_number = age_at_death
                  , observation_date = va_date
                  )
  
  ## level of education
  observation_table_b <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    dplyr::select(individual_id, education_level, va_date) %>%
    tidyr::drop_na(education_level) %>% 
    tidyr::pivot_longer(cols = education_level
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    ##Get ConceptID for education levels
    dplyr::left_join(df_usagi_merge_approved
                     , by = c("value" = "sourceName", "name" = "ADD_INFO:variable_name")
                     ) %>%
    #observation concept_id for education
    dplyr::mutate(observation_concept_id = 42528763) %>%
    dplyr::rename(value_as_concept_id = conceptId
                  , value_as_string = value
                  , observation_date = va_date
                  )
  
  ## socio-economic status
  observation_table_c <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    dplyr::select(individual_id, ses_quintile, va_date) %>%
    tidyr::drop_na(ses_quintile) %>% 
    tidyr::pivot_longer(cols = ses_quintile
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    ##Get ConceptID for socio-economic status
    dplyr::left_join(df_usagi_merge_approved
                     , by = c("value" = "sourceName", "name" = "ADD_INFO:variable_name")
                     ) %>%
    #observation concept_id for economic status
    dplyr::mutate(observation_concept_id = 4249447
                  , conceptId = ifelse(value %in% c("lower"), 4277050,
                                       ifelse(value %in% c("middle lower"), 4198183, 
                                              ifelse(value %in% c("middle"), 4330874, 
                                                     ifelse(value %in% c("middle upper"), 4274784,
                                                            ifelse(value %in% c("upper"), 4311552, conceptId
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                  ) %>%
    dplyr::rename(value_as_concept_id = conceptId
                  , value_as_string = value
                  , observation_date = va_date
                  )
  
  observation_table  <- dplyr::bind_rows(observation_table_a, observation_table_b, observation_table_c) %>%
    dplyr::inner_join( person_cdm_table[[nn]] %>%
                         dplyr::select(person_id, person_source_value, provider_id, care_site_id),
                       by = c("individual_id" = "person_source_value")
                       ) %>%
    dplyr::inner_join( visit_detail_cdm_table[[nn]] %>%
                         dplyr::select(visit_detail_id, person_id, visit_detail_start_date, provider_id, care_site_id,
                                       visit_occurrence_id),
                       by = c("person_id" = "person_id", "observation_date" = "visit_detail_start_date",
                              "provider_id" = "provider_id", "care_site_id" = "care_site_id"
                              )
                       ) %>%
    dplyr::arrange(person_id) %>%
    dplyr::mutate(observation_id = dplyr::row_number()
                  , observation_datetime = NA
                  , observation_type_concept_id = 32883
                  , qualifier_concept_id = 0
                  , unit_concept_id = 0
                  , observation_source_value = NA
                  , observation_source_concept_id = 0
                  , unit_source_value = NA
                  , qualifier_source_value = NA
                  , value_source_value = NA
                  , observation_event_id = NA
                  , obs_event_field_concept_id = 0
                  ) %>%
    dplyr::select(observation_id, person_id, observation_concept_id, observation_date, observation_datetime
                  , observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id
                  , qualifier_concept_id, unit_concept_id, provider_id, visit_occurrence_id, visit_detail_id
                  , observation_source_value, observation_source_concept_id, unit_source_value, qualifier_source_value
                  , value_source_value, observation_event_id, obs_event_field_concept_id
                  ) %>%
    dplyr::mutate(across(c(observation_source_value, value_source_value, qualifier_source_value), ~strtrim(.x, 49)
                         )
                  , value_as_string = strtrim(value_as_string, 59)
                  )
  
  }, simplify = FALSE
  )

 
## Loading to CDM tables
 
 observation_cdm_load <- sapply(names(observation_cdm_table), function(x){
   nn <- x 
   
   interest_table <- "observation"
   
   ## Inserting data to specific schema and table
     DBI::dbWriteTable(con
                       , name = Id(schema = nn, table = interest_table)
                       , value = observation_cdm_table[[nn]]
                       , overwrite = TRUE
                       , row.names = FALSE
                       , field.types = c(observation_id="integer", person_id="integer", observation_concept_id="integer"
                                         , observation_date="date", observation_datetime="timestamp without time zone"
                                         , observation_type_concept_id="integer", value_as_number="numeric", value_as_string="character varying (60)"
                                         , value_as_concept_id="integer", qualifier_concept_id="integer", unit_concept_id="integer", provider_id="integer"
                                         , visit_occurrence_id="integer", visit_detail_id="integer", observation_source_value="character varying (50)"
                                         , observation_source_concept_id="integer", unit_source_value="character varying (50)"
                                         , qualifier_source_value="character varying (50)", value_source_value="character varying (50)" 
                                         , observation_event_id="integer", obs_event_field_concept_id="integer"
                                         )
                       )
     
  ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
     DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.observation ADD CONSTRAINT xpk_observation PRIMARY KEY (observation_id);
                                                          ")
                      )
  
}, simplify = FALSE
)
 


