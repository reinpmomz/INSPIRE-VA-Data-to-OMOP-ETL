library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)

## Location CDM table Transformation

location_cdm_table <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  location_table <- df_final %>%
    dplyr::filter(site_name %in% hdss_id) %>%
    ##Distinct Location
    dplyr::distinct(location_name, residence, country) %>%
    ##Drop NA in location name
    tidyr::drop_na(location_name) %>%
    tidyr::pivot_longer(cols = country
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    ##Get ConceptID for Country
    dplyr::left_join(df_usagi_merge_approved
                     , by = c("value" = "sourceName", "name" = "ADD_INFO:variable_name")
                     ) %>%
    dplyr::mutate(latitude = if_else(location_name == "Harar", 9.3129314,
                                       if_else(location_name == "Haramaya", 9.3919283, 
                                               if_else(location_name == "Kersa", 9.2486416, 
                                                       if_else(location_name == "Iganga/Mayuge ", 0.5415745, 
                                                               if_else(location_name == "Karonga", -9.9458954, 
                                                                       if_else(location_name == "korogocho", -1.2524607, 
                                                                               if_else(location_name == "viwandani", -1.3066697, 
                                                                                       if_else(location_name == "Ouagadougou", 12.3587779,
                                                                                               if_else(location_name == "Niakhar", 14.4759704,
                                                                                                       as.numeric(location_name)
                                                                                                       )
                                                                                               )
                                                                                       )
                                                                               )
                                                                       )
                                                               )
                                                       )
                                               )
                                     ) #latitude
                  , longitude = if_else(location_name == "Harar", 39.8707564,
                                        if_else(location_name == "Haramaya", 42.0028566, 
                                                if_else(location_name == "Kersa", 41.6415642, 
                                                        if_else(location_name == "Iganga/Mayuge ", 33.3064786, 
                                                                if_else(location_name == "Karonga", 33.8749464, 
                                                                        if_else(location_name == "korogocho", 36.8791299, 
                                                                                if_else(location_name == "viwandani", 36.8662333, 
                                                                                        if_else(location_name == "Ouagadougou", -1.7016636,
                                                                                                if_else(location_name == "Niakhar", -16.4238188,
                                                                                                        as.numeric(location_name)
                                                                                                        )
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        )
                                                )
                                        ) #longitude
                  , latitude = round(latitude, 7)
                  , longitude = round(longitude, 7)
                  , city = NA
                  , state = NA
                  , zip = NA
                  , county = NA
                  , location_id = dplyr::row_number()
                  , location_source_value = location_name
                  ) %>%
    dplyr::rename(country_concept_id = conceptId
                  , country_source_value = value
                  , address_1 = location_name
                  , address_2 = residence
                  ) %>%
    dplyr::select(location_id, address_1, address_2 , city, state, zip, county, location_source_value,
                  country_concept_id, country_source_value, latitude, longitude)
    
}, simplify = FALSE
)

## Loading to CDM tables

location_cdm_load <- sapply(names(location_cdm_table), function(x){
  nn <- x
  
  interest_table <- "location"
  
  ## Inserting data to specific schema and table
   DBI::dbWriteTable(con
                     , name = Id(schema = nn, table = interest_table)
                     , value = location_cdm_table[[nn]]
                     , overwrite = TRUE
                     , row.names = FALSE
                     , field.types = c(location_id="integer", address_1= "character varying (50)", address_2="character varying (50)",
                                       city="character varying (50)", state="character varying (2)", zip="character varying (9)",
                                       county= "character varying (20)", location_source_value="character varying (50)",
                                       country_concept_id= "integer", country_source_value="character varying (80)",
                                       latitude="numeric", longitude="numeric"
                                       )
                      )
   
   ## CDM Primary Key Constraints for OMOP Common Data Model 5.4
   DBI::dbSendQuery(con, glue::glue("
    ALTER TABLE {nn}.location ADD CONSTRAINT xpk_location PRIMARY KEY (location_id);
                                                          ")
                    )
  
}, simplify = FALSE
)

