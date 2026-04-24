library(dplyr)
library(omopgenerics)


working_directory

#cdm reference object

cdm_reference <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
    
    nn <- x
    #If name is too long,.txt file will fail to generate and show error
    source_name <- cdm_source_cdm_table[[nn]] %>%
      dplyr::pull(cdm_source_name) %>%
      as.character()
    
    cdm <- omopgenerics::cdmFromTables(
      tables = list("person" = person_cdm_table[[nn]]
                    , "observation_period" = observation_period_cdm_table[[nn]]
                    , "visit_occurrence" = visit_occurrence_cdm_table[[nn]]
                    , "visit_detail" = visit_detail_cdm_table[[nn]]
                    , "observation" = observation_cdm_table[[nn]]
                    , "death" = death_cdm_table[[nn]]
                    , "location" = location_cdm_table[[nn]]
                    , "care_site" = caresite_cdm_table[[nn]]
                    , "provider" = provider_cdm_table[[nn]]
                    , "cdm_source" = cdm_source_cdm_table[[nn]]
                    , "concept" = vocabulary_tables_data[["concept"]]
                    , "vocabulary" = vocabulary_tables_data[["vocabulary"]]
                    ),
      cdmName = source_name
      )
  
  
}, simplify = FALSE
)
