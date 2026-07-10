library(RPostgres)
library(DBI)

working_directory


## Set the search path to the staging schema

dbExecute(con, sprintf("SET search_path TO %s", vocabulary_schema_name))

list_vocabulary_tables <- DBI::dbListTables(con)

vocabulary_tables <- list_vocabulary_tables[list_vocabulary_tables %in% c("concept", "vocabulary", "domain", 
                                                                          "concept_class", "concept_synonym",
                                                                          "concept_relationship", "relationship",
                                                                          "concept_ancestor", "source_to_concept_map",
                                                                          "drug_strength")]

#list_staging_tables <- DBI::dbListObjects(con, DBI::Id(schema = staging_schema_name))

## Read tables in selected scheme
vocabulary_tables_data <- sapply(vocabulary_tables[vocabulary_tables %in% c("concept", "vocabulary")], function(x){
  nn <- x
  
  read_tables <- DBI::dbReadTable(con, nn)
    
  
}, simplify = FALSE
)


