library(RPostgres)
library(DBI)

working_directory


# Create a new schema for each study
create_cdm_schema_name <- sapply(list_va_hdss, function(x){
  nn <- x
  name <- paste0(nn, "_cdm")
  
  # Create a new schema
  query <- paste0("CREATE SCHEMA IF NOT EXISTS ", name, ";")
  
  # Execute the query
  out <- dbExecute(con, query)
  
  
}, simplify = FALSE
)

# Create results schema for each study
create_results_schema_name <- sapply(list_va_hdss, function(x){
  nn <- x
  name <- paste0(nn, "_results")
  
  # Create a new schema
  query <- paste0("CREATE SCHEMA IF NOT EXISTS ", name, ";")
  
  # Execute the query
  out <- dbExecute(con, query)
  
  
}, simplify = FALSE
)

#Create a single vocabulary schema
create_vocabulary_schema_name <- dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS vocabulary;")
                                           )

  
