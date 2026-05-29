library(RPostgres)
library(DBI)

working_directory

# Create results schema for each study
create_network_results_schema_name <- sapply(list_va_hdss, function(x){
  nn <- x
  name <- paste0(nn, "_results_network")
  
  # Create a new schema
  query <- paste0("CREATE SCHEMA IF NOT EXISTS ", name, ";")
  
  # Execute the query
  out <- dbExecute(con, query)
  
  
}, simplify = FALSE
)


  
