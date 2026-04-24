library(DatabaseConnector)
library(Achilles)

working_directory

#Automated Characterization of Health Information at Large-Scale Longitudinal Evidence Systems (ACHILLES) 
## Achilles provides descriptive statistics on an OMOP CDM database. ACHILLES currently supports CDM version 5.3 and 5.4.

## Create connection details
cd_achilles <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = paste0("localhost","/",database_name),
  user = "postgres",
  password = Sys.getenv("postgres_password"),
  port = 5432,
  extraSettings = "tcpKeepAlive=true",
  pathToDriver = base::file.path(data_Dir, "JDBC Driver postgresql")
  )


## Run Achilles
achilles_analysis <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)

  results_schema <- paste0(hdss_id,"_results")
  
  vocabulary_schema <- "vocabulary"
  
  #If name is too long,.txt file will fail to generate and show error
  source_name <- cdm_source_cdm_table[[nn]] %>%
    dplyr::pull(cdm_source_name) %>%
    as.character()
  
  output_folder <- base::file.path(Achilles_Analysis_Dir, nn) #create output folder for individual studies
  
  #Add the default error report logger
  #ParallelLogger::addDefaultErrorReportLogger(file.path(output_folder, "errorReportSql.txt"))
  
  options(rstudio.connectionObserver.errorsSuppressed = TRUE)
  
  #run achilles
  Achilles::achilles(connectionDetails = cd_achilles,
                     cdmDatabaseSchema = nn,
                     resultsDatabaseSchema = results_schema,  #no capital letters- brings issues with postgres
                     vocabDatabaseSchema = vocabulary_schema,
                     sourceName = source_name,
                     createTable = TRUE,
                     smallCellCount = 5,
                     cdmVersion = "5.4",
                     createIndices = TRUE,
                     numThreads = 1,
                     outputFolder = output_folder,
                     optimizeAtlasCache = FALSE
                     )
  
  
}, simplify = FALSE
)

