library(DatabaseConnector)
library(DataQualityDashboard)

working_directory

#DQD dashboard for the study data

#VIEWING CHECKS 
#To see description of checks using R, execute the command below:

checks <- DataQualityDashboard::listDqChecks(cdmVersion = "5.4") # Put the version of the CDM you are using

## Create connection details
cd_dqd <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = paste0("localhost","/",database_name),
  user = "postgres",
  password = Sys.getenv("postgres_password"),
  port = 5432,
  extraSettings = "tcpKeepAlive=true",
  pathToDriver = base::file.path(data_Dir, "JDBC Driver postgresql")
)


dqd_dashboard <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)
  
  #Create results schema objects
  results_schema <- paste0(hdss_id,"_results")
  
  vocabulary_schema <- "vocabulary"
  
  #If name is too long,.txt file will fail to generate and show error
  source_name <- cdm_source_cdm_table[[nn]] %>%
    dplyr::pull(cdm_source_name) %>%
    as.character()
  
  output_folder <- base::file.path(DQD_Dir, nn) #create output folder for individual studies
  
  output_file <- paste0(nn, "_results",".json")
  
  DataQualityDashboard::executeDqChecks(connectionDetails = cd_dqd,
                                        cdmDatabaseSchema = nn, # database schema name of the CDM
                                        resultsDatabaseSchema = results_schema, # database schema name of the results
                                        vocabDatabaseSchema = vocabulary_schema, #default is to set it as the cdmDatabaseSchema
                                        cdmSourceName = source_name, # a human readable name for your CDM source
                                        cdmVersion = "5.4", # the CDM version you are targeting. Currently supports 5.2, 5.3, and 5.4
                                        numThreads = 1, #determine how many threads (concurrent SQL sessions) to use
                                        sqlOnly = FALSE, # set to TRUE if you just want to get the SQL scripts and not actually run the queries
                                        sqlOnlyUnionCount = 1,
                                        sqlOnlyIncrementalInsert =  FALSE, # set to TRUE if you want the generated SQL queries to calculate DQD
                                        outputFolder = output_folder, #where should the results and logs go?
                                        outputFile = output_file,
                                        verboseMode = FALSE, # set to FALSE if you don't want the logs to be printed to the console
                                        writeToTable = TRUE, # set to FALSE if you want to skip writing to a SQL table in the results schema
                                        writeTableName = "dqdashboard_results", #The name of the results table. Default 'dqdashboard_results'
                                        writeToCsv = FALSE, # set to FALSE if you want to skip writing to csv file
                                        csvFile = "", # only needed if writeToCsv is set to TRUE
                                        checkLevels = c("TABLE", "FIELD", "CONCEPT"),
                                        tablesToExclude = c("CONCEPT", "VOCABULARY", "CONCEPT_ANCESTOR", "CONCEPT_RELATIONSHIP", "CONCEPT_CLASS"
                                                            , "CONCEPT_SYNONYM", "RELATIONSHIP", "DOMAIN", "DRUG_STRENGTH"
                                                            )
                                        )
  
  
}, simplify = FALSE
)



