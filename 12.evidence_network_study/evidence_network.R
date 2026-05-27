library(DbDiagnostics)
library(DatabaseConnector)
library(Achilles)
library(DataQualityDashboard)

working_directory

#Designed to characterize the data sources in the network
##result in a resource to connect researchers with questions to data partner organizations who have data


## Create connection details
cd_evdnet <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = paste0("localhost","/",database_name),
  user = "postgres",
  password = Sys.getenv("postgres_password"),
  port = 5432,
  extraSettings = "tcpKeepAlive=true",
  pathToDriver = base::file.path(data_Dir, "JDBC Driver postgresql")
  )


## Run Evidence Network
evidence_network_study <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
  nn <- x
  hdss_id <- gsub("_cdm", "", nn)

  results_schema <- paste0(hdss_id,"_results")
  results_schema_network <- paste0(hdss_id,"_results_network")
  
  vocabulary_schema <- "vocabulary"
  
  #If name is too long,.txt file will fail to generate and show error
  source_name <- cdm_source_cdm_table[[nn]] %>%
    dplyr::pull(cdm_source_name) %>%
    as.character()
  
  output_folder <- base::file.path(EvidenceNetwork_Dir, nn) #create output folder for individual studies
  
  # Turn off the connection pane in environment settings to speed up run time
  #options(connectionObserver = NULL)
  
  #run Evidence Network 
  executeDbProfile_new(connectionDetails = cd_evdnet,
                                  cdmDatabaseSchema = nn,
                                  resultsDatabaseSchema = results_schema,  #no capital letters- brings issues with postgres
                                  writeTo = results_schema_network, #used to store any missing analyses that need to be run. Only set if appendAchilles = FALSE
                                  vocabDatabaseSchema = vocabulary_schema,
                                  cdmSourceName = source_name,
                                  siteName = "APHRC", #The name of the site or institution that owns or licenses the data.
                                  siteOHDSIParticipation = "No", #Yes/No if the site contributed to an OHDSI study in the past
                                  siteOHDSIRunPackage = "No", #Yes/No if site has someone who can run and/or debug an OHDSI study package
                                  siteSponsoredStudy = "Yes", #Yes/No if site is interested in participating in sponsored studies
                                  dataFullName = "Verbal Autopsy-INSPIRE Network", #The full name of the database
                                  dataShortName = "va_inspire", #	The short name or nickname of the database
                                  dataContactName = "Agnes Kiragga", #person who should be contacted in the event database is identified as a good candidate for a study
                                  dataContactEmail = "akiragga@aphrc.org", #email address of the person who should be contacted in the event database is identified as a good candidate for a study
                                  dataDoiType = "Other", #data object identifier (DOI) the database has. Options are "DOI","CURIE","ARK","Other"
                                  governanceTime = "4 weeks", #How long (in weeks) it typically takes to receive approval to run a study on this database
                                  dataProvenance = "Other", #type(s) of data in database. Options "Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other"
                                  refreshTime = "Yearly", #	 How often the data are refreshed
                                  outputFolder = output_folder, # The folder where your results should be written
                                  cdmVersion = "5.4", #The version of the OMOP CDM you are currently on. v5.3 and v5.4 are supported
                                  appendAchilles = FALSE, # Whether to append existing Achilles tables or create new ones
                                  minCellCount = 5, #Minimum cell count to allow in analyses. Default = 0
                                  roundTo = 10, # Whether to round to the 10s or 100s place. Valid inputs are 10 or 100, default is 10.
                                  excludedConcepts = c(),
                                  addDQD = FALSE, #Specify if DQD should be run. Default = TRUE
                                  tableCheckThresholds = "default",
                                  fieldCheckThresholds = "default",
                                  conceptCheckThresholds = "default"
                                  )
  
}, simplify = FALSE
)

