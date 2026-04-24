library(dplyr)
library(omopgenerics)
library(OmopSketch)
library(visOmopResults)
library(flextable)
library(officer)


working_directory 

#characterise the observation period table

observation_period_omopsketch <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
    
    nn <- x
    
    snapshot <- OmopSketch::summariseObservationPeriod(cdm = cdm_reference[[nn]]
                                                       , missingData = TRUE
                                                       , quality = TRUE
                                                       , byOrdinal = TRUE
                                                       , ageGroup = NULL
                                                       , sex = FALSE
                                                       , dateRange = NULL
                                                       )
    
    table <- OmopSketch::tableObservationPeriod(result = snapshot
                                                , header = "cdm_name"
                                                , type = "flextable" #visOmopResults::tableType() for supported table types 
                                                                     #gt, flextable, tibble, datatable, reactible, tinytable
                                                
                                                )
  
}, simplify = FALSE
)


## save flextable output

flextable::save_as_docx(values = observation_period_omopsketch, 
                        path = base::file.path(OMOPSketch_Dir, "observation_period_omopsketch.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          section_columns = NULL, #Use NULL (default value) for no content.
                          header_default = NULL, #Use NULL (default value) for no content.
                          header_even = NULL, #Use NULL (default value) for no content.
                          header_first = NULL, #Use NULL (default value) for no content.
                          footer_default = NULL, #Use NULL (default value) for no content.
                          footer_even = NULL, #Use NULL (default value) for no content.
                          footer_first = NULL #Use NULL (default value) for no content.
                          )
                        )

