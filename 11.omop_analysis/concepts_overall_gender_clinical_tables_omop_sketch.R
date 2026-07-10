library(dplyr)
library(omopgenerics)
library(OmopSketch)
library(visOmopResults)
library(flextable)
library(officer)


working_directory 

# Characterise concepts in the clinical tables
## "visit_occurrence", "visit_detail", "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure"
## "measurement", "observation", "death", "note", "specimen", "payer_plan_period", "drug_era", "dose_era", "condition_era"

concepts_overall_gender_clinical_tables_omopsketch <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
    
    nn <- x
    
    snapshot <- OmopSketch::summariseConceptIdCounts(cdm = cdm_reference[[nn]]
                                                     , omopTableName = c("death")
                                                     , countBy = "record" #record, person
                                                     , interval = "overall" #"years", "quarters", "months" or "overall"
                                                     , sex = TRUE
                                                     , ageGroup = NULL
                                                     , inObservation = FALSE
                                                     , sample = NULL
                                                     , dateRange = NULL
                                                     )
    
    table <- OmopSketch::tableTopConceptCounts(result = snapshot
                                               , top = top_concepts
                                               , countBy = "record"
                                               , type = "flextable" #visOmopResults::tableType() for supported table types 
                                                                    #gt, flextable, tibble, datatable, reactible, tinytable
                                               
                                               )
  
}, simplify = FALSE
)


## save flextable output

flextable::save_as_docx(values = concepts_overall_gender_clinical_tables_omopsketch, 
                        path = base::file.path(OMOPSketch_Dir, "concepts_overall_gender_clinical_tables_omopsketch.docx"),
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


