library(dplyr)
library(omopgenerics)
library(OmopSketch)
library(visOmopResults)
library(flextable)
library(officer)


working_directory 

#Snapshot

snapshot_omopsketch <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
    
    nn <- x
    
    snapshot <- OmopSketch::summariseOmopSnapshot(cdm = cdm_reference[[nn]])
    table <- OmopSketch::tableOmopSnapshot(result = snapshot
                                           , header = "cdm_name"
                                           , hide = "variable_level"
                                           , groupColumn = "variable_name"
                                           , type = "flextable" #visOmopResults::tableType() for supported table types 
                                                                #gt, flextable, tibble, datatable, reactible, tinytable
                                           
                                           )
  
  
}, simplify = FALSE
)

## save flextable output

flextable::save_as_docx(values = snapshot_omopsketch, 
                        path = base::file.path(OMOPSketch_Dir, "snapshot_omopsketch.docx"),
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
