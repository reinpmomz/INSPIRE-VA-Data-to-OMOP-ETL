library(dplyr)
library(readr)
library(writexl)


working_directory

#summary of records in OMOP-tables

summary_OMOP_tables <- sapply(ls(pattern = "_cdm_table$"), function(x){
  nn <- x
  tables <- get(x)
  studies <- names(tables)
  
  out <- sapply(studies, function(y){
     data <- data.frame(study = y
                        ,n_row = nrow(tables[[y]])
                        )
      
  }, simplify = FALSE
    )
  
  out_ <- dplyr::bind_rows(out)
    

}, simplify = FALSE
)

## Save the output 

writexl::write_xlsx(summary_OMOP_tables,
                    path = base::file.path(output_Dir, paste0("summary_OMOP_tables.xlsx") )
                    )
