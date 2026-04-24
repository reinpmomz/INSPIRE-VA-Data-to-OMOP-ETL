library(dplyr)
library(readr)
library(writexl)
library(tidyr)


working_directory

#summary of vocabs in OMOP-tables per study

summary_OMOP_vocabs_study <- sapply(ls(pattern = "_cdm_table$"), function(x){
  nn <- x
  tables <- get(x)
  studies <- names(tables)
  
  out <- sapply(studies, function(y){
     data <- tables[[y]] %>%
       dplyr::mutate(study = y
                     , across(contains("_concept_id"), ~as.integer(.))
                     ) %>%
       dplyr::select(contains(c("_concept_id", "study"))) %>%
       tidyr::pivot_longer(!study) %>%
       dplyr::distinct(value, .keep_all = TRUE)
      
  }, simplify = FALSE
    )
  
  out_ <- dplyr::bind_rows(out) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(table = nn)
  

}, simplify = FALSE
)

summary_OMOP_vocabs_study_merge <- dplyr::bind_rows(summary_OMOP_vocabs_study) %>%
  dplyr::left_join(vocabulary_tables_data[["concept"]]
                   ,by = c("value"="concept_id")
                   ) 


summary_OMOP_vocabs_study_unique <- summary_OMOP_vocabs_study_merge %>%
  dplyr::select(-c(name, table)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(vocabulary = if_else(vocabulary_id == "INSPIRE", "Local", "Standard")
                , across(c(domain_id, vocabulary, study), ~as.factor(.x))
                ) %>%
  dplyr::select(study, value, domain_id, vocabulary) %>%
  tidyr::pivot_wider(names_from = c(study, vocabulary)
                     , values_from = value
                     , values_fn = length
                     , values_fill = 0
                     , names_expand = TRUE
                     , names_sort = TRUE
                     )

## Save the output 
writexl::write_xlsx(list(OMOP_vocabs_study = summary_OMOP_vocabs_study_merge
                         , OMOP_vocabs_study_unique = summary_OMOP_vocabs_study_unique
                         ),
                    path = base::file.path(output_Dir, paste0("summary_OMOP_vocabs_study_merge.xlsx") )
                    )



