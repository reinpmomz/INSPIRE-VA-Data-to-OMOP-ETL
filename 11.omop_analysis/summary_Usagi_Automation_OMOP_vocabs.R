library(dplyr)
library(writexl)
library(tidyr)


working_directory

#summary of Usagi Automation OMOP_vocabs

summary_Usagi_automation_OMOP_vocabs <- summary_OMOP_vocabs_merge_unique %>%
  dplyr::select(value, concept_name, domain_id, vocabulary) %>%
  dplyr::left_join(df_usagi_merge_approved %>%
                     dplyr::mutate(mapping = "Usagi"
                                   , createdBy = ifelse(createdBy != "<auto>", "Usagi Manual", createdBy)
                                   , createdBy = factor(createdBy, levels = c("<auto>", "Usagi Manual"))
                                   ) %>%
                     dplyr::group_by(createdBy) %>%
                     dplyr::arrange(conceptId, .by_group = TRUE) %>%
                     dplyr::ungroup() %>%
                     dplyr::distinct(conceptId, .keep_all = TRUE)
                   , by = c("value" = "conceptId")
                   ) %>%
  dplyr::mutate(mapping = tidyr::replace_na(mapping, "Athena")
                ,`ADD_INFO:variable_name` = tidyr::replace_na(`ADD_INFO:variable_name`, "concept_id")
                , createdBy = as.character(createdBy)
                , createdBy = tidyr::replace_na(createdBy, "Athena Search")
                )

## Save the output 

writexl::write_xlsx(list(summary_mapping = summary_Usagi_automation_OMOP_vocabs %>%
                           dplyr::add_count(name = "total") %>%
                           dplyr::group_by(mapping, total) %>%
                           dplyr::count() %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(prop = round(n*100/total, 1))
                         , summary_automation = summary_Usagi_automation_OMOP_vocabs %>%
                           dplyr::add_count(name = "total") %>%
                           dplyr::group_by(createdBy, total) %>%
                           dplyr::count() %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(prop = round(n*100/total, 1))
                         , summary_mapping_automation = summary_Usagi_automation_OMOP_vocabs %>%
                           dplyr::group_by(mapping) %>%
                           dplyr::add_count(name = "total") %>%
                           dplyr::ungroup() %>%
                           dplyr::group_by(mapping, total, createdBy) %>%
                           dplyr::count() %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(prop = round(n*100/total, 1))
                         , summary_variable_automation = summary_Usagi_automation_OMOP_vocabs %>%
                           dplyr::group_by(`ADD_INFO:variable_name`) %>%
                           dplyr::add_count(name = "total") %>%
                           dplyr::ungroup() %>%
                           dplyr::group_by(`ADD_INFO:variable_name`, total, createdBy) %>%
                           dplyr::count() %>%
                           dplyr::ungroup() %>%
                           dplyr::mutate(prop = round(n*100/total, 1))
                         , summary_Usagi_automation_list = summary_Usagi_automation_OMOP_vocabs
                         ),
                    path = base::file.path(output_Dir, paste0("summary_Usagi_automation_OMOP_vocabs.xlsx") )
                    )

