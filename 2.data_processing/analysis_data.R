library(dplyr)
library(tidyr)
library(labelled)
library(writexl)


## group variables 
### if empty vector use character()
analysis_vars_df <- selected_vars_df[selected_vars_df$select == "retain" & !is.na(selected_vars_df$select),]

## make dataset with variables for descriptive and inferential statistics
df_analysis <- df_final %>%
  dplyr::select(any_of(analysis_vars_df$new_variable)
                ) %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                ) 

analysis_report <- paste0(
  paste0(analysis_vars_df$new_variable,collapse=", ")," ", length(analysis_vars_df$new_variable)
  , " variables used for analysis" ,". ", nrow(df_final)-nrow(df_analysis) , 
  " rows omitted", " Final rows ", nrow(df_analysis) 
)

print(analysis_report)

none_analysis_report <- paste0(
  paste0(selected_vars_df$new_variable[selected_vars_df$select == "drop"],
         collapse=", ")," ", 
  length(selected_vars_df$new_variable[selected_vars_df$select == "drop"])
  , " variables not used for analysis"
)
print(none_analysis_report)

### creating data dictionary
analysis_attribute <- as.data.frame(labelled::generate_dictionary(df_analysis, labels = TRUE, values = TRUE)
                                    ) %>%
    dplyr::mutate(across(c(levels, value_labels), ~as.character(.x))
                  )

### Saving data dictionary
writexl::write_xlsx(analysis_attribute,
                    path = base::file.path(output_Dir, paste0("data_dictionary_analysis.xlsx") )
                    )

