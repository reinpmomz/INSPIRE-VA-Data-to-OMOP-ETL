library(dplyr)
library(janitor)
library(readr)
library(tibble)
library(labelled)
library(writexl)

## Merging the data

df_clean_merged_list <- sapply(ls(pattern = "_clean$"), function(x){
  nn <- x
  df <- get(x)
  
}, simplify=FALSE)


### Comparison of dataframes to indicate whether they will successfully bind together by rows

df_comparison_rows_all <- janitor::compare_df_cols(df_clean_merged_list,
                                                   return = "all"
                                                   )

df_comparison_rows_match <- janitor::compare_df_cols(df_clean_merged_list,
                                                        return = "match"
                                                     )

df_comparison_rows_mismatch <- janitor::compare_df_cols(df_clean_merged_list,
                                                        return = "mismatch"
                                                        )

writexl::write_xlsx(list(comparison_all = df_comparison_rows_all,
                         comparison_match = df_comparison_rows_match,
                         comparison_mismatch = df_comparison_rows_mismatch
                         ),
                    path = base::file.path(output_Dir, paste0("bindrows_data_report.xlsx") )
                    )

### clean mismatch variables across the datasets from factor to character

df_match_merged_list <- sapply(names(df_clean_merged_list), function(x){
  nn <- x
  mismatch <- df_comparison_rows_mismatch %>%
    dplyr::pull(column_name)
  
  out <- df_clean_merged_list[[nn]] %>% 
    dplyr::mutate(across(c(any_of(mismatch)), ~as.character(.x)
                         )
                  )
    
}, simplify=FALSE)


