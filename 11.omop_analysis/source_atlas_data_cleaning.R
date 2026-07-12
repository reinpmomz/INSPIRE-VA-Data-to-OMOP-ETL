library(dplyr)
library(tidyr)


working_directory

#cleaning

df_source <- df_final %>%
  dplyr::select(any_of(source_atlas_vars_df$new_variable)) %>%
  dplyr::mutate(data_type = "Source"
                ,data_type = as.factor(data_type)
                )

df_atlas <- sapply(as.character(unique(df_final$site_name)), function(x){
  nn <- x
  df <- df_final %>%
    dplyr::filter(site_name %in% nn)
  
  person <- df %>%
    tidyr::drop_na(birth_date, location_name) %>%
    dplyr::select(individual_id, gender, va_done)
  
  death <- person %>%
    dplyr::left_join(df %>%
                       tidyr::drop_na(death_date) %>%
                       dplyr::filter(va_done %in% c("Yes")) %>%
                       dplyr::select(individual_id, cause_of_death_new)
                     , by = c("individual_id")
                     )
  
  observation <- death %>%
    dplyr::left_join(df %>%
                       dplyr::filter(va_done %in% c("Yes")) %>%
                       dplyr::select(individual_id, ses_quintile, education_level, age_at_death, age_group_at_death)
                     , by = c("individual_id")
                     )
  
  out <- observation %>%
    dplyr::mutate(site_name = nn
                  )
  
  return(out)
  
}, simplify = FALSE
) %>%
  dplyr::bind_rows() %>%
  dplyr::select(any_of(source_atlas_vars_df$new_variable)) %>%
  dplyr::mutate(data_type = "Atlas"
                ,data_type = as.factor(data_type)
                , site_name = as.factor(site_name)
                )


## Merge datasets
df_source_atlas_merge <- dplyr::bind_rows(df_source, df_atlas) 

