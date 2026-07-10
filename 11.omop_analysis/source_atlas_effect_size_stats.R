library(dplyr)
library(forcats)
library(writexl)


working_directory

#

source_atlas_effect_size_stats <- sapply(as.character(unique(df_source_atlas_merge$site_name)), function(x){
    
    nn <- x
    
    out <- effectsize_corr_table(df = df_source_atlas_merge %>%
                                   dplyr::filter(site_name == nn) %>%
                                   dplyr::mutate(across(where(is.factor),  ~fct_drop(.x )) #drop unused factor level
                                                 ),
                                 by_vars = c("data_type"),
                                 par_effsize = TRUE,
                                 var_equal = TRUE
                                 )
    
    out_ <- dplyr::bind_rows(out)
    
  }, simplify = FALSE
  )

## Saving effect size stats Output  

writexl::write_xlsx(source_atlas_effect_size_stats,
                    path = base::file.path(output_Dir, "source_atlas_effect_size_stats.xlsx" )
                    )
