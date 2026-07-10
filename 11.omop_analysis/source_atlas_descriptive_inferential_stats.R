library(dplyr)
library(flextable)


my_gtsummary_theme

gtsummary_compact_theme

## Descriptive Inferential statistics

source_atlas_descriptive_inferential_stats <- 
  categorical_inferential_strata_table(df = df_source_atlas_merge,
                                       foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                       caption = "",
                                       strata_var = c("site_name"),
                                       by_vars = c("data_type") , 
                                       percent = "column",
                                       flex_table = TRUE,
                                       ci=FALSE,
                                       p_value = TRUE,
                                       par_test = FALSE,
                                       var_equal = FALSE
                                       )

print(source_atlas_descriptive_inferential_stats)


### Save the output

flextable::save_as_docx(values = source_atlas_descriptive_inferential_stats, 
                        path = base::file.path(output_Dir, "source_atlas_descriptive_inferential_stats.docx"),
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

