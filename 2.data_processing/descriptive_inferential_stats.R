library(dplyr)


my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics

descriptive_stats <- 
  categorical_inferential_table(df = df_analysis,
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "",
                                by_vars = c("site_name") , 
                                percent = "column",
                                overall = TRUE,
                                flex_table = TRUE,
                                ci=FALSE,
                                p_value = FALSE,
                                include_vars = names(df_analysis)[!names(df_analysis) %in% c("birth_date", "death_date",
                                                                                             "residence", "country",
                                                                                             "va_date")]
                                )

print(descriptive_stats)

