library(dplyr)
library(stringr)
library(omopgenerics)
library(OmopSketch)
library(visOmopResults)
library(flextable)
library(officer)


working_directory 

#characterise the observation period table

observation_period_omopsketch <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
    
    nn <- x
    
    snapshot <- OmopSketch::summariseObservationPeriod(cdm = cdm_reference[[nn]]
                                                       , missingData = TRUE
                                                       , quality = TRUE
                                                       , byOrdinal = TRUE
                                                       , ageGroup = NULL
                                                       , sex = FALSE
                                                       , dateRange = NULL
                                                       )
  
}, simplify = FALSE
)

## Table
observation_period_omopsketch_table <- 
  sapply(names(observation_period_omopsketch), function(x){
    
    nn <- x
    
    table <- OmopSketch::tableObservationPeriod(result = observation_period_omopsketch[[nn]]
                                                , header = "cdm_name"
                                                , type = "flextable" #visOmopResults::tableType() for supported table types 
                                                                     #gt, flextable, tibble, datatable, reactible, tinytable
                                                
                                                )
  
}, simplify = FALSE
)

### save flextable output

flextable::save_as_docx(values = observation_period_omopsketch_table, 
                        path = base::file.path(OMOPSketch_Dir, "observation_period_omopsketch.docx"),
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

## Plot
observation_period_omopsketch_plot <- 
  sapply(names(observation_period_omopsketch), function(x){
    
    nn <- x
    
    plot <- OmopSketch::plotObservationPeriod(result = observation_period_omopsketch[[nn]]
                                              , variableName = "Number subjects" #"Number subjects", "Records per person",
                                                                                 #"Duration in days" or "Days to next observation period"
                                              , plotType = "barplot" #"barplot", "boxplot", "densityplot" or "cumulativeplot"
                                              , facet = NULL #visOmopResults::tidyColumns()
                                              , colour = "observation_period_ordinal" #visOmopResults::tidyColumns() 
                                                              #"cdm_name", "observation_period_ordinal", "variable_name",
                                                              # "variable_level", "mean", "sd", "min", "q05", "q25"
                                                              # "median", "q75", "q95", "max", "count", "density_x"
                                                              # "sex"(if sex TRUE in result)
                                                              # age_group" (if defined list in ageGroup)
                                                              # "density_y", "percentage", "na_count", "na_percentage"
                                                              # "zero_count", "zero_percentage"
                                              , style = NULL # visOmopResults::plotStyle() "darwin"  "default"
                                              , type = "ggplot" #visOmopResults::plotType() for supported plot types 
                                                                #"ggplot" "plotly"
                                              )
  
}, simplify = FALSE
)


### Combine the plots
observation_period_omopsketch_plot_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = observation_period_omopsketch_plot,
                    ncol = 2,
                    nrow = 3,
                    labels = stringr::str_to_upper(stringr::str_replace_all(names(observation_period_omopsketch_plot), "_cdm", "")
                                                   ),
                    hjust = -0.5,
                    vjust = 0.5,
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL),
                    legend = "bottom", 
                    common.legend = TRUE
                    ),
  top = "",
  right = NULL,
  left = "",
  bottom = NULL
  )

print(observation_period_omopsketch_plot_grid)

## save plot output
ggsave(plot=observation_period_omopsketch_plot_grid, height = 7.5, width = 16,
       filename = paste0("observation_period_omopsketch_plot",".png"),
       path = OMOPSketch_Dir, bg='white')

