library(dplyr)
library(omopgenerics)
library(OmopSketch)
library(visOmopResults)
library(ggpubr)
library(ggplot2)


working_directory 

# trends over time for death table

death_agegroup_trend_omopsketch_plot <- 
  sapply(list_all_schemas_va_hdss_cdm$schema_name[grepl("_cdm$", list_all_schemas_va_hdss_cdm$schema_name)], function(x){
    
    nn <- x
    
    snapshot <- OmopSketch::summariseTrend(cdm = cdm_reference[[nn]]
                                           , event = c("death")
                                           , episode = NULL #"observation_period"
                                           , output = "record" #record" (default), "person", "person-days", "age", "sex"
                                           , interval = "years" #"years", "quarters", "months" or "overall"
                                           , ageGroup = list("Under 5" = c(0, 4), "5-14" = c(5, 14)
                                                             , "15-24" = c(15, 24), "25-39" = c(25, 39)
                                                             , "40-64" = c(40, 64), "65 and above" = c(65, Inf)
                                                             ) #NULL
                                           , sex = FALSE
                                           , inObservation = FALSE
                                           , dateRange = NULL
                                           )
    
    plot <- OmopSketch::plotTrend(result = snapshot
                                  , output = NULL
                                  , facet = "type" #omop_table (for more than one table)
                                  , colour = "age_group" #visOmopResults::tidyColumns() 
                                                  #"cdm_name", "variable_name", "variable_level",
                                                  # "sex"(if sex TRUE in result)
                                                  # age_group" (if defined list in ageGroup)
                                                  # "omop_table", "count", "percentage", "interval", "type" (if event or episode is defined)
                                  , style = NULL # visOmopResults::plotStyle() "darwin"  "default"
                                  , type = "ggplot" #visOmopResults::plotType() for supported plot types 
                                                       #"ggplot" "plotly"
                                  
                                  )
  
}, simplify = FALSE
)

## Combine the plots
death_agegroup_trend_omopsketch_plot_grid <- ggpubr::annotate_figure(
  ggpubr::ggarrange(plotlist = death_agegroup_trend_omopsketch_plot,
                    ncol = 3,
                    nrow = 2,
                    labels = names(death_agegroup_trend_omopsketch_plot),
                    hjust = -0.5,
                    vjust = 0.5,
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL),
                    legend = "right", 
                    common.legend = TRUE
                    ),
  top = "",
  right = NULL,
  left = "",
  bottom = NULL
  )

print(death_agegroup_trend_omopsketch_plot_grid)

## save plot output
ggsave(plot=death_agegroup_trend_omopsketch_plot_grid, height = 7.5, width = 16,
       filename = paste0("death_agegroup_trend_omopsketch_plot",".png"),
       path = OMOPSketch_Dir, bg='white')


