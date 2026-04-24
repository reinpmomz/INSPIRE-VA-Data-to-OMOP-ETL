library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(ggplot2)


working_directory

#summary of vocabs in OMOP-tables

summary_OMOP_vocabs <- sapply(ls(pattern = "_cdm_table$"), function(x){
  nn <- x
  tables <- get(x)
  studies <- names(tables)
  
  out <- sapply(studies, function(y){
     data <- tables[[y]] %>%
       dplyr::select(contains("_concept_id")) %>%
       dplyr::mutate(across(everything(), ~as.integer(.))) %>%
       tidyr::pivot_longer(everything()) %>%
       dplyr::distinct()
      
  }, simplify = FALSE
    )
  
  out_ <- dplyr::bind_rows(out) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(table = nn)
  

}, simplify = FALSE
)

summary_OMOP_vocabs_merge <- dplyr::bind_rows(summary_OMOP_vocabs) %>%
  dplyr::left_join(vocabulary_tables_data[["concept"]]
                   ,by = c("value"="concept_id")
                   )

## Save the output 

writexl::write_xlsx(list(summary_vocabs_merge = summary_OMOP_vocabs_merge
                         ,count_vocabs_unique = summary_OMOP_vocabs_merge %>%
                           dplyr::select(-c(name, table)) %>%
                           dplyr::distinct() %>%
                           dplyr::mutate(vocabulary = if_else(vocabulary_id == "INSPIRE", "Local", "Standard")
                                         , vocabulary = if_else(vocabulary == "Standard" & standard_concept %in% c("S") , "Standard and Valid",
                                                                if_else(vocabulary == "Standard" & is.na(standard_concept), "Non-Standard and Valid",
                                                                        vocabulary
                                                                        )
                                                                )
                                         , across(c(domain_id, vocabulary), ~as.factor(.x))
                                         ) %>%
                           dplyr::select(value, domain_id, vocabulary) %>%
                           dplyr::group_by(domain_id, vocabulary) %>%
                           dplyr::count(name = "total") %>%
                           dplyr::ungroup() %>%
                           dplyr::arrange(-total)
                         ),
                    path = base::file.path(output_Dir, paste0("summary_OMOP_vocabs_merge.xlsx") )
                    )

# Plot of Unique vocab Domains

summary_OMOP_vocabs_unique_plot <- summary_OMOP_vocabs_merge %>%
  dplyr::select(-c(name, table)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(vocabulary = if_else(vocabulary_id == "INSPIRE", "Local", "Standard")
                , vocabulary = if_else(vocabulary == "Standard" & standard_concept %in% c("S") , "Standard and Valid",
                                       if_else(vocabulary == "Standard" & is.na(standard_concept), "Non-Standard and Valid",
                                               vocabulary
                                              )
                                       )
                , across(c(domain_id, vocabulary), ~as.factor(.x))
                ) %>%
  dplyr::select(value, domain_id, vocabulary) %>%
  ggplot(aes(x= forcats::fct_rev(forcats::fct_infreq(domain_id)), group=vocabulary)) +
  coord_flip() +
  geom_bar(aes(y = after_stat(count), fill = vocabulary),
           position="stack", stat="count", show.legend = TRUE, width = 0.8) +
  scale_y_continuous(n.breaks = 10, limits = c(NULL, NULL),
                     expand = expansion(mult = c(0,0.05))) +
  geom_text(aes(label = paste0(#after_stat(count),
                               #" (",
                               scales::percent(after_stat(count)/sum(after_stat(count)),
                                               accuracy = 0.1) #,")" 
                               )
                ),
            stat = "count", 
            hjust = 0.03,
            angle = 35,
            position = position_stack(vjust = 0.5),
            colour = "black",
            size = 3.3) +
  labs(x="Domain",y="Unique count of concepts", fill="") +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 9, color = "red", face = "bold", hjust = 0.5),
    axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
    axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
    #axis.ticks.y = element_line(linewidth = 0.5, color="black"),
    axis.ticks.x = element_line(linewidth = 0.5, color="black"),
    axis.text.y = element_text(angle = 0, lineheight = 0.7, size = 10), #hjust = 0.5
    axis.text.x = element_text(angle = 0, lineheight = 0.7, size = 10), #vjust = 0.5
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.caption = element_text(angle = 0, size = 10, face = "italic"),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
  
plot(summary_OMOP_vocabs_unique_plot)

# Save the plot

ggsave(plot=summary_OMOP_vocabs_unique_plot, height = 7, width = 12.5,
       filename = "summary_OMOP_vocabs_unique_plot.png",
       path = output_Dir, bg='white')
  
