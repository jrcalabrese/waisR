#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(scales)

set.seed(12345)

source("subtests.R")
source("main_scales.R")
#source("bell_curve.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$full_subtests_plot <- renderPlot({
      
        # put subtests in a dataframe
        full_subtests_df <- data.frame(rbind(
          input$similarities, 
          input$vocabulary, input$information, input$comprehension,
          input$block_design, input$matrix_reasoning, input$visual_puzzles, input$figure_weights, input$picture_completion,
          input$digit_span, input$arithmetic, input$letter_number,
          input$symbol_search, input$coding, input$cancellation)) %>%
          tibble::rownames_to_column(var = "scale") %>%
          rename(scale_score = 2) %>%
          mutate(scale_group = c("Verbal Comprehension", "Verbal Comprehension", "Verbal Comprehension", "Verbal Comprehension",
                                 "Perceptual Reasoning", "Perceptual Reasoning", "Perceptual Reasoning", "Perceptual Reasoning", "Perceptual Reasoning", 
                                 "Working Memory", "Working Memory", "Working Memory", 
                                 "Processing Speed", "Processing Speed", "Processing Speed"))
        
        full_subtests_df$scale_group = factor(full_subtests_df$scale_group,
                                              levels = c("Verbal Comprehension", "Perceptual Reasoning", "Working Memory", "Processing Speed"))
        
        full_subtests_df$gp <- c(1,1,1,1,2,2,2,2,2,3,3,3,4,4,4)
                                 
        # make ggplot labels
        labels = c("SI", "VC", "IN", "(CO)",
                   "BD", "MR", "VP", "(FW)", "(PCm)",
                   "DS", "AR", "(LN)",
                   "SS", "CD", "(CA)")
        
        names(labels) <- interaction(full_subtests_df$scale, factor(full_subtests_df$scale_group))

        # make a ggplot
        full_subtests_df %>%
          ggplot(aes(x=interaction(scale, factor(scale_group)), y=as.numeric(scale_score), 
                     label = (scale_group),
                     group=interaction(scale_group, gp))) +
          theme_bw() +
          geom_line(stat="identity") + 
          geom_point(size=2, color="blue") +
          geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 9.5, ymax = 10.5),
                    alpha = 0.015, fill = "darkturquoise") +
          scale_y_continuous(breaks = (seq(0, 20, by = 1)), limits = c(0,20)) +
          scale_x_discrete(position = "top", labels = labels) +
          xlab("") + ylab("") +
          ggtitle("WAIS-IV Subtest Scaled Score Profile") + theme(plot.title = element_text(hjust = 0.5)) +
          facet_grid( ~ scale_group,
                      scales = "free", space = "free")
        
    })
    
    output$full_subtests_down <- downloadHandler(
      filename = "full_subtests_plot.png",
      content = function(file){
        ggsave(file, device = "png", width=7, height=5)
      
    
      })
    
    output$main_scales_plot <- renderPlot({
      
      # put main scales in a dataframe
      main_scales_df <- data.frame(rbind(
        input$vci, input$pri, input$wmi, input$psi,
        input$fsiq, input$gai, input$cpi)) %>%
        tibble::rownames_to_column(var = "scale") %>%
        rename(scale_score = 2)
      
      # make ggplot labels
      main_scale_labels = c("VCI", "PRI", "WMI", "PSI",
                            "FSIQ", "GAI", "CPI")
      
      main_scales_df$gp <- c(1,1,1,1,
                             2,2,2)
      
      # make a ggplot
      main_scales_df %>%
        ggplot(aes(x=scale, y=as.numeric(scale_score), group=gp)) +
        theme_bw() +
        geom_line(stat="identity") + 
        geom_point(size=2, color="blue") +
        geom_vline(xintercept = 4.5) +
        geom_vline(xintercept = 9.5) +
        geom_vline(xintercept = 12.5) + 
        geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 99.5, ymax = 100.5),
                  alpha = 0.015, fill = "darkturquoise") +
        scale_y_continuous(breaks = (seq(35, 165, by = 5)), limits = c(35,165)) +
        scale_x_discrete(position = "top", labels = unique(main_scale_labels)) +
        xlab("") + ylab("") +
        ggtitle("WAIS-IV Composite Score Profile") + theme(plot.title = element_text(hjust = 0.5)) 
      
    })
    
    output$main_scales_down <- downloadHandler(
      filename = "main_scales_plot.png",
      content = function(file){
        ggsave(file, device = "png", width=7, height=5)
        
      })
}
    
    #output$bell_curve_plot <- renderPlot({
      
      # put main scales in a dataframe
      #bell_curve_df <- data.frame(rbind(
        #input$vci_bell, input$pri_bell, input$wmi_bell, input$psi_bell, input$fsiq_bell)) %>%
        #tibble::rownames_to_column(var = "scale") %>%
        #rename(percentile = 2)
      
      # make ggplot labels
      #bell_curve_labels = c("VCI", "PRI", "WMI", "PSI", "FSIQ")
      
      # do stuff to make the plot easier
      #curve <- data.frame(score = seq(35, 165, 
                                      #length.out = 131))
      
      #curve$y <- dnorm(curve$score, mean = 100, sd = 15)
      
      #curve <- curve %>%
        #mutate(classification = case_when(
          #score <= 69 ~ "Intellectual Disability",
          #score <= 79 ~ "Borderline",
          #score <= 89 ~ "Low Average",
          #score <= 109 ~ "Average",
          #score <= 119 ~ "High Average", 
          #score <= 129 ~ "Superior",
          #score > 129 ~ "Very Superior",
          #TRUE ~ NA_character_)
        #)
      
      #curve$classification <- factor(curve$classification, 
                                     #levels=c("Intellectual Disability",
                                              #"Borderline",
                                              #"Low Average",
                                              #"Average",
                                              #"High Average",
                                              #"Superior",
                                              #"Very Superior"))
      
      # make the bell
      #ggplot(curve, aes(x = score, y = y, fill = classification)) + 
        #geom_ribbon(data = curve, aes(ymax = y), ymin = 0, alpha = 0.5) +
        #geom_line(color = "black") + 
        #theme_bw() + 
        #theme(legend.position = "bottom") +
        #scale_fill_manual(values = 
                            #c("red", "green", "blue", "yellow", "blue", "green", "red")) +
        #scale_y_continuous("", breaks=NULL) + 
        #scale_x_continuous("", breaks=NULL) +
        #geom_vline(data = bell_curve_df, aes(xintercept = as.numeric(percentile)))
      
      #})
    
    #output$bell_curve_down <- downloadHandler(
      #filename = "bell_curve_plot.png",
      #content = function(file){
        #ggsave(file, device = "png", width=12, height=7)
        
      #})
    
#}
    

# DO NOT ACTUALLY INLCLUDE THESE LINES IN THE FINAL PRODUCT!!!!
# JUST COPY AND PASTE THEM DIRECTLY INTO YOUR CONSOLE!!!

# Run the application 
#shinyApp(ui = ui, server = server)

# Deploy app
#deployApp(appName="waisR", account="jrcalabrese")

