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
source("bell_curve.R")

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
                    alpha = 0.02, fill = "darkturquoise") +
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
      
      # make custom legend
      legend <- data.frame( 
        classification = c("Very Superior", "Superior", "High Average", "Average", "Low Average", "Borderline", "Intellectual Disability"),
        color = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"))
      
      # make it a factor
      legend$classification <- factor(legend$classification, 
                                     levels=c("Intellectual Disability",
                                              "Borderline",
                                              "Low Average",
                                              "Average",
                                              "High Average",
                                              "Superior",
                                              "Very Superior"))
      
      # make a ggplot
      main_scales_df %>%
        ggplot(aes(x=scale, y=as.numeric(scale_score), group=gp)) +
        theme_bw() +
        geom_line(stat="identity",color="blue") + 
        {if (input$checkbox == TRUE) geom_point(size=2, shape=15, aes(color = factor(legend$classification))) } +
        {if (input$checkbox == TRUE) scale_color_manual(name = " ", values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")) } +
        geom_point(color= "blue", fill="blue", shape=16, size=3) +
        geom_vline(xintercept = 4.5) +
        geom_vline(xintercept = 9.5) +
        geom_vline(xintercept = 12.5) +
        ## Color Bars
        # Very Superior
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 130, ymax = 139),
                                               alpha = 0.06, fill = "#3B9AB2") } +
        # Superior
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 120, ymax = 129),
                                               alpha = 0.06, fill = "#78B7C5") } +
        # High Average
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 110, ymax = 119),
                                               alpha = 0.06, fill = "#EBCC2A") } +
        # Average
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = 109),
                                               alpha = 0.06, fill = "#E1AF00") } +
        # Low Average
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 80, ymax = 89),
                                               alpha = 0.06, fill = "#EBCC2A") } +
        # Borderline
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 79),
                                               alpha = 0.06, fill = "#78B7C5") } +
        # Intellectual Disability
        {if (input$checkbox == TRUE) geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 60, ymax = 69),
                                               alpha = 0.06, fill = "#3B9AB2") } +
          ##### *** Instead of shaded boxes, just dotted lines!?????
        ## Custom Legend
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
    
    output$bell_curve_plot <- renderPlot({
    
    # put main scales in a dataframe
    bell_curve_df <- data.frame(rbind(
    input$vci_bell, input$pri_bell, input$wmi_bell, input$psi_bell, input$fsiq_bell)) %>%
    tibble::rownames_to_column(var = "scale") %>%
    rename(percentile = 2)
    
    # make ggplot labels
    bell_curve_labels = c("VCI", "PRI", "WMI", "PSI", "FSIQ")
    
    # do stuff to make the plot easier
    curve <- data.frame(score = seq(35, 165, 
                                    length.out = 126)) #126
    
    curve$y <- dnorm(curve$score, mean = 100, sd = 15)
    
    # make dummy labels for the table content
    dummy <- data.frame(classification = c(
      "Intellectual Disability", 
      "Borderline", 
      "Low Average", 
      "Average", 
      #"Average", 
      "High Average", 
      "Superior", 
      "Very Superior")
    )

    curve <- curve %>%
      mutate(classification = case_when(
        score <= 69.99 ~ "Intellectual Disability",
        score <= 79.99 ~ "Borderline",
        score <= 89.99 ~ "Low Average",
        score <= 109.99 ~ "Average",
        score <= 119.99 ~ "High Average", 
        score <= 129.99 ~ "Superior",
        score > 129.99 ~ "Very Superior",
        TRUE ~ NA_character_)
      )
    
    curve$classification <- factor(curve$classification, 
                                   levels=c("Intellectual Disability",
                                            "Borderline",
                                            "Low Average",
                                            "Average",
                                            "High Average",
                                            "Superior",
                                            "Very Superior"))
    
    # make the bell
    ggplot(data = curve, aes(x = score, y = y, fill = classification)) + 
      geom_ribbon(data = curve, aes(ymax = y), ymin = 0, alpha = 0.5) +
      geom_line(color = "black") + 
      theme_bw() + xlab("") +
      ggtitle("WAIS-IV Normal Curve Profile") + theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 1)) +
      theme(legend.title = element_blank()) +
      scale_fill_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")) +
      # This removes x-axis tick marks, I don't want that I don't think...
      scale_y_continuous("", breaks=NULL) + 
      #scale_x_continuous("", breaks=NULL) +
      scale_x_continuous(breaks = round(seq(min(curve$score), max(curve$score), by = 10),1)) +
      geom_text(aes(x=65, y=.0005, label="2.2%"), check_overlap = TRUE) +
      geom_text(aes(x=75, y=.0005, label="6.7%"), check_overlap = TRUE) +
      geom_text(aes(x=85, y=.0005, label="16.1%"), check_overlap = TRUE) +
      geom_text(aes(x=100, y=.0005, label="50%"), check_overlap = TRUE) +
      #geom_text(aes(x=105, y=.0005, label="25%"), check_overlap = TRUE) +
      geom_text(aes(x=115, y=.0005, label="16.1%"), check_overlap = TRUE) +
      geom_text(aes(x=125, y=.0005, label="6.7%"), check_overlap = TRUE) +
      geom_text(aes(x=135, y=.0005, label="2.2%"), check_overlap = TRUE) +
      #geom_vline(data = bell_curve_df, aes(xintercept = as.numeric(percentile))) +
      annotate("label", x = input$vci_bell, y = .023, label = paste0("VCI = ", input$vci_bell, "\n(", (ifelse(input$vci_bell < 69, "Intellectual Disability",
                                                                                                      ifelse(input$vci_bell < 79, "Borderline",
                                                                                                             ifelse(input$vci_bell < 89, "Low Average",
                                                                                                                    ifelse(input$vci_bell < 109, "Average",
                                                                                                                           ifelse(input$vci_bell < 119, "High Average",
                                                                                                                                  ifelse(input$vci_bell < 129, "Superior",
                                                                                                                                         ifelse(input$vci_bell < 165, "Very Superior")
                                                                                                                                         ))))))), ")")) +
      annotate("label", x = input$pri_bell, y = .019, label = paste0("PRI = ", input$pri_bell, "\n(", (ifelse(input$pri_bell < 69, "Intellectual Disability",
                                                                                                             ifelse(input$pri_bell < 79, "Borderline",
                                                                                                                    ifelse(input$pri_bell < 89, "Low Average",
                                                                                                                           ifelse(input$pri_bell < 109, "Average",
                                                                                                                                  ifelse(input$pri_bell < 119, "High Average",
                                                                                                                                         ifelse(input$pri_bell < 129, "Superior",
                                                                                                                                                ifelse(input$pri_bell < 165, "Very Superior")
                                                                                                                                         ))))))), ")")) +
      annotate("label", x = input$wmi_bell, y = .015, label = paste0("WMI = ", input$wmi_bell, "\n(", (ifelse(input$wmi_bell < 69, "Intellectual Disability",
                                                                                                             ifelse(input$wmi_bell < 79, "Borderline",
                                                                                                                    ifelse(input$wmi_bell < 89, "Low Average",
                                                                                                                           ifelse(input$wmi_bell < 109, "Average",
                                                                                                                                  ifelse(input$wmi_bell < 119, "High Average",
                                                                                                                                         ifelse(input$wmi_bell < 129, "Superior",
                                                                                                                                                ifelse(input$wmi_bell < 165, "Very Superior")
                                                                                                                                         ))))))), ")")) +
      annotate("label", x = input$psi_bell, y = .011, label = paste0("PSI = ", input$psi_bell, "\n(", (ifelse(input$psi_bell < 69, "Intellectual Disability",
                                                                                                             ifelse(input$psi_bell < 79, "Borderline",
                                                                                                                    ifelse(input$psi_bell < 89, "Low Average",
                                                                                                                           ifelse(input$psi_bell < 109, "Average",
                                                                                                                                  ifelse(input$psi_bell < 119, "High Average",
                                                                                                                                         ifelse(input$psi_bell < 129, "Superior",
                                                                                                                                                ifelse(input$psi_bell < 165, "Very Superior")
                                                                                                                                         ))))))), ")")) +
      annotate("label", x = 45, y = .005, size=6, label = paste0("FSIQ = ", input$fsiq_bell, "\n(", (ifelse(input$fsiq_bell < 69, "Intellectual Disability",
                                                                                                              ifelse(input$fsiq_bell < 79, "Borderline",
                                                                                                                     ifelse(input$fsiq_bell < 89, "Low Average",
                                                                                                                            ifelse(input$fsiq_bell < 109, "Average",
                                                                                                                                   ifelse(input$fsiq_bell < 119, "High Average",
                                                                                                                                          ifelse(input$fsiq_bell < 129, "Superior",
                                                                                                                                                 ifelse(input$fsiq_bell < 165, "Very Superior")
                                                                                                                                          ))))))), ")"))
    
    })
    
    output$bell_curve_down <- downloadHandler(
      filename = "bell_curve_plot.png",
      content = function(file){
        ggsave(file, device = "png", width=12, height=7)
    
    })
    
    }    

# DO NOT ACTUALLY INLCLUDE THESE LINES IN THE FINAL PRODUCT!!!!
# JUST COPY AND PASTE THEM DIRECTLY INTO YOUR CONSOLE!!!

# Run the application 
#shinyApp(ui = ui, server = server)

# Deploy app
#deployApp(appName="waisR", account="jrcalabrese")

