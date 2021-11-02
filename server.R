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

source("full_subtests.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$full_subtests_plot <- renderPlot({
      
        # put validity in a dataframe
        full_subtests_df <- data.frame(rbind(input$block_design, input$similiarities, input$digit_span, 
                                        input$matrix_reasoning, input$vocabulary,
                                        input$arithmetic, input$symbol_search, input$visual_puzzles, 
                                        input$information, input$coding,
                                        input$letter_number, input$figure_weights, input$comprehension, 
                                        input$cancellation, 
                                        input$picture_completion)) %>%
          tibble::rownames_to_column(var = "scale") %>%
          rename(scale_score = 2)
        
        # order scale variable
        subtests_labels = c("Block Design", "Similiarities", "Digit Span", 
        "Matrix Reasoning", "Vocabulary", "Arithmetic", "Symbol Search", "Visual Puzzles", 
        "Information", "Coding", "Letter-Number Sequencing", "Figure Weights", "Comprehension",
        "Cancellation", "Picture Completion")
        
        # make a ggplot
        full_subtests_df %>%
          ggplot(aes(x=scale, y=as.numeric(scale_score), group=1)) + # Group=1 connects points
          theme_bw() +
          geom_line() + 
          geom_point(size=2) +
          #geom_hline(yintercept=50) +
          #geom_hline(yintercept=65) +
          scale_y_continuous(sec.axis = dup_axis(), breaks = (seq(0, 20, by = 1)), limits = c(0,20)) +
          scale_x_discrete(labels = subtests_labels) +
          xlab("") + ylab("") +
          ggtitle("WAIS-IV Subtests") + theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    output$full_subtests_down <- downloadHandler(
      filename = "full_subtests_plot.png",
      content = function(file){
        ggsave(file, device = "png", width=7, height=3.5)
      }
    )
}
    