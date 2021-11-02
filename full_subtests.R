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

# Define UI for application that draws various ggplots
full_subtests <- tabPanel("Full Scales",
  titlePanel("Full 15 Subtests"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "block_design", "Block Design", 10, min = 0, max = 20),
      numericInput(inputId = "similiarities", "Similiarities", 10, min = 0, max = 20),
      numericInput(inputId = "digit_span", "Digit Span", 10, min = 0, max = 20),
      numericInput(inputId = "matrix_reasoning", "Matrix Reasoning", 10, min = 0, max = 20),
      numericInput(inputId = "vocabulary", "Vocabulary", 10, min = 0, max = 20),
      numericInput(inputId = "arithmetic", "Arithmetic", 10, min = 0, max = 20),
      numericInput(inputId = "symbol_search", "Symbol Search", 10, min = 0, max = 20),
      numericInput(inputId = "visual_puzzles", "Visual Puzzles", 10, min = 0, max = 20),
      numericInput(inputId = "information", "Information", 10, min = 0, max = 20),
      numericInput(inputId = "coding", "Coding", 10, min = 0, max = 20),
      numericInput(inputId = "letter_number", "Letter-Number Sequencing", 10, min = 0, max = 20),
      numericInput(inputId = "figure_weights", "Figure Weights", 10, min = 0, max = 20),
      numericInput(inputId = "comprehension", "Comprehension", 10, min = 0, max = 20),
      numericInput(inputId = "cancellation", "Cancellation", 10, min = 0, max = 20),
      numericInput(inputId = "picture_completion", "Picture Completion", 10, min = 0, max = 20),
      ),
    # Show the line plot
    mainPanel(
      plotOutput(outputId = "full_subtests_plot"),
      downloadButton(outputId = "full_subtests_down", label = "Download the plot"),
    )
  )
)