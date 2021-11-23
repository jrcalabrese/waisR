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
main_scales <- tabPanel("Main Scales",
  titlePanel("Main Scales"),
  sidebarLayout(
    sidebarPanel(
      # Enter numbers
      numericInput(inputId = "vci", "Verbal Comprehension", 100, min = 35, max = 165),
      numericInput(inputId = "pri", "Perceptual Reasoning", 100, min = 35, max = 165),
      numericInput(inputId = "wmi", "Working Memory", 100, min = 35, max = 165),
      numericInput(inputId = "psi", "Processing Speed", 100, min = 35, max = 165),
      numericInput(inputId = "fsiq", "Full Scale", 100, min = 35, max = 165),
      numericInput(inputId = "gai", "General Ability", 100, min = 35, max = 165),
      numericInput(inputId = "cpi", "Cognitive Proficiency", 100, min = 35, max = 165),
      ),
    # Show the line plot
    mainPanel(
      plotOutput(outputId = "main_scales_plot"),
      downloadButton(outputId = "main_scales_down", label = "Download the plot"),
      checkboxInput("checkbox",label = "Enable colorbars for descriptive classifications", value = TRUE),
      verbatimTextOutput("value"),
    )
  )
)