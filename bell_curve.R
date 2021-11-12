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
bell_curve <- tabPanel("Bell Curve (will fix later...)",
                     titlePanel("Bell Curve"),
                     sidebarLayout(
                       sidebarPanel(
                         # Enter numbers
                         numericInput(inputId = "vci_bell", "Verbal Comprehension", 100, min = 35, max = 165),
                         numericInput(inputId = "pri_bell", "Perceptual Reasoning", 100, min = 35, max = 165),
                         numericInput(inputId = "wmi_bell", "Working Memory", 100, min = 35, max = 165),
                         numericInput(inputId = "psi_bell", "Processing Speed", 100, min = 35, max = 165),
                         numericInput(inputId = "fsiq_bell", "Full Scale", 100, min = 35, max = 165),
                       ),
                       # Show the line plot
                       mainPanel(
                         plotOutput(outputId = "bell_curve_plot"),
                         downloadButton(outputId = "bell_curve_down", label = "Download the plot"),
                       )
                     )
)