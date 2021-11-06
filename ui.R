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

source("subtests.R")
source("main_scales.R")

ui <- navbarPage("waisR by @jrosecalabrese", theme = shinytheme("flatly"),
                 subtests,
                 main_scales
)