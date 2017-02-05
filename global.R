# BSD_2_clause

library(dplyr)
library(elastic)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(stringr)

system("touch restart.txt", intern = FALSE)

# readRenviron("/home/jacobmalcom/.Renviron")

elastic::connect()
