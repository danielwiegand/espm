## LIBRARIES

library(tidyverse)
library(shiny)
library(ggiraph)
library(nleqslv)
library(bench)
library(Rcpp)


ui <- fluidPage(
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href = "style.css")
  ),
  
  titlePanel("ESPM"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("1. Wahl des Funktionstyps"),
      
      selectInput("selected_rm", label = "", 
                  choices = c("RM-1", "RM-2", "RM-3", "RM-4", "RM-5", "RM-6"),
                  selected = "RM-1"),
      
      h3("2. Bestimmung des EU-Emissionsbudgets"),
      
      numericInput("global_emission_budget_gt_2018", label = "Globales Emissionsbudget (Gt CO2)", value = 420),
      
      tableOutput("global_budget"),
      
      tags$br(),
      
      sliderInput("pop_weighting", label = "Gewichtung Bevölkerung vs. Emissionen (%)", 
                  min = 0, max = 100, step = 10, value = 50, pre = "Bevölkerung: ", post = "%"),
      
      tableOutput("weighted_key"),
      
      h3("3. Wahl der maximal möglichen Negativemissionen"),
      
      sliderInput("max_negative_emissions_perc", label = "Maximal mögliche Netto-Negativemissionen (%)", 
                  min = 0, max = 10, step = 1, value = 8),
      
      tableOutput("negative_emissions"),
      
      div(style = "width:100%; text-align:center;",
          actionButton("go", label = "Aktualisierung", icon = icon("retweet"))
      )
    ),
    
    mainPanel(
      
      div(class = "plot-container", style = "clear:left; width:90%;",
          girafeOutput("emis_pathway")
      )
      
    )
  )
)
  