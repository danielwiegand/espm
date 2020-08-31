## LIBRARIES

library(tidyverse)
library(shiny)
library(ggiraph)
library(nleqslv)


ui <- fluidPage(
  
  titlePanel("ESPM"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("global_emission_budget_gt_2018", label = "Global emission budget", value = 420),
      
      sliderInput("pop_weighting", label = "Population weighting", min = 0, max = 100, step = 10, value = 50),
      
      sliderInput("max_negative_emissions_perc", label = "Maximum negative emissions", min = 0, max = 10, step = 1, value = 8)

      ),
    
    mainPanel(
      
      
    )
  )
)
  