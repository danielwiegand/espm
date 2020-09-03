## LIBRARIES

library(tidyverse)
library(shiny)
library(ggiraph)
library(nleqslv)
library(bench)


ui <- fluidPage(
  
  titlePanel("ESPM"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selected_rm", label = "Select pathway type", 
                  choices = c("RM-1", "RM-2", "RM-3", "RM-4", "RM-5", "RM-6"),
                  selected = "RM-1"),
      
      numericInput("global_emission_budget_gt_2018", label = "Global emission budget", value = 420),
      
      sliderInput("pop_weighting", label = "Population weighting", min = 0, max = 100, step = 10, value = 50),
      
      sliderInput("max_negative_emissions_perc", label = "Maximum negative emissions", 
                  min = 0, max = 10, step = 1, value = 8)

      ),
    
    mainPanel(
      
      plotOutput("emis_pathway")
      
    )
  )
)
  