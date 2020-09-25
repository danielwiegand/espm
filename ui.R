## LIBRARIES

library(tidyverse)
library(ggiraph)
library(nleqslv)
library(shinyWidgets)
library(RColorBrewer)
library(shinyjs)
library(gridExtra)

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href = "style.css")
  ),
  
  titlePanel(title = uiOutput("title"),
             windowTitle = "Extended Smooth Pathway Model (ESPM)"),

  absolutePanel(top = "10px", right = "10px", actionLink("link_author", label = "Author & Contact", icon = icon("info-circle"))),
  
  uiOutput("box_contact"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("1. Select a scenario type", style = "float:left;"),
      
      actionLink("link_info_scenario_type", "", icon = icon("info-circle"), style = "float:left; margin-top:20px; margin-left:10px;"),
      uiOutput("box_info_scenario_type"),
      
      checkboxGroupButtons(
        inputId = "selected_rm",
        label = "", 
        choiceNames = c("RM-1", "RM-2", "RM-3", "RM-4", "RM-5", "RM-6"),
        choiceValues = c("RM-1 const", "RM-2 exp", "RM-3 lin", "RM-4 quadr", "RM-5 rad", "RM-6 abs"),
        # selected = c("RM-1 const", "RM-2 exp", "RM-3 lin", "RM-4 quadr", "RM-5 rad", "RM-6 abs")
        selected = c("RM-2 exp", "RM-3 lin", "RM-5 rad", "RM-6 abs")
      ),

      h3("2. Calculate the EU's emission budget"),
      
      tags$div("Global emission budget (Gt CO2)", style = "font-weight:bold; float:left;"),
      actionLink("link_info_budget", "", icon = icon("info-circle"), style = "float:left; margin-left: 10px;"),
      uiOutput("box_info_budget"),
      
      numericInput("global_emission_budget_gt_2018", label = "", value = 420, width = "70%"),

      tags$div(style = "float:left;",
               tableOutput("global_budget")
      ),
      actionLink("link_info_emissions_2018", "", icon = icon("info-circle"), style = "float:left; margin-left:10px; margin-top:5px;"),
      uiOutput("box_info_emissions_2018"),
      
      tags$br(),
      
      sliderInput("pop_weighting", label = "Weighting population vs. emissions (%)", 
                  min = 0, max = 100, step = 10, value = 50, pre = "Population: ", post = "%"),
      
      tags$div(style = "float:left;",
        tableOutput("weighted_key")
      ),
      
      actionLink("link_info_eu_budget", "", icon = icon("info-circle"), style = "float:left; margin-top:100px; margin-left: 10px;"),
      uiOutput("box_info_eu_budget"),
      
      h3("3. Select negative emissions", style = "float:left;"),
      
      actionLink("link_info_negative_emissions", "", icon = icon("info-circle"), style = "float:left; margin-top:20px; margin-left:10px;"),
      uiOutput("box_info_negative_emissions"),
      
      sliderInput("max_negative_emissions_perc", label = "Maximum possible net negative emissions (%)", 
                  min = 0, max = 10, step = 1, value = 8),
      
      tableOutput("negative_emissions"),
      
      div(style = "width:100%; text-align:center;",
          actionButton("go", label = "Update", icon = icon("retweet"))
      ),
      
      switchInput(
        inputId = "date_display_range",
        onLabel = "2100",
        offLabel = "2050",
        size = "mini",
        onStatus = "info",
        offStatus = "info",
        label = "Date range",
        labelWidth = "80px"
      )
      
    ),
    
    mainPanel(

      fluidRow(
        
        column(12,
               
               # absolutePanel(draggable = T,
               #               
               #               wellPanel(sliderInput("display_year",
               #                                     "Maximum year to display",
               #                                     min = 2030,
               #                                     max = 2100,
               #                                     value = 2050,
               #                                     step = 10),
               #                         
               #                         style = "z-index: 10; opacity: 0.85; font-size:16px;", 
               #                         top = "0%", right = "16%", fixed = T, width = "22%", align = "justify"
               #               )
               # ),
               
               div(class = "plot-container", style = "clear:left; width:100%;",
                   girafeOutput("emis_pathway")
                   )
        ),
        
        column(6,
               
               div(class = "plot-container", style = "clear:left; width:100%;",
                   girafeOutput("comparison_1990")
               )
        ),
        
        column(6,
               
               div(class = "plot-container", style = "clear:left; width:100%;",
                   girafeOutput("emission_change_rates")
               )
               
        )
      )
    )
  )
)
  