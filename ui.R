## LIBRARIES

library(tidyverse)
library(ggiraph)
library(nleqslv)
library(shinyWidgets)
library(RColorBrewer)
library(shinyjs)
library(gridExtra)
library(gridtext)

## UI

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href = "style.css")
    ),

  titlePanel(title = uiOutput("title"),
             windowTitle = "Smooth Emission Paths"),

  absolutePanel(top = "10px", right = "10px", actionLink("link_author", label = "Author & Contact", icon = icon("info-circle"))),
  
  uiOutput("box_contact"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
      
      h3("1. Select scenario types", style = "float:left;"),
      
      actionLink("link_info_scenario_type", "", icon = icon("info-circle"), style = "float:left; margin-top:20px; margin-left:10px;"),
      uiOutput("box_info_scenario_type"),
      tags$br(), tags$br(),
      
      checkboxGroupButtons(
        inputId = "selected_rm",
        label = "", 
        choiceNames = c("RM-1", "RM-3", "RM-4", "RM-5", "RM-6"),
        choiceValues = c("RM-1 const", "RM-3 lin", "RM-4 quadr", "RM-5 rad", "RM-6 abs"),
        # selected = c("RM-1 const", RM-3 lin", "RM-4 quadr", "RM-5 rad", "RM-6 abs")
        selected = c("RM-4 quadr", "RM-3 lin", "RM-5 rad", "RM-6 abs")
      ),
      
      h3("2. Budget and past emissions", style = "margin-top:35px;"),
      
      tags$div("Global or regional emission budget (2020-2100)", style = "font-weight:bold; float:left;"),
      actionLink("link_info_budget", "", icon = icon("info-circle"), style = "float:left; margin-left: 10px;"),
      uiOutput("box_info_budget"),
      
      numericInput("emission_budget", label = "", value = 334, width = "80%"),
      
      tags$div("Base year (2019) emissions", style = "font-weight:bold; float:left;"),
      numericInput("base_year_emissions", label = "", value = 43.1, width = "80%"),
      
      sliderInput("reference_year", label = "Reference year", 
                  min = 1990, max = 2020, step = 1, value = 2010, sep = "", width = "80%"),
      
      numericInput("reference_year_emissions", label = "Reference year emissions", value = 38.5, width = "80%"),
      
      h3("3. Select negative emissions", style = "float:left;"),
      
      actionLink("link_info_negative_emissions", "", icon = icon("info-circle"), style = "float:left; margin-top:20px; margin-left:10px;"),
      uiOutput("box_info_negative_emissions"),
      
      numericInput("max_negative_emissions_perc", label = "Maximum possible net negative emissions (%)", value = 0, width = "80%"),
      
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
      ),
      
      downloadButton("report", "Generate report")
      
    ),
    
    mainPanel(

      fluidRow(
        
        column(12,
               
               tags$h3("Emissions over time"),
               div(class = "plot-container", style = "clear:left; width:100%;",
                   girafeOutput("emis_pathway")
                   )
        ),
        
        column(5,
               
               tags$h3("Comparison to reference year"),
               div(class = "plot-container", style = "clear:left; width:100%; margin-left:20px; margin-top:20px;",
                   tableOutput("comparison_reference_year")
               )
               
        ),
        
        column(7,
               tags$h3("Annual emission change rates"),
               div(class = "plot-container", style = "clear:left; width:100%;",
                   girafeOutput("emission_change_rates")
               )
               
        )
      )
    )
  )
)
  