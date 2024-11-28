"Main file of the web app containing the server"

server <- function(input, output, session) {
  
  source("pathway.R", local = T)
  source("plots.R", local = T)
  source("report.R", local = T)
  source("notifications.R", local = T)
  
  # Definition of some general variables ####
  
  date_display_range <- reactive({
    if(input$date_display_range == T) {2100} else {2050}
  })
  
  colors_to_display <- eventReactive(input$go, ignoreNULL = F, {
    a <- c("RM-1 const" = "#4b8abd", "RM-3 lin" = "#a7b25d", 
           "RM-4 quadr" = "#7970a2", "RM-5 rad" = "#f1974b", "RM-6 abs" = "#818181")
    b <- a[input$selected_rm]
  }) 
  
  # Thresholds for linear path ####
  
  threshold_linear_rm1 <- reactive({
    # Threshold from when on the path becomes linear (rm1)
    multiplication_factor = if_else(
      input$min_emissions < 0,
      max(
        -(input$min_emissions/input$base_year_emissions) * 1.85,
        0.045
      ),
      0.045
    )
    input$base_year_emissions * multiplication_factor
  })
  
  threshold_linear_other <- reactive({
    # Threshold from when on the path becomes linear (all other rms)
    multiplication_factor = if_else(
      input$min_emissions < 0,
      max(
        -(input$min_emissions/input$base_year_emissions) * 1.5,
        0.035
      ),
      0.035
    )
    input$base_year_emissions * multiplication_factor
  })
  
  # Overshoot amounts ####
  # Was originally inside the else statement of plot_result(), but I put it outside to make it 
  # accessible for the report. In case of problems, it might be a solution to put it back (DW, 21.02.2021)
  
  overshoot_amounts <- eventReactive(input$go, ignoreNULL = F, {
    
    total_emissions <- result() %>%
      group_by(rm) %>%
      rename("RM" = rm) %>%
      summarize("Budget" = round(sum(emissions[-1]), 1))
    
    # if there is no overshoot in any path
    if(nrow(result()[result()$emissions < 0,]) == 0) {
      overshoots <- tibble("RM" = c(input$selected_rm),
                           "Overshoot" = 0)
      
      # if there is overshoot in some paths
    } else {
      overshoots <- result() %>%
        rename("RM" = rm) %>%
        filter(emissions < 0) %>%
        group_by(RM) %>%
        summarize("Overshoot" = round(sum(emissions, na.rm = T) * -1, 1)) 
    }
    
    out <- left_join(total_emissions, overshoots) %>%
      select(RM, Budget, 'Overshoot') %>%
      mutate("Unit" = input$emission_unit) %>%
      # in case some paths do not have overshoots, replace NA by 0
      mutate(Overshoot = case_when(is.na(Overshoot) ~ 0, 
                                   TRUE ~ Overshoot))
    
    return(out)
  })
  
  # Pathway calculation ####
  
  result <- eventReactive(input$go, {
    req(input$initial_change_rate)
    calculate_pathway(rm = input$selected_rm, init_rr = input$initial_change_rate / 100)
  }, ignoreNULL = F) # Fire also at startup
  
  output$emis_pathway <- renderGirafe({
    girafe(ggobj = plot_result(result()), width_svg = 7, height_svg = 2.5) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"),
                     opts_selection(type = "none"))
  })

}
