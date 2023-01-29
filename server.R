"Main file of the ESPM web app containing the server"

FIRST_YEAR <- 2020 # First year for which emissions are calculated
EU_POPULATION_SHARE <- .058
EU_EMISSIONS_SHARE <- .070
EU_EMISSIONS_2019 <- 2.93 # Annual EU emissions in 2019 (Gt)
EU_EMISSIONS_1990 <- 3.80 # Annual EU emissions in 1990 (Gt)
THRESHOLD_LINEAR_RM1 <- 0.136741914720844000 # Threshold from when on the path becomes linear (rm1)
THRESHOLD_LINEAR_OTHER <- 0.106354822560656 # Threshold from when on the path becomes linear (all other rms)
INITIAL_REDUCTION_RATE <- -0.0418 # Emission reduction rate to start with (in RM 2-5); is assumed EU emission change between 2019 and 2020 (percent)
EU_PAST_EMISSIONS <- data.frame(
  year = seq(2010, 2019, 1),
  emissions = c(3.351, 3.257, 3.164, 3.075, 2.954, 3.018, 3.042, 3.126, 3.057, EU_EMISSIONS_2019),
  historical = "y"
)

server <- function(input, output) {
  
  source("pathway.R", local = T)
  source("plots.R", local = T)
  source("report.R", local = T)
  source("notifications.R", local = T)
  
  # Definition of some general variables ####
  
  date_display_range <- reactive({
    if(input$date_display_range == T) {2100} else {2050}
  })
  
  colors_to_display <- eventReactive(input$go, ignoreNULL = F, {
    a <- c("RM-1 const" = "#4b8abd", "RM-2 exp" = "#b9594d", "RM-3 lin" = "#a7b25d", 
           "RM-4 quadr" = "#7970a2", "RM-5 rad" = "#f1974b", "RM-6 abs" = "#818181")
    b <- a[input$selected_rm]
  }) 
  
  global_emission_budget_gt <- reactive({
    input$global_emission_budget_gt_2020
  })
  
  # Relation between temperature and emissions budget ####
  
  # temperature_budget_relation <- tibble(
  #   temperature = c(1.5, 1.57, 1.6, 1.67, 1.75),
  #   budget = c(420, 530, 570, 680, 800)
  # )
  # 
  # global_emission_budget_gt <- reactive({
  #   temperature_budget_relation$budget[temperature_budget_relation$temperature == input$temperature_increase]  - GLOBAL_EMISSIONS_2018_2019_GT
  # })
  
  
  # Render information for the left column ####
  
  output$weighted_key <- renderTable(
    data.frame(x = c("EU share of global emissions", "EU share of global population", "Weighted key", "EU emission budget from 2020 on"),
               y = c("7.0%", "5.8%", paste0(round(weighted_key() * 100, 1), "%"),
                     paste0(round(eu_emission_budget_gt(), 1), " Gt CO2"))),
    colnames = F
  )
  
  output$negative_emissions <- renderTable(
    data.frame(x = "Maximum possible net negative emissions (p.a.): ",
               # THIS IS ROUNDED TO ONLY ONE DECIMAL PLACE WHICH DECREASES ACCURACY TO MAKE IT COMPATIBLE TO RESULTS IN THE ESPM PAPER. CAN BE CHANGED BACK IN FUTURE.
               y = paste0(round(max_negative_emissions_gt()*-1, 1), " Gt")), 
    colnames = F
  )
  
  # Weighted key and emission budget ####
  
  weighted_key <- reactive({
    EU_POPULATION_SHARE * input$pop_weighting / 100 + EU_EMISSIONS_SHARE * (1 - input$pop_weighting / 100)
  })
  
  eu_emission_budget_gt <- reactive({
    global_emission_budget_gt() * weighted_key()
  })
  
  # Maximum net negative emissions ####
  
  max_negative_emissions_gt <- reactive({
    # THIS IS ROUNDED TO ONLY ONE DECIMAL PLACE WHICH DECREASES ACCURACY TO MAKE IT COMPATIBLE TO RESULTS IN THE ESPM PAPER. CAN BE CHANGED BACK IN FUTURE.
    round(input$max_negative_emissions_perc / 100 * EU_EMISSIONS_2019 * -1, 1)
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
      mutate("Unit" = "Gt") %>%
      # in case some paths do not have overshoots, replace NA by 0
      mutate(Overshoot = case_when(is.na(Overshoot) ~ 0, 
                                   TRUE ~ Overshoot))
    
    return(out)
  })
  
  
  # Pathway calculation ####
  
  result <- eventReactive(input$go, {
    calculate_pathway(rm = input$selected_rm)
  }, ignoreNULL = F) # Fire also at startup
  
  output$emis_pathway <- renderGirafe({
    girafe(ggobj = plot_result(result()), width_svg = 7, height_svg = 2.5) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"),
                     opts_selection(type = "none"))
  })

}
