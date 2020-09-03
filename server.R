server <- function(input, output) {
  
  # Base data
  
  first_year <- 2020 # First year for which emissions are calculated
  eu_population_share <- .058
  eu_emissions_share <- .072
  annual_global_emissions_gt <- 42.1 # in Gt
  eu_emissions_2019 <- 3.03870921601875 # Annual EU emissions in 2019 (Gt)
  emission_change_rate_2020 <- -.0217 # EU emission change between 2019 and 2020 (percent)
  threshold_linear_rm1 <- 0.136741914720844000 # Threshold from when on the path becomes linear (rm1)
  threshold_linear_other <- 0.106354822560656 # Threshold from when on the path becomes linear (all other rms)
  initial_reduction_rate <- -0.0217324116377584 # Emission reduction rate to start with (in RM 2-5)
  
  global_emission_budget_gt <- reactive({
    input$global_emission_budget_gt_2018 - 2 * annual_global_emissions_gt
  })
  
  # EU: weighted key and emission budget
  
  weighted_key <- reactive({
    eu_population_share * input$pop_weighting / 100 + eu_emissions_share * (1 - input$pop_weighting / 100)
  })
  
  eu_emission_budget_gt <- reactive({
    global_emission_budget_gt() * weighted_key()
  })
  
  # Maximum net negative emissions
  
  max_negative_emissions_gt <- reactive({
    input$max_negative_emissions_perc / 100 * eu_emissions_2019 * -1
  })
  

  
  # PATHWAY CALCULATION
  
  make_linear <- function(x, rm) {
    for(i in 3:length(x)) {
      if(rm == "rm1") {
        if(x[i-1] <= threshold_linear_rm1) {
          x[i] <- x[i-1] + x[i-1] - x[i-2]
        }
      } else {
        if(x[i-1] <= threshold_linear_other) {
          x[i] <- x[i-1] + x[i-1] - x[i-2]
        }
      }
    }
    return(x)
  }

  make_horizontal <- function(x) {
    for(i in 3:length(x)) {
      if(x[i] <= max_negative_emissions_gt()) {
        x[i] <- max_negative_emissions_gt()
      }
    }
    return(x)
  }

  optimize_function <- function(fun, neg) {
    if(neg == T) {
      xstart <- matrix(runif(800, min = -1, max = 0), ncol = 1)
    } else {
      xstart <- matrix(runif(800, min = 0, max = 1), ncol = 1)
    }
    opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog",  control = list(btol = 1e-6))
    return(opt_x)
  }

  create_fun <- function(rm, budget) {
    fun <<- function(x){
      emis <- rep(eu_emissions_2019, 82)
      t <- 0:81
      year <- 2019:2100
      rr <- rep(initial_reduction_rate, 82)
      
      for(i in 2:82){
        if(rm == "rm1") {
          emis[i] <- emis[i-1] * (1 + x)
        } else if (rm == "rm2") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] * (1 + x))
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "rm3") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] + x)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "rm4") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * (year[i] - first_year)^2 + initial_reduction_rate)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "rm5") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "rm6") {
          emis[i] <- emis[i-1] + x
        }
      }
      
      emis <- make_linear(emis, rm = rm)
      emis <- make_horizontal(emis)

      ret <- numeric(1)
      ret[1] <- sum(emis[-1]) - eu_emission_budget_gt()
      return(ret)
    }
  }

  calculate_result <- function(x, rm) {
    emis <- rep(eu_emissions_2019, 82)
    t <- 0:81
    year <- 2019:2100
    rr <- rep(initial_reduction_rate, 82)
    for(i in 2:82){
      if(rm == "rm1") {
        emis[i] <- emis[i-1] * (1 + x)
      } else if (rm == "rm2") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] * (1 + x))
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm3") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] + x)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm4") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * (year[i] - first_year)^2 + initial_reduction_rate)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm5") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm6") {
        emis[i] <- emis[i-1] + x
      }
    }

    emis <- make_linear(emis, rm = rm)
    emis <- make_horizontal(emis)

    dat <- data.frame(t = t,
                      year = year,
                      emissions = emis,
                      rr = rr)
    return(dat)
  }

  plot_result <- function(x) {
    ggplot(x, aes(x = year, y = emissions)) +
      geom_line()
  }
  
  calculate_pathway <- function(rm, neg) {
    
    create_fun(rm = rm)
    
    bench::mark(
    opt_x <- optimize_function(fun, neg = neg)
    )
    
    # Mem: 68,5 | time: 32,3

    # if(neg == T) {
    #   result <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)],
    #                              rm = rm)
    # } else {
    #   result <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] > 0)],
    #                              rm = rm)
    # }

    # return(result)
    
  }
  
  # RM-1 const ####

  result <- reactive({
    if(input$selected_rm == "RM-1") {
      calculate_pathway(rm = "rm1", neg = T)
    } else if(input$selected_rm == "RM-2") {
      calculate_pathway(rm = "rm2", neg = F) # optimized value is positive
    } else if(input$selected_rm == "RM-3") {
      calculate_pathway(rm = "rm3", neg = T)
    } else if(input$selected_rm == "RM-4") {
      calculate_pathway(rm = "rm4", neg = T)
    } else if(input$selected_rm == "RM-5") {
      calculate_pathway(rm = "rm5", neg = T)
    } else if(input$selected_rm == "RM-6") {
      calculate_pathway(rm = "rm6", neg = T)
    }
  })
  
  observe(print(result()))
  
  # output$emis_pathway <- renderPlot({
  #   plot_result(result())
  # })
  
  
  
  
  # create_fun(rm = "rm1")
  # 
  # opt_x <- reactive({
  #   optimize_function(fun, neg = T)
  # })
  # 
  # result_rm1 <- reactive({
  #   calculate_result(x = opt_x()[[1]][which(opt_x()[[1]] < 0)],
  #                                rm = "rm1")
  # })
  # 
  # output$emis_pathway <- renderPlot({
  #   plot_result(result_rm1())
  # })


  # # RM-2 exp ####
  # 
  # create_fun(rm = "rm2")
  # 
  # opt_x <- reactive({
  #   optimize_function(fun, neg = F)
  # })
  # 
  # result_rm2 <- reactive({
  #   calculate_result(x = opt_x()[[1]][which(opt_x()[[1]] > 0)], # only allow positive x
  #                                rm = "rm2")
  # })
  # 
  # output$emis_pathway <- renderPlot({
  #   plot_result(result_rm2())
  # })
  # 
  # 
  # # RM-3 lin ####
  # 
  # create_fun(rm = "rm3")
  # 
  # opt_x <- reactive({
  #   optimize_function(fun, neg = T)
  # })
  # 
  # result_rm3 <- reactive({
  #   calculate_result(x = opt_x()[[1]][which(opt_x()[[1]] < 0)], # only allow negative x
  #                                rm = "rm3")
  # })
  # 
  # output$emis_pathway <- renderPlot({
  #   plot_result(result_rm3())
  # })
  # 
  # 
  # # RM-4 quadr ####
  # 
  # create_fun(rm = "rm4")
  # 
  # opt_x <- reactive({
  #   optimize_function(fun, neg = T)
  # })
  # 
  # result_rm4 <- reactive({
  #   calculate_result(x = opt_x()[[1]][which(opt_x()[[1]] < 0)], # only allow negative x
  #                                rm = "rm4")
  # })
  # 
  # output$emis_pathway <- renderPlot({
  #   plot_result(result_rm4())
  # })
  # 
  # 
  # # RM-5 rad ####
  # 
  # ## WIESO HIER CORRECTING FACTOR???
  # 
  # create_fun(rm = "rm5")
  # 
  # opt_x <- reactive({
  #   optimize_function(fun, neg = T)
  # })
  # 
  # result_rm5 <- reactive({
  #   calculate_result(x = opt_x()[[1]][which(opt_x()[[1]] < 0)], # only allow negative x
  #                                rm = "rm5")
  # })
  # 
  # output$emis_pathway <- renderPlot({
  #   plot_result(result_rm5())
  # })
  # 
  # 
  # # RM-6 abs ####
  # 
  # create_fun(rm = "rm6")
  # 
  # opt_x <- reactive({
  #   optimize_function(fun, neg = T)
  # })
  # 
  # result_rm6 <- reactive({
  #   calculate_result(x = opt_x()[[1]][which(opt_x()[[1]] < 0)], # only allow negative x
  #                                rm = "rm6")
  # })
  # 
  # output$emis_pathway <- renderPlot({
  #   plot_result(result_rm6())
  # })
  


  
  
  
}
