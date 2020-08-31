server <- function(input, output) {
  
  # Base data
  
  eu_population_share <- .058
  eu_emissions_share <- .072
  annual_global_emissions_gt <- 42.1 # in Gt
  eu_emissions_2019 <- 3.04 # Annual EU emissions in 2019 (Gt)
  emission_change_rate_2020 <- -.0217 # EU emission change between 2019 and 2020 (percent)
  
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
    input$max_negative_emissions_perc * eu_emissions_2019
  })
  
  # OPTIMIZATION ####
  
  set.seed(1234)
  
  
  
  f <- function (x, a) (a*x + 100)
  optimize(f, c(0, 1), tol = 0.0001, a = 1/3)
  
  
  
  
  
  
  # # Function to create the input function for the non-linear optimization
  # create_fun <- function(emis_by, emis_neg, budget) {
  #   fun <<- function(x){
  #     dat <- data.frame(emission = emis_by, t = 0:82)
  #     for(i in 1:83){
  #       dat$emission[i] <- dat$emission[i+1] * x
  #         
  #         # -(dat$t[i]+1)^(1/2)*x[1] - 
  #         # x[2]*(dat$t[i]+1)^(1/3) - x[3] - (dat$t[i]+1)^(1/4)*x[4] -
  #         # (dat$t[i]+1)^(1/5)*x[5]
  #     }
  #     ret <- numeric(1)
  #     ret[1] <- dat$emission[1] - emis_by
  #     # ret[2] <- dat$emission[83] + emis_neg
  #     ret[3] <- sum(dat$emission) - budget
  #     # ret[4] <- dat$emission[2] - emis_by * 0.984
  #     ret[5] <- min(dat$emission) + emis_neg
  #     return(ret)
  #   }
  # }
  
  
  # # Function to execute the linear optimization and to plot the results
  # create_output <- function(fun, emissions_baseyear, filename) {
  #   xstart <- matrix(runif(800, min=-0.3, max=0.3), ncol = 5)
  #   opt_x <- searchZeros(xstart, fun,  method="Broyden", global="pwldog", control=list(btol=1e-6))
  #   
  #   # check
  #   dat <- data.frame(emission = emissions_baseyear, t = 0:82, year = 2018:2100, type = filename)
  #   for(i in 1:83){
  #     dat$emission[i] <- -(dat$t[i]+1)^(1/2)*opt_x[[1]][1,1] -
  #       opt_x[[1]][1,2]*(dat$t[i]+1)^(1/3)- opt_x[[1]][1,3] -
  #       (dat$t[i]+1)^(1/4)*opt_x[[1]][1,4] -
  #       (dat$t[i]+1)^(1/5)*opt_x[[1]][1,5]
  #   }
  #   # reduction
  #   dat <- dat %>% 
  #     mutate(reduction = lag(emission) - emission)
  #   dat$reduction[1] <- emissions_baseyear*0.016
  
  # PATHWAY CALCULATIONS ####
  
  timespan_empty <- tibble(
    year =  2019:2100,
    abs_change = 0,
    reduction_rate = 0,
    emissions = 0
  ) %>%
    mutate(emissions = case_when(year == 2019 ~ eu_emissions_2019,
                                 TRUE ~  emissions))
    
  
  # RM-1 - constant annual reduction rate
  
  
  
}
