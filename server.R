server <- function(input, output) {
  
  # Base data
  
  first_year <- 2020 # First year for which emissions are calculated
  eu_population_share <- .058
  eu_emissions_share <- .072
  annual_global_emissions_gt <- 42.1 # in Gt
  eu_emissions_2019 <- 3.03870921601875 # Annual EU emissions in 2019 (Gt)
  threshold_linear_rm1 <- 0.136741914720844000 # Threshold from when on the path becomes linear (rm1)
  threshold_linear_other <- 0.106354822560656 # Threshold from when on the path becomes linear (all other rms)
  initial_reduction_rate <- -0.0217324116377584 # Emission reduction rate to start with (in RM 2-5); is EU emission change between 2019 and 2020 (percent)
  eu_past_emissions <- data.frame(
    year = seq(2010, 2019, 1),
    emissions = c(3.347, 3.249, 3.158, 3.070, 2.954, 3.021, 3.043, 3.112, 3.039, eu_emissions_2019),
    historical = "y"
  )
  
  global_emission_budget_gt <- reactive({
    input$global_emission_budget_gt_2018 - 2 * annual_global_emissions_gt
  })
  
  output$global_budget <- renderTable(
    data.frame(x = c("Emissionen der Jahre 2018 und 2019: ", "Globales Budget ab 2020: "), 
               y = c(paste0(2 * annual_global_emissions_gt, " Gt CO2"), paste0(global_emission_budget_gt(), " Gt CO2"))),
    colnames = F, align = c("lr")
  )
  
  output$weighted_key <- renderTable(
    data.frame(x = c("Anteil der EU an den globalen Emissionen", "Anteil der EU an der globalen Bevölkerung", "Gewichtungsschlüssel", "EU-Emissionsbudget"),
               y = c("7,2%", "5,8%", paste0(round(weighted_key() * 100, 1), "%"),
                     paste0(round(eu_emission_budget_gt(), 1), " Gt"))),
    colnames = F
  )
  
  output$negative_emissions <- renderTable(
    data.frame(x = "Maximal mögliche Netto-Negativemissionen: ",
               y = paste0(round(max_negative_emissions_gt()*-1, 2), " Gt")),
    colnames = F
  )
  
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
      ret[1] <- sum(emis[-1]) - budget
      # ret[2] <- abs(sum(rr)) - sum(abs(rr))
      return(ret)
    }
  }
  
  optimize_function <- function(fun, neg) {
    
    if(neg == T) {
      xstart <- matrix(runif(20, min = -1, max = 0), ncol = 1)
    } else {
      xstart <- matrix(runif(20, min = 0, max = 1), ncol = 1)
    }
    
    opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog") #,  control = list(btol = 1e1, ftol = 1e-8, xtol = 1e1, cndtol = 1e1))
    
    return(opt_x)
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
                      rr = rr, 
                      rm = rm)
    return(dat)
  }
  
  plot_result <- function(x) {
    if(nrow(x) == 0) {
      ggplot() + 
        theme_classic() + 
        geom_label(aes(2060, 1.5, family = "sans-serif", color = "red",
                       label = "Keine Lösung für diesen Funktionstyp - bitte andere Einstellungen wählen!"), size = 3) +
        xlim(2010, 2100) +
        ylim(0, 3) +
        theme(text = element_text(size = 10, family = "sans-serif"),
              legend.position = "none") +
        labs(x = "Jahr", y = "Emissionen (Gt)")
    } else {
      total_emissions <- round(sum(x$emissions[-1]), 1)
      year_zero_emissions <- x$year[which(x$emissions <= 0)[1]]
      ggplot(x, aes(x = year, y = emissions, col = rm)) +
        geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
        geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
        geom_line() +
        geom_point_interactive(aes(tooltip = paste0(year, ": ", round(emissions, 1), " Gt"), data_id = year)) +
        geom_point_interactive(data = eu_past_emissions, color = "grey", 
                               aes(x = year, y = emissions, data_id = year,
                                   tooltip = paste0(year, ": ", round(emissions, 1), " Gt"))) +
        geom_label(aes(2045, 1.5, hjust = 0,
                       label = paste0("Gesamtemissionen: ", total_emissions, " Gt", "\n", "Netto-Nullemissionen im Jahr ", year_zero_emissions)), 
                   size = 3) +
        theme_classic() +
        scale_x_continuous(breaks = scales::extended_breaks(n = 9)(2010:2100)) +
        scale_y_continuous(breaks = scales::extended_breaks(n = 9)(-0.5:3.5)) +
        labs(x = "Jahr", y = "Emissionen (Gt)")
    }
  }
  
  calculate_pathway <- function(rm) {
    
    output <- data.frame(t = numeric(), year = numeric(), emissions = numeric(), rr = numeric(), rm = character())
    
    withProgress(message = 'Aktualisierung', value = 0, {
      
      for(i in 1:length(rm)) {
        
        neg <- ifelse(rm[i] == "rm2", F, T)
        
        opt_x <- NULL
        
        for(j in c(1, 0.95, 1.05)) { 
          
          # Slightly vary the budget so that an optimum is found
          
          if(is.null(opt_x)) {
            
            create_fun(rm = rm[i], budget = eu_emission_budget_gt() * j)
            
            opt_x <- optimize_function(fun, neg = neg)
            
          } else {
            # Do nothing
          }
          
        }
        
        if(is.null(opt_x)) {
          result <- NULL
        } else {
          result <- calculate_result(x = opt_x[[1]], rm = rm[i])
          
          # if(neg == T) {
          #   result <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)],
          #                              rm = rm[i])
          # } else {
          #   result <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] > 0)],
          #                              rm = rm[i])
          # }
          
        }
        
        if(is.null(result)) {
          # do nothing
        } else {
          output <- rbind(output, result)
        }
        
        incProgress(1/length(rm), detail = paste("Erstelle Funktion", i))
        
      }
      
    }) # end withProgress
    
    return(output)
    
  }
  
  result <- eventReactive(input$go, {
    calculate_pathway(rm = input$selected_rm)
  })
  
  output$emis_pathway <- renderGirafe({
      girafe(ggobj = plot_result(result()), width_svg = 7, height_svg = 4) %>%
        girafe_options(opts_hover(css = "fill:wheat; stroke:orange;;"))
  })
  
}
