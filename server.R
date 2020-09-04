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
  
  # calculate_rolling_rms
  cppFunction('
        NumericVector calculate_rolling_rms(String rm, NumericVector emis, NumericVector year, double x, double first_year, double initial_reduction_rate, NumericVector rr){
        for (int i = 2; i < 83; ++i){
          if(rm == "rm1") {
            emis[i] = emis[i-1] * (1 + x);
          } else if(rm == "rm2") {
            if(year[i] == first_year) {
              rr[i] = initial_reduction_rate;
            } else {
              rr[i] = rr[i-1] * (1 + x);
            }
            emis[i] = emis[i-1] * (1 + rr[i]);
          } else if(rm == "rm3") {
            if(year[i] == first_year) {
              rr[i] = initial_reduction_rate;
            } else {
            /*######################################*/
              rr[i] = rr[i-1] + x;
            /*######################################*/
            }
            emis[i] = emis[i-1] * (1 + rr[i]);
          } else if(rm == "rm4") {
            if(year[i] == first_year) {
              rr[i] = initial_reduction_rate;
            } else {
              rr[i] = x * pow((year[i] - first_year), 2) + initial_reduction_rate;
            }
            emis[i] = emis[i-1] * (1 + rr[i]);
          } else if(rm == "rm5") {
            if(year[i] == first_year) {
              rr[i] = initial_reduction_rate;
            } else {
              rr[i] = x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate;
            }
            emis[i] = emis[i-1] * (1 + rr[i]);
          } else if(rm == "rm6") {
            emis[i] = emis[i-1] + x;
          }
        }
        return(emis);
        }
      ')
  
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
      
      # VARIANTE 1: SCHLEIFEN
      
      # for(i in 2:82){
      #   if(rm == "rm1") {
      #     emis[i] <- emis[i-1] * (1 + x)
      #   } else if (rm == "rm2") {
      #     rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] * (1 + x))
      #     emis[i] <- emis[i-1] * (1 + rr[i])
      #   } else if (rm == "rm3") {
      #     rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] + x)
      #     emis[i] <- emis[i-1] * (1 + rr[i])
      #   } else if (rm == "rm4") {
      #     rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * (year[i] - first_year)^2 + initial_reduction_rate)
      #     emis[i] <- emis[i-1] * (1 + rr[i])
      #   } else if (rm == "rm5") {
      #     rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate)
      #     emis[i] <- emis[i-1] * (1 + rr[i])
      #   } else if (rm == "rm6") {
      #     emis[i] <- emis[i-1] + x
      #   }
      # }
      
      # VARIANTE 2: RCPP-FUNKTION
      calculate_rolling_rms(rm = rm, 
                            emis = emis, 
                            year = year, 
                            x = x, 
                            first_year = first_year,
                            initial_reduction_rate = initial_reduction_rate,
                            rr = rr
      )
      
      emis <- make_linear(emis, rm = rm)
      emis <- make_horizontal(emis)
      
      ret <- numeric(1)
      ret[1] <- sum(emis[-1]) - eu_emission_budget_gt()
      return(ret)
    }
  }

  optimize_function <- function(fun, neg) {
    if(neg == T) {
      xstart <- matrix(runif(10, min = -1, max = 0), ncol = 1)
    } else {
      xstart <- matrix(runif(10, min = 0, max = 1), ncol = 1)
    }
    opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog",  control = list(btol = 1e-6))
    
    return(opt_x)
  }
  
  calculate_pathway <- function(rm, neg) {
    
    withProgress(message = 'Aktualisierung', value = 0, {
      
      incProgress(0, detail = "Erstellen der Funktion")
    
      create_fun(rm = rm)
        
      incProgress(0.30, detail = "Optimieren der Funktion")
      
      opt_x <- optimize_function(fun, neg = neg)
      
      # Benchmark
        # Normal: Mem: 27mb | time: 58,5
        # RCPP einzeln: Mem: 125mb | time: 25,7
        # RCPP einmalig: Mem: NA | time: 17,7
      
      incProgress(0.80, detail = "Erstellen der Grafik")
      
      if(is.null(opt_x)) {
        result <- NULL
      } else {
        if(neg == T) {
          result <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)],
                                     rm = rm)
        } else {
          result <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] > 0)],
                                     rm = rm)
        }
      }
    
    })

    return(result)
    
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
    if(is.null(x)) {
      ggplot() + 
        theme_classic() + 
        geom_label(aes(2060, 1.5, family = "sans-serif", color = "red",
                       label = "Keine Lösung für diesen Funktionstyp - bitte andere Einstellungen wählen!"), size = 3) +
        xlim(2020, 2100) +
        ylim(0, 3) +
        theme(text = element_text(size = 10, family = "sans-serif"),
              legend.position = "none") +
        labs(x = "Jahr", y = "Emissionen (Gt)")
    } else {
      total_emissions <- round(sum(x$emissions[-1]), 1)
      year_zero_emissions <- x$year[which(x$emissions < 0)[1]]
      ggplot(x, aes(x = year, y = emissions)) +
        geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
        geom_line() +
        geom_point_interactive(aes(tooltip = paste0(year, ": ", round(emissions, 1), " Gt"))) +
        geom_label(aes(2045, 1.5, hjust = 0,
                        label = paste0("Gesamtemissionen: ", total_emissions, " Gt", "\n", "Netto-Nullemissionen im Jahr ", year_zero_emissions)), 
                   size = 3) +
        theme_classic() +
        labs(x = "Jahr", y = "Emissionen (Gt)")
    }
  }
  
  result <- eventReactive(input$go, {
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
  
  output$emis_pathway <- renderGirafe({
    girafe(ggobj = plot_result(result()), width_svg = 7, height_svg = 4)
  })
  
}
