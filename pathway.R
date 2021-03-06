"Functions to calculate an emission pathway"

make_linear <- function(x, rm) {
  for(i in 3:length(x)) {
    if(rm == "RM-1 const") {
      if(x[i-1] <= THRESHOLD_LINEAR_RM1) {
        x[i] <- x[i-1] + x[i-1] - x[i-2]
      }
    } else {
      if(x[i-1] <= THRESHOLD_LINEAR_OTHER) {
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

create_fun <- function(rm, budget, init_rr) {
  
  fun <<- function(x){
    emis <- rep(EU_EMISSIONS_2019, 82)
    t <- 0:81
    year <- 2019:2100
    rr <- rep(init_rr, 82)
    
    for(i in 2:82){
      if(rm == "RM-1 const") {
        emis[i] <- emis[i-1] * (1 + x)
      } else if (rm == "RM-2 exp") {
        rr[i] <- ifelse(year[i] == 2020, init_rr, rr[i-1] * (1 + x))
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-3 lin") {
        rr[i] <- ifelse(year[i] == 2020, init_rr, rr[i-1] + x)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-4 quadr") {
        rr[i] <- ifelse(year[i] == 2020, init_rr, x * (year[i] - FIRST_YEAR)^2 + init_rr)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-5 rad") {
        rr[i] <- ifelse(year[i] == 2020, init_rr, x * sqrt(year[i] - 0.5 - FIRST_YEAR) + init_rr)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-6 abs") {
        emis[i] <- emis[i-1] + x
      }
    }
    
    emis <- make_linear(emis, rm = rm)
    emis <- make_horizontal(emis)
    
    ret <- numeric(1)
    ret[1] <- sum(emis[-1]) - budget
    # ret[2] <- abs(sum(rr)) - sum(abs(rr)) # Condition to have a steady function - does not work
    return(ret)
  }
}

optimize_function <- function(fun, neg) {
  
  if(neg == T) {
    xstart <- matrix(runif(5, min = -1, max = 0), ncol = 1)
  } else {
    xstart <- matrix(runif(5, min = 0, max = 1), ncol = 1)
  }
  
  opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog")
  
  return(opt_x)
}

calculate_result <- function(x, rm, init_rr) {
  emis <- rep(EU_EMISSIONS_2019, 82)
  t <- 0:81
  year <- 2019:2100
  rr <- rep(init_rr, 82)
  
  for(i in 2:82){
    if(rm == "RM-1 const") {
      emis[i] <- emis[i-1] * (1 + x)
    } else if (rm == "RM-2 exp") {
      rr[i] <- ifelse(year[i] == 2020, init_rr, rr[i-1] * (1 + x))
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "RM-3 lin") {
      rr[i] <- ifelse(year[i] == 2020, init_rr, rr[i-1] + x)
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "RM-4 quadr") {
      rr[i] <- ifelse(year[i] == 2020, init_rr, x * (year[i] - FIRST_YEAR)^2 + init_rr)
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "RM-5 rad") {
      rr[i] <- ifelse(year[i] == 2020, init_rr, x * sqrt(year[i] - 0.5 - FIRST_YEAR) + init_rr)
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "RM-6 abs") {
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
                     label = "No solution for this scenario type - please choose other options!"), size = 3) +
      xlim(2010, 2100) +
      ylim(0, 3) +
      theme(text = element_text(size = 10, family = "sans-serif"),
            legend.position = "none") +
      labs(x = "Year", y = "Emissions (Gt)")
    
  } else {
    
    year_zero_emissions <- x$year[which(x$emissions <= 0)[1]]
    
    column_colors <- reactive({
      # Specify which cells should have red font color in overshoot_amounts()
      out = matrix(rep("black", 4), ncol = 4, nrow = nrow(overshoot_amounts()), byrow = TRUE)
      out[which(overshoot_amounts()$Overshoot > 0), 3] <- "red"
      return(out)
    })
    
    x %>%
      rownames_to_column("data_id") %>%
      filter(year <= date_display_range()) %>%
      ggplot(aes(x = year, y = emissions, color = rm)) +
      geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
      geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
      geom_line_interactive(aes(data_id = rm, hover_css = "fill:none;", tooltip = rm)) +
      geom_point_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(emissions, 2), " Gt"), data_id = data_id),
                             size = 0.6) +
      geom_point_interactive(data = EU_PAST_EMISSIONS, color = "grey", size = 1,
                             aes(x = year, y = emissions, data_id = year,
                                 tooltip = paste0(year, ": ", round(emissions, 2), " Gt"))) +
      annotation_custom(tableGrob(overshoot_amounts(), rows = NULL, theme = ttheme_minimal(base_size = 6, 
                                                                                           # Font colors per column
                                                                                           core = list(fg_params = list(col = as.vector(column_colors()))),
                                                                                           padding = unit(c(2, 2), "mm"))),
                        xmin = ifelse(date_display_range() == 2100, 2060, 2035), ymin = 1.5, ymax = 3) +
      theme_classic() +
      scale_x_continuous(breaks = scales::extended_breaks(n = 9)(2010:2100)) +
      scale_y_continuous(breaks = scales::extended_breaks(n = 9)(-0.5:3.5)) +
      labs(x = "Year", y = "Emissions (Gt)", color = "Scenario type", subtitle = "Emissions over time") +
      theme(text = element_text(size = 10),
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 6),
            legend.key.size = unit(.4, "cm")) +
      scale_color_manual(values = c(colors_to_display()))
  }
}

calculate_pathway <- function(rm) {
  
  output <- data.frame(t = numeric(), year = numeric(), emissions = numeric(), rr = numeric(), rm = character())
  
  withProgress(message = 'Update...', value = 0, {
    
    rms_with_no_result <- NULL
    
    for(i in 1:length(rm)) {
      
      neg <- ifelse(rm[i] == "RM-2 exp", F, T)
      
      opt_x <- NULL
      result <- NULL
      sequence <- 1 - rep(seq(0.01, 0.05, 0.01), each = 2) * rep(c(1, -1))
      set.seed(3)
      
      # Slightly vary the budget so that an optimum is found for a steady function
      for(j in sequence) {
        
        create_fun(rm = rm[i], budget = eu_emission_budget_gt() * j, init_rr = INITIAL_REDUCTION_RATE)
        
        opt_x <- optimize_function(fun, neg = neg)
        
        if(!is.null(opt_x)) {
          
          result <- calculate_result(x = opt_x[[1]], rm = rm[i], init_rr = INITIAL_REDUCTION_RATE)
          
          is_steady <- abs(sum(result$rr)) == sum(abs(result$rr)) 
          
          if(is_steady == T) {
            # This condition checks if the function is steady
            # If it is not steady, the loop continues
            break
          }
          
        }
      }
      
      if(is.null(result) | is_steady == F) {
        rms_with_no_result <- c(rms_with_no_result, rm[i])
      } else {
        output <- rbind(output, result)
      }
      
      incProgress(1/length(rm), detail = paste("Optimize scenario type", i))
    }
    
  }) # end withProgress
  
  if(length(rms_with_no_result) > 0) {
    showNotification(paste0("No result was found for ", stringr::str_flatten(rms_with_no_result, collapse = ", ")), type = "error")
  }
  
  return(output)
  
}