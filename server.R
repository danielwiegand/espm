server <- function(input, output) {
  
  # Base data
  
  first_year <- 2020 # First year for which emissions are calculated
  eu_population_share <- .058
  eu_emissions_share <- .072
  annual_global_emissions_gt <- 42.1 # in Gt
  eu_emissions_2019 <- 3.03870921601875 # Annual EU emissions in 2019 (Gt)
  eu_emissions_1990 <- 3.751043368376680 # Annual EU emissions in 1990 (Gt)
  threshold_linear_rm1 <- 0.136741914720844000 # Threshold from when on the path becomes linear (rm1)
  threshold_linear_other <- 0.106354822560656 # Threshold from when on the path becomes linear (all other rms)
  initial_reduction_rate <- -0.0217324116377584 # Emission reduction rate to start with (in RM 2-5); is EU emission change between 2019 and 2020 (percent)
  eu_past_emissions <- data.frame(
    year = seq(2010, 2019, 1),
    emissions = c(3.347, 3.249, 3.158, 3.070, 2.954, 3.021, 3.043, 3.112, 3.039, eu_emissions_2019),
    historical = "y"
  )
  date_display_range <- reactive({
    if(input$date_display_range == T) {2100} else {2050}
  })
  colors_to_display <- reactive({
    data.frame("rm" = c("RM-1", "RM-2", "RM-3", "RM-4", "RM-5", "RM-6"),
               "color" = c("#4f81bd", "#c0504d", "#9bbb59", "#000000", "#f79646", "#808080")) %>%
      filter(rm %in% input$selected_rm) %>%
      select(color) %>% as.list()
  }) 
  
  observe(print(colors_to_display()))
  
  global_emission_budget_gt <- reactive({
    input$global_emission_budget_gt_2018 - 2 * annual_global_emissions_gt
  })
  
  output$global_budget <- renderTable(
    data.frame(x = c("Emissions of 2018 and 2019: ", "Global budget from 2020 on: "), 
               y = c(paste0(2 * annual_global_emissions_gt, " Gt CO2"), paste0(global_emission_budget_gt(), " Gt CO2"))),
    colnames = F, align = c("lr")
  )
  
  output$weighted_key <- renderTable(
    data.frame(x = c("EU share of global emissions", "EU share of global population", "Weighted key", "EU emission budget"),
               y = c("7,2%", "5,8%", paste0(round(weighted_key() * 100, 1), "%"),
                     paste0(round(eu_emission_budget_gt(), 1), " Gt"))),
    colnames = F
  )
  
  output$negative_emissions <- renderTable(
    data.frame(x = "Maximum possible negative emissions (p.a.): ",
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
      if(rm == "RM-1") {
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
        if(rm == "RM-1") {
          emis[i] <- emis[i-1] * (1 + x)
        } else if (rm == "RM-2") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] * (1 + x))
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "RM-3") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] + x)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "RM-4") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * (year[i] - first_year)^2 + initial_reduction_rate)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "RM-5") {
          rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "RM-6") {
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
      xstart <- matrix(runif(20, min = -1, max = 0), ncol = 1)
    } else {
      xstart <- matrix(runif(20, min = 0, max = 1), ncol = 1)
    }
    
    opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog")
    
    return(opt_x)
  }
  
  calculate_result <- function(x, rm) {
    emis <- rep(eu_emissions_2019, 82)
    t <- 0:81
    year <- 2019:2100
    rr <- rep(initial_reduction_rate, 82)
    for(i in 2:82){
      if(rm == "RM-1") {
        emis[i] <- emis[i-1] * (1 + x)
      } else if (rm == "RM-2") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] * (1 + x))
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-3") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, rr[i-1] + x)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-4") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * (year[i] - first_year)^2 + initial_reduction_rate)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-5") {
        rr[i] <- ifelse(year[i] == 2020, initial_reduction_rate, x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-6") {
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
      
      total_emissions <- round(sum(x$emissions[-1]), 1)
      year_zero_emissions <- x$year[which(x$emissions <= 0)[1]]
      overshoot_amounts <- x %>%
        rename("RM" = rm) %>%
        filter(emissions < 0) %>%
        group_by(RM) %>%
        summarize("Overshoot (Gt)" = round(sum(emissions) * -1, 2)) 
      
      x %>%
        rownames_to_column("data_id") %>%
        filter(year <= date_display_range()) %>%
        ggplot(aes(x = year, y = emissions, color = rm)) +
        geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
        geom_vline(xintercept = 2019.5, color = "grey", linetype = "dashed") +
        geom_line_interactive(aes(data_id = rm, hover_css = "fill:none;", tooltip = rm)) +
        geom_point_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(emissions, 2), " Gt"), data_id = data_id), 
                               size = 0.6) +
        geom_point_interactive(data = eu_past_emissions, color = "grey", size = 1,
                               aes(x = year, y = emissions, data_id = year,
                                   tooltip = paste0(year, ": ", round(emissions, 2), " Gt"))) +
        annotation_custom(tableGrob(overshoot_amounts, rows = NULL, theme = ttheme_minimal(base_size = 7, padding = unit(c(2, 2), "mm"))), 
                          xmin = 2035, xmax = 2050, ymin = 1.5, ymax = 3) +
        theme_classic() +
        scale_x_continuous(breaks = scales::extended_breaks(n = 9)(2010:2100)) +
        scale_y_continuous(breaks = scales::extended_breaks(n = 9)(-0.5:3.5)) +
        labs(x = "Year", y = "Emissions (Gt)", color = "Scenario type", subtitle = "Emissions over time") +
        theme(text = element_text(size = 10)) +
        scale_color_brewer(palette = "YlOrRd")
    }
  }
  
  calculate_pathway <- function(rm) {
    
    output <- data.frame(t = numeric(), year = numeric(), emissions = numeric(), rr = numeric(), rm = character())
    
    withProgress(message = 'Aktualisierung', value = 0, {
      
      for(i in 1:length(rm)) {
        
        neg <- ifelse(rm[i] == "RM-2", F, T)
        
        opt_x <- NULL
        is_steady <- T
        
        # Slightly vary the budget so that an optimum is found for a steady function
        for(j in c(1, 0.95, 1.05, 0.9, 1.1, 0.85, 1.15, 0.8, 1.2, 0.75, 1.25)) {  
          
          if(is.null(opt_x) | is_steady == F) {
            
            create_fun(rm = rm[i], budget = eu_emission_budget_gt() * j)
            
            opt_x <- optimize_function(fun, neg = neg)
            
            if(is.null(opt_x)) {
              
              result <- NULL
              
            } else {
              
              result <- calculate_result(x = opt_x[[1]], rm = rm[i])
              
              is_steady <- abs(sum(result$rr)) == sum(abs(result$rr)) 
              # This condition checks if the function is steady
              # If it is not steady, the loop continues
            }
          }
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
  }, ignoreNULL = F) # Fire also at startup
  
  output$emis_pathway <- renderGirafe({
    girafe(ggobj = plot_result(result()), width_svg = 7, height_svg = 2.5) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"))
  })
  
  observe(print(input$date_display_range))
  
  # Bar plot: Change compared to 1990
  
  comparison_1990 <- reactive({
    result() %>%
      filter(year %in% c(2020, 2030, 2035, 2040, 2050)) %>%
      mutate(change = (1 - emissions / eu_emissions_1990) * -100,
             year = as.character(year),
             is_2030 = ifelse(year == 2030, "Y", "N")) %>%
      rownames_to_column("data_id") %>%
      ggplot(aes(x = year, y = change, fill = year, color = is_2030)) +
      geom_col_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(change, 1), "%"), data_id = data_id), width = .7) +
      facet_wrap(~rm) +
      theme_classic() +
      labs(y = "Change (%)", x = "", fill = "Scenario type", subtitle = "Change compared to 1990") +
      theme(text = element_text(size = 12),
            axis.text.x = element_text(size = 6),
            legend.position = "none") +
      scale_color_manual(values = c(colors_to_display()$color)) +
      # scale_fill_brewer(palette = "YlOrRd") +
      scale_color_manual(values = c(NA, "black"))
  })
  
  output$comparison_1990 <- renderGirafe(
    girafe(ggobj = comparison_1990(), width_svg = 4, height_svg = 3) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"))
  )
  
  # Line plot: Emission change rate
  
  emission_change_rates <- reactive({
    result() %>%
      mutate(rr_eff = ifelse(emissions > threshold_linear_other, (emissions / lag(emissions) -1) * 100, 0)) %>%
      rownames_to_column("data_id") %>%
      filter(year <= date_display_range() & year > 2019) %>%
      mutate(rr = rr * 100) %>%
      ggplot(aes(x = year, y = rr_eff, color = rm)) +
      geom_line_interactive(aes(data_id = rm, hover_css = "fill:none;", tooltip = rm)) +
      geom_point_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(rr_eff, 2), " %"), data_id = data_id), 
                             size = 0.6) +
      theme_classic() +
      scale_x_continuous(breaks = scales::extended_breaks(n = 8)(2020:2100)) +
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none") +
      labs(x = "", y = "Change (%)", subtitle = "Emission change rate")
  })
  
  output$emission_change_rates <- renderGirafe(
    girafe(ggobj = emission_change_rates(), width_svg = 3.8, height_svg = 2.8) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"))
  )
  
  
  # # Bar plot: Overshoot amounts per RM
  # 
  # overshoot_amounts <- reactive({
  #   result() %>%
  #     filter(emissions < 0) %>%
  #     group_by(rm) %>%
  #     summarize(overshoot = sum(emissions) * -1) %>%
  #     rownames_to_column("data_id") %>%
  #     ggplot(aes(x = rm, y = overshoot, fill = rm)) +
  #     geom_col_interactive(aes(data_id = data_id, tooltip = paste0(rm, ": ", round(overshoot, 1), " Gt")), width = 0.7) +
  #     theme_classic() +
  #     labs(y = "Overshoot 2020-2100 (Gt)", x = "", fill = "Scenario type") +
  #     theme(text = element_text(size = 12),
  #           axis.text.x = element_text(size = 6),
  #           legend.position = "none") +
  #     scale_fill_brewer(palette = "YlOrRd")
  # })
  # 
  # output$overshoot_amounts <- renderGirafe(
  #   girafe(ggobj = overshoot_amounts(), width_svg = 4, height_svg = 3) %>%
  #     girafe_options(opts_hover(css = "fill:black; stroke:black;"))
  # )
  
  # Notifications
  
  observeEvent(input$link_info_scenario_type, {
    shinyjs::toggle("info_scenario_type")
  })

  output$box_info_scenario_type <- renderUI({
    hidden(div(class = "info-box", id = "info_scenario_type", "Get more information on scenario types on", tags$a(href = "https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf", "this webpage.")))
  })
  
}
