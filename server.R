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
  initial_reduction_rate <- -0.02 # Emission reduction rate to start with (in RM 2-5); is assumed EU emission change between 2019 and 2020 (percent)
  eu_past_emissions <- data.frame(
    year = seq(2010, 2019, 1),
    emissions = c(3.347, 3.249, 3.158, 3.070, 2.954, 3.021, 3.043, 3.112, 3.039, eu_emissions_2019),
    historical = "y"
  )
  
  date_display_range <- reactive({
    if(input$date_display_range == T) {2100} else {2050}
  })
  
  colors_to_display <- eventReactive(input$go, ignoreNULL = F, {
    a <- c("RM-1 const" = "#4b8abd", "RM-2 exp" = "#b9594d", "RM-3 lin" = "#a7b25d", "RM-4 quadr" = "#7970a2", "RM-5 rad" = "#f1974b", "RM-6 abs" = "#818181")
    b <- a[input$selected_rm]
  }) 
  
  output$title <- renderUI({
    output = tagList()
    output[[1]] <- HTML("Extended Smooth Pathway Model (ESPM)<br><span style = 'font-size:20px;'>Calculating Paris compatible emission paths and targets using the example of the EU</span>")
    output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
    output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
    HTML("The Extended Smooth Pathway Model (ESPM) is a model to determine 
    emission paths which are in line with the Paris Agreement. It consists of two calculation steps: Determination 
    of a <b>national budget</b> and derivation of plausible <b>national emission paths</b> from this budget.<br /><br />
    This app focuses on the EU. A weighting model is offered to determine its emission budget. The 
    weighting is based on the EU's share of global emissions and of global population. The weighted 
    key is then applied to the global budget to determine the EU's budget 2020 - 2100. <br /><br />The scenario 
    types used to determine the emission paths differ in their assumptions about the annual emission 
    changes (see plot 'Annual emission change rates').<br /> <br />
    An important question concerns the possibility of future <b>negative emissions</b>. The app allows you specify the 
    potential for net negative emissions. Please refer to the notes in the corresponding help text.<br /><br />
    A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target = '_blank'>here</a>. More information about the ESPM and other tools at: <a href = 'http://www.save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>.<br /><br />"),
    actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
    
    return(output)
  })
  
  showModal(modalDialog(
    title = "The Extended Smooth Pathway Model (ESPM)",
    HTML("The Extended Smooth Pathway Model (ESPM) is a model to determine emission paths which are in line with the Paris Agreement.
    It consists of two calculation steps: Determination of a <b>national budget</b> and derivation of plausible <b>national emission paths</b> from this budget.<br /><br />
    This app focuses on the EU. A weighting model is offered to determine its emission budget. The weighting is based on the EU's share
    of global emissions and of global population. The weighted key is then applied to the global budget to determine the EU's budget 2020 - 2100.<br /><br />
    The scenario types used to determine the emission paths differ in their assumptions about the annual emission changes (see plot 'Annual emission change rates').<br /><br />
    An important question concerns the possibility of future <b>negative emissions</b>. The app allows you specify the potential for net negative emissions. Please refer to the notes in the corresponding help text.<br /><br />
    A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target = '_blank'>here</a>. More information about the ESPM and other tools at: <a href = 'http://www.save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>."),
    easyClose = FALSE,
    footer = modalButton("Close")
  ))
  
  # temperature_budget_relation <- tibble(
  #   temperature = c(1.5, 1.57, 1.6, 1.67, 1.75),
  #   budget = c(420, 530, 570, 680, 800)
  # )
  # 
  # global_emission_budget_gt <- reactive({
  #   temperature_budget_relation$budget[temperature_budget_relation$temperature == input$temperature_increase]  - 2 * annual_global_emissions_gt
  # })
  
  global_emission_budget_gt <- reactive({
    input$global_emission_budget_gt_2018 - 2 * annual_global_emissions_gt
  })
  
  output$global_budget <- renderTable(
    data.frame(x = c("Emissions of 2018 and 2019: ", "Global budget from 2020 on: "), 
               y = c(paste0(2 * annual_global_emissions_gt, " Gt CO2"), paste0(global_emission_budget_gt(), " Gt CO2"))),
    colnames = F, align = c("lr")
  )
  
  output$weighted_key <- renderTable(
    data.frame(x = c("EU share of global emissions", "EU share of global population", "Weighted key", "EU emission budget from 2020 on"),
               y = c("7.2%", "5.8%", paste0(round(weighted_key() * 100, 1), "%"),
                     paste0(round(eu_emission_budget_gt(), 1), " Gt CO2"))),
    colnames = F
  )
  
  output$negative_emissions <- renderTable(
    data.frame(x = "Maximum possible net negative emissions (p.a.): ",
               # THIS IS ROUNDED TO ONLY ONE DECIMAL PLACE WHICH DECREASES ACCURACY TO MAKE IT COMPATIBLE TO RESULTS IN THE ESPM PAPER. CAN BE CHANGED BACK IN FUTURE.
               y = paste0(round(max_negative_emissions_gt()*-1, 1), " Gt")), 
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
    # THIS IS ROUNDED TO ONLY ONE DECIMAL PLACE WHICH DECREASES ACCURACY TO MAKE IT COMPATIBLE TO RESULTS IN THE ESPM PAPER. CAN BE CHANGED BACK IN FUTURE.
    round(input$max_negative_emissions_perc / 100 * eu_emissions_2019 * -1, 1)
  })
  
  
  # Overshoot amounts
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
  
  
  # PATHWAY CALCULATION
  
  make_linear <- function(x, rm) {
    for(i in 3:length(x)) {
      if(rm == "RM-1 const") {
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
  
  create_fun <- function(rm, budget, init_rr) {
    
    fun <<- function(x){
      emis <- rep(eu_emissions_2019, 82)
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
          rr[i] <- ifelse(year[i] == 2020, init_rr, x * (year[i] - first_year)^2 + init_rr)
          emis[i] <- emis[i-1] * (1 + rr[i])
        } else if (rm == "RM-5 rad") {
          rr[i] <- ifelse(year[i] == 2020, init_rr, x * sqrt(year[i] - 0.5 - first_year) + init_rr)
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
    emis <- rep(eu_emissions_2019, 82)
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
        rr[i] <- ifelse(year[i] == 2020, init_rr, x * (year[i] - first_year)^2 + init_rr)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "RM-5 rad") {
        rr[i] <- ifelse(year[i] == 2020, init_rr, x * sqrt(year[i] - 0.5 - first_year) + init_rr)
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
        geom_point_interactive(data = eu_past_emissions, color = "grey", size = 1,
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
          
          create_fun(rm = rm[i], budget = eu_emission_budget_gt() * j, init_rr = initial_reduction_rate)
          
          opt_x <- optimize_function(fun, neg = neg)
          
          if(!is.null(opt_x)) {
            
            result <- calculate_result(x = opt_x[[1]], rm = rm[i], init_rr = initial_reduction_rate)
            
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
  
  result <- eventReactive(input$go, {
    calculate_pathway(rm = input$selected_rm)
  }, ignoreNULL = F) # Fire also at startup
  
  output$emis_pathway <- renderGirafe({
    girafe(ggobj = plot_result(result()), width_svg = 7, height_svg = 2.5) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"),
                     opts_selection(type = "none"))
  })
  
  # Plots ####
  
  # Bar plot: Change compared to 1990
  
  comparison_1990 <- reactive({
    if(nrow(result()) == 0) {
      ggplot() + 
        theme_classic() + 
        geom_label(aes(2060, 1.5, family = "sans-serif", color = "red",
                       label = "No solution for this scenario type!"), size = 3) +
        xlim(2010, 2100) +
        ylim(0, 3) +
        theme(text = element_text(size = 10, family = "sans-serif"),
              legend.position = "none") +
        labs(x = "Year", y = "Emissions (Gt)")
      
    } else {
      result() %>%
        filter(year %in% c(2030, 2035, 2040, 2050)) %>%
        mutate(change = (1 - emissions / eu_emissions_1990) * -100,
               year = as.character(year),
               is_2030 = ifelse(year == 2030, "Y", "N")) %>%
        rownames_to_column("data_id") %>%
        ggplot(aes(x = year, y = change, fill = rm, color = is_2030)) +
        geom_col_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(change, 1), "%"), data_id = data_id), width = .7) +
        facet_wrap(~rm) +
        # geom_label(size = 1.5, aes(label = paste0(round(change, 0), "%")), nudge_y = 20, color = "black") +
        geom_text(size = 2, aes(label = paste0(round(change, 0), "%")), vjust = -1, color = "black") +
        theme_classic() +
        labs(y = "Change (%)", x = "", fill = "Scenario type", subtitle = "Change compared to 1990") +
        theme(text = element_text(size = 12),
              axis.text.x = element_text(size = 6, angle = 0),
              legend.position = "none") +
        scale_fill_manual(values = c(colors_to_display(), name = "rm")) +
        scale_color_manual(values = c(NA, "black"))
    }
  })
  
  output$comparison_1990 <- renderGirafe(
    girafe(ggobj = comparison_1990(), width_svg = 4, height_svg = 3) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"),
                     opts_selection(type = "none"))
  )
  
  
  # Line plot: Emission change rate
  
  emission_change_rates <- reactive({
    if(nrow(result()) == 0) {
      ggplot() + 
        theme_classic() + 
        geom_label(aes(2060, 1.5, family = "sans-serif", color = "red",
                       label = "No solution for this scenario type!"), size = 3) +
        xlim(2010, 2100) +
        ylim(0, 3) +
        theme(text = element_text(size = 10, family = "sans-serif"),
              legend.position = "none") +
        labs(x = "Year", y = "Emissions (Gt)")
      
    } else {
      result() %>%
        mutate(rr_eff = ifelse(emissions > threshold_linear_rm1, (emissions / lag(emissions) -1) * 100, 0)) %>%
        rownames_to_column("data_id") %>%
        filter(year <= date_display_range() & year > 2019,
               rr_eff < 0 | rr_eff > 0) %>%
        ggplot(aes(x = year, y = rr_eff, color = rm)) +
        geom_line_interactive(aes(data_id = rm, hover_css = "fill:none;", tooltip = rm)) +
        geom_point_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(rr_eff, 2), " %"), data_id = data_id), 
                               size = 0.6) +
        theme_classic() +
        scale_x_continuous(breaks = scales::extended_breaks(n = 8)(2020:2100)) +
        # scale_y_continuous(limits = c(-30, 0)) +
        theme(axis.text.x = element_text(size = 6),
              legend.position = "none") +
        labs(x = "", y = "Change (%)", subtitle = "Annual emission change rates") +
        scale_color_manual(values = c(colors_to_display()))
    }
  })
  
  output$emission_change_rates <- renderGirafe(
    girafe(ggobj = emission_change_rates(), width_svg = 3.8, height_svg = 2.8) %>%
      girafe_options(opts_hover(css = "fill:black; stroke:black;"),
                     opts_selection(type = "none"))
  )
  
  
  # Report ####
  
  warning_text = reactive({
    if(input$global_emission_budget_gt_2018 > 680 & input$max_negative_emissions_perc > 0) {
      "<b>Warning</b>:<br />You have combined a relatively large global budget with possible<br />
      net negative emissions. Please note that the resulting emission<br />
      overshoot increases the risk of exceeding climate tipping points."
    } else if(input$global_emission_budget_gt_2018 >= 800 & input$max_negative_emissions_perc == 0) {
      "<b>Warning</b>:<br />You have chosen a relatively high global budget. Please note the<br />
      higher risk that tipping points in the climate system can be exceeded."
    } else {
      ""
    }
  })
  
  observe(print(
    result() %>%
      filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2050)) %>%
      mutate(change = (1 - emissions / eu_emissions_1990) * -100,
             year = as.character(year)) %>%
      select(year, rm, change) %>%
      pivot_wider(names_from = rm, values_from = change)
  ))
  
  output$report <- downloadHandler(
    
    filename = "espm_report.pdf",
    
    content = function(file) {
      
      # Select template for report depending on selected file type
      template_file <- "report_pdf.Rmd"
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempReport2 <- file.path(tempdir(), "style.css")
      file.copy(paste0("www/", template_file), tempReport, overwrite = TRUE)
      file.copy("www/style.css", tempReport2, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(first_year = first_year,
                     result = result(),
                     date_display_range = date_display_range(),
                     eu_past_emissions = eu_past_emissions,
                     overshoot_amounts = overshoot_amounts(),
                     colors_to_display = colors_to_display(),
                     eu_emissions_1990 = eu_emissions_1990,
                     threshold_linear_rm1 = threshold_linear_rm1,
                     global_emission_budget_gt = global_emission_budget_gt(),
                     pop_weighting = input$pop_weighting,
                     global_emission_budget_gt_2018 = input$global_emission_budget_gt_2018,
                     eu_emission_budget_gt = eu_emission_budget_gt(),
                     max_negative_emissions_gt = max_negative_emissions_gt()*-1,
                     warning_text = warning_text()
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  # Notifications ####
  
  # Scenario type
  
  observeEvent(input$link_info_scenario_type, {
    shinyjs::toggle("info_scenario_type")
  })

  output$box_info_scenario_type <- renderUI({
    hidden(div(class = "info-box", style = "left:330px;", id = "info_scenario_type", HTML("Scenario types differ regarding the annual 
               emission changes associated with them (see plot 'Annual emission change rates'). Get more information on scenario types here:
               <ul><li><a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types_short.pdf', target = '_blank'>Short description</a></li>
               <li><a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf', target = '_blank'>Comprehensive mathematical description</a></li></ul>"),
               actionLink("close_info_scenario_type", icon = icon("window-close"), label = "Close")
               ))
  })
  
  observeEvent(input$close_info_scenario_type, {
    shinyjs::hide("info_scenario_type")
  })
  
  # General
  
  observeEvent(input$link_info_general, {
    shinyjs::toggle("info_general")
  })

  observeEvent(input$close_info_general, {
    shinyjs::hide("info_general")
  })
  
  # Global emission budget
  
  observeEvent(input$link_info_budget, {
    shinyjs::toggle("info_budget")
  })
  
  output$base_data_for_display <- renderTable(
    tibble(
      "Data" = c("Annual emissions EU27", "Annual global emissions"),
      "1990" = c(3.75, ""),
      "2018" = c(3.04, 42.10),
      "Unit" = c("Gt", "Gt")
    ), bordered = T
  )
  
  output$box_info_budget <- renderUI({
    hidden(div(class = "info-box", style = "left:300px; width:650px;", id = "info_budget", 
               HTML("Regarding the global emission budget, we refer in particular to the IPCC Special Report 2018 (<a href = 'http://ipcc.ch/sr15', target = '_blank'>www.ipcc.ch/sr15/</a>). According to this report, compliance with the 1.5°C limit corresponds with a probability of 67% to a remaining CO2 budget of 420 Gt. <a href ='https://www.klima-retten.info/PDF/IPCC_SR15_Remaining_Carbon_Budgets.pdf', target = '_blank'>Here</a> we have summarized the statements of the IPCC. The following table summarizes the main results:<br /><br />"),
               tags$img(src = "table_ipcc_emission_budgets.png", width = "400px"), tags$br(), tags$br(),
               HTML("The higher the selected budget, the greater the risk that dangerous tipping points in the climate system will be exceeded."), tags$br(), tags$br(),
               actionLink("close_info_budget", icon = icon("window-close"), label = "Close")))
  })
  
  observeEvent(input$close_info_budget, {
    shinyjs::hide("info_budget")
  })
  
  # Emissions of 2018
  
  observeEvent(input$link_info_emissions_2018, {
    shinyjs::toggle("info_emissions_2018")
  })
  
  output$box_info_emissions_2018 <- renderUI({
    hidden(div(class = "info-box", style = "left:390px; width:500px;", id = "info_emissions_2018", 
               HTML("Global emissions 2018: 42.1 Gt CO2 (source: <a href = 'https://www.globalcarbonproject.org/', target = '_blank'>Global Carbon Project)</a><br />"),
               actionLink("close_info_emissions_2018", icon = icon("window-close"), label = "Close")))
  })
  
  observeEvent(input$close_info_emissions_2018, {
    shinyjs::hide("info_emissions_2018")
  })
  
  # EU emission budget
  
  observeEvent(input$link_info_eu_budget, {
    shinyjs::toggle("info_eu_budget")
  })
  
  output$box_info_eu_budget <- renderUI({
    hidden(div(class = "info-box", style = "left:390px;; width:500px;", id = "info_eu_budget", 
               tableOutput("base_data_for_display"),
               HTML("<ul><li>Source for EU emissions: <a href = 'https://www.eea.europa.eu/data-and-maps/data/data-viewers/greenhouse-gases-viewer', target = '_blank'>EEA</a>. The figures used here include land use, land use change and forestry (LULUCF) and 'international transport'.</li><li>Source for global emissions: <a href = 'https://www.globalcarbonproject.org/', target = '_blank'>Global Carbon Project</a></li></ul>"),
               HTML("A weighting model is offered to determine the EU's emission budget. The weighting is based on the EU's share of global emissions and of global population. The weighted key is then applied to the global budget to determine the EU's budget 2020 - 2100.<br /><br />"),
               HTML("The emission paths presented here may show a small divergence in relation to the emission budget specified. This is due to technical reasons: In some cases, the optimization algorithm does not yield a solution, so that the underlying budget has to be varied. These deviations do not exceed 5% of the budget. The budget which is actually used is displayed in the table above the emission paths.<br /><br />"),
               actionLink("close_info_eu_budget", icon = icon("window-close"), label = "Close")))
  })
  
  observeEvent(input$close_info_eu_budget, {
    shinyjs::hide("info_eu_budget")
  })
  
  # Author & Contact
  
  observeEvent(input$link_author, {
    shinyjs::toggle("info_contact")
  })
  
  observeEvent(input$close_author, {
    shinyjs::hide("info_contact")
  })

  output$box_contact <- renderUI({
    hidden(div(class = "author-box", id = "info_contact", HTML("<img src = 'daniel_wiegand.gif', style = 'float:left; width:200px; margin-right:20px'>Daniel Wiegand works as a CSR consultant and data scientist. Currently he is doing his doctorate in business ethics at the university of philosophy in Munich.<br /><br />
    For information regarding the Extended Smooth Pathway Model, refer to <a href = 'http://save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>.<br /><br />
    For more information, refer to my <a href = 'https://danielwiegand.github.io/'>personal website</a>. All code to create this website is available on my <a href = 'https://github.com/danielwiegand/espm'>GitHub page</a>. For comments and suggestions contact me on daniel.a.wiegand [at] posteo.de.<br />"),
               actionLink("close_author", icon = icon("window-close"), label = "Close", style = "float:right;")))
  })
  
  # Negative emissions
  output$box_info_negative_emissions <- renderUI({
    hidden(div(class = "info-box", style = "left:390px; width:500px;", id = "info_negative_emissions", 
               HTML("An important question concerns the possibility of future negative emissions. The app allows you specify the potential 
               for net negative emissions by specifying a percentage that is applied to the current EU emissions. This percentage then 
               determines the minimum value of the emission paths by 2100.<br />If net negative emissions are allowed, the EU budget 
               may be temporarily exceeded. This overshoot will then be offset by net negative emissions by 2100. However, it should be 
               noted that overshoot can also lead to dangerous tipping points in the climate system being exceeded. Also, it should be 
               pointed out that the costs of actively capturing CO2 are still unclear and that there are major methodological and substantive 
               problems in quantifying sinks.<br />The actual overshoot
               per scenario type is displayed in the table above the emission paths.<br /><br />"),
               actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
  })
  
  observeEvent(input$link_info_negative_emissions, {
    shinyjs::toggle("info_negative_emissions")
  })
  
  observeEvent(input$close_info_negative_emissions, {
    shinyjs::hide("info_negative_emissions")
  })
  
  
  # Warning: Too high budget
  
  observeEvent(input$go, {
    if(input$global_emission_budget_gt_2018 > 680 & input$max_negative_emissions_perc > 0) {
      showNotification("You have combined a relatively large global budget with possible net negative emissions. Please note that the resulting emission overshoot increases the risk of exceeding tipping points in the climate system.", 
                       type = "warning",
                       duration = 15)
    } else if(input$global_emission_budget_gt_2018 >= 800 & input$max_negative_emissions_perc == 0) {
      showNotification("You have chosen a relatively high global budget. Please note the higher risk that tipping points in the climate system can be exceeded.", 
                       type = "warning",
                       duration = 15)
    }
  })

  
}
