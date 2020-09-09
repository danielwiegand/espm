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
  
  colors_to_display <- eventReactive(input$go, ignoreNULL = F, {
    a <- c("RM-1" = "#4b8abd", "RM-2" = "#b9594d", "RM-3" = "#a7b25d", "RM-4" = "#7970a2", "RM-5" = "#f1974b", "RM-6" = "#818181")
    b <- a[input$selected_rm]
  }) 
  
  output$title <- renderUI({
    output = tagList()
    output[[1]] <- HTML("Extended Smooth Pathway Model (ESPM)<br><span style = 'font-size:20px;'>Calculating Paris compatible emission goals using the example of the EU</span>")
    output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
    output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
    HTML("The Extended Smooth Pathway Model (ESPM) is a model to determine 
    emission paths which are in line with the Paris agreement. It consists of two calculation steps: Determination 
    of a national budget and derivation of plausible national emission paths from this budget.<br /><br />
    This app focuses on the EU. A weighting model is offered to determine its emission budget. The 
    weighting is based on the EU's share of global emissions and of global population. The weighted 
    key is then applied to the global budget to determine the EU's budget 2020 - 2100. <br /><br />The scenario 
    types used to determine the emission paths differ in their assumptions about the annual emission 
    changes (see plot 'Emission change rate'). A comprehensive mathematical description of the scenario 
    types can be downloaded <a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf'>here</a>.<br /> <br />
    An important question concerns the possibility of future negative 
    emissions. The app allows you specify the potential for net negative emissions by specifying a 
    percentage that is applied to the current EU emissions. This percentage then determines the minimum 
    value of the emission paths until 2100. If net negative emissions are allowed, the EU budget may be 
    temporarily exceeded. This overshoot will then be offset by net negative emissions by 2100. 
    However, it should be noted that overshoot can also lead to dangerous tipping points in the 
    climate system being exceeded.<br /><br />"),
    actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
    
    return(output)
  })
  
  showModal(modalDialog(
    title = "The Extended Smooth Pathway Model (ESPM)",
    HTML("The Extended Smooth Pathway Model (ESPM) is a model to determine emission paths which are in line with the Paris agreement.
    It consists of two calculation steps: Determination of a national budget and derivation of plausible national emission paths from this budget.<br /><br />
    This app focuses on the EU. A weighting model is offered to determine its emission budget. The weighting is based on the EU's share
    of global emissions and of global population. The weighted key is then applied to the global budget to determine the EU's budget 2020 - 2100.<br /><br />
    The scenario types used to determine the emission paths differ in their assumptions about the annual emission changes (see plot 'Emission change rate').
    A comprehensive mathematical description of the scenario types can be downloaded <a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf'>here</a>.<br /><br />
    An important question concerns the possibility of future negative emissions. The app allows you specify the potential for 
    net negative emissions by specifying a percentage that is applied to the current EU emissions. This percentage then 
    determines the minimum value of the emission paths until 2100.<br /><br />
    If net negative emissions are allowed, the EU budget may be temporarily exceeded. This overshoot will then be offset by net negative emissions by 2100. However, it should be noted that overshoot can also lead to dangerous tipping points in the climate system being exceeded."),
    easyClose = FALSE,
    footer = modalButton("Close")
  ))
  
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
               y = c("7.2%", "5.8%", paste0(round(weighted_key() * 100, 1), "%"),
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
      
      year_zero_emissions <- x$year[which(x$emissions <= 0)[1]]
      overshoot_amounts <- eventReactive(input$go, ignoreNULL = F, {
        
        total_emissions <- x %>%
          group_by(rm) %>%
          rename("RM" = rm) %>%
          summarize("Budget" = round(sum(emissions[-1]), 1))
        
        if(nrow(x[x$emissions < 0,]) == 0) {
          overshoots <- tibble("RM" = c(input$selected_rm),
                        "Overshoot" = 0)
          } else {
            overshoots <- x %>%
              rename("RM" = rm) %>%
              filter(emissions < 0) %>%
              group_by(RM) %>%
              summarize("Overshoot" = round(sum(emissions, na.rm = T) * -1, 1)) 
          }
        
        out <- left_join(total_emissions, overshoots) %>%
          select(RM, Budget, 'Overshoot') %>%
          mutate("Unit" = "Gt")
        
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
        annotation_custom(tableGrob(overshoot_amounts(), rows = NULL, theme = ttheme_minimal(base_size = 6, padding = unit(c(2, 2), "mm"))), 
                          xmin = ifelse(date_display_range() == 2100, 2060, 2035), ymin = 1.5, ymax = 3) +
        theme_classic() +
        scale_x_continuous(breaks = scales::extended_breaks(n = 9)(2010:2100)) +
        scale_y_continuous(breaks = scales::extended_breaks(n = 9)(-0.5:3.5)) +
        labs(x = "Year", y = "Emissions (Gt)", color = "Scenario type", subtitle = "Emissions over time") +
        theme(text = element_text(size = 10)) +
        scale_color_manual(values = c(colors_to_display()))
        # scale_color_brewer(palette = "YlOrRd")
    }
  }
  
  calculate_pathway <- function(rm) {
    
    output <- data.frame(t = numeric(), year = numeric(), emissions = numeric(), rr = numeric(), rm = character())
    
    withProgress(message = 'Update...', value = 0, {
      
      for(i in 1:length(rm)) {
        
        neg <- ifelse(rm[i] == "RM-2", F, T)
        
        opt_x <- NULL
        is_steady <- T
        sequence <- seq(1, 0.90, -0.005)
        set.seed(3)
        
        # Slightly vary the budget so that an optimum is found for a steady function
        for(j in sequence) {
          
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
        
        incProgress(1/length(rm), detail = paste("Optimize scenario type", i))
        
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
  
  # Bar plot: Change compared to 1990
  
  comparison_1990 <- reactive({
    result() %>%
      filter(year %in% c(2020, 2030, 2035, 2040, 2050)) %>%
      mutate(change = (1 - emissions / eu_emissions_1990) * -100,
             year = as.character(year),
             is_2030 = ifelse(year == 2030, "Y", "N")) %>%
      rownames_to_column("data_id") %>%
      ggplot(aes(x = year, y = change, fill = rm, color = is_2030)) +
      geom_col_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(change, 1), "%"), data_id = data_id), width = .7) +
      facet_wrap(~rm) +
      geom_text(size = 2, aes(label = paste0(round(change, 0), "%")), vjust = -1, color = "black") +
      theme_classic() +
      labs(y = "Change (%)", x = "", fill = "Scenario type", subtitle = "Change compared to 1990") +
      theme(text = element_text(size = 12),
            axis.text.x = element_text(size = 6),
            legend.position = "none") +
      # scale_fill_brewer(palette = "YlOrRd") +
      scale_fill_manual(values = c(colors_to_display(), name = "rm")) +
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
      filter(year <= date_display_range() & year > 2019,
             rr_eff < 0 | rr_eff > 0) %>%
      ggplot(aes(x = year, y = rr_eff, color = rm)) +
      geom_line_interactive(aes(data_id = rm, hover_css = "fill:none;", tooltip = rm)) +
      geom_point_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(rr_eff, 2), " %"), data_id = data_id), 
                             size = 0.6) +
      theme_classic() +
      scale_x_continuous(breaks = scales::extended_breaks(n = 8)(2020:2100)) +
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none") +
      labs(x = "", y = "Change (%)", subtitle = "Emission change rates") +
      scale_color_manual(values = c(colors_to_display()))
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
  
  # Scenario type
  
  observeEvent(input$link_info_scenario_type, {
    shinyjs::toggle("info_scenario_type")
  })

  output$box_info_scenario_type <- renderUI({
    hidden(div(class = "info-box", style = "left:330px;", id = "info_scenario_type", "Scenario types differ regarding the annual 
               emission changes associated with them (see plot 'Emission change rate'). Get more information on scenario types ", 
               tags$a(href = "https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf", "here"), HTML(".<br />"),
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
  
  # Budget
  
  observeEvent(input$link_info_budget, {
    shinyjs::toggle("info_budget")
  })
  
  output$base_data_for_display <- renderTable(
    tibble(
      "Data" = c("Annual emissions EU27 (database: EEA)", "Annual global emissions (database: GCP)"),
      "1990" = c(3.75, ""),
      "2018" = c(3.04, 42.10),
      "Unit" = c("Gt", "Gt")
    ), bordered = T, width = "600px"
  )
  
  output$box_info_budget <- renderUI({
    hidden(div(class = "info-box", style = "left:470px; width:500px;", id = "info_budget", 
               tableOutput("base_data_for_display"),
               HTML("The emission paths presented here may show a small divergence in relation to the emission budget specified. This is due to technical reasons: In some cases,the optimization algorithm does not yield a solution, so that the underlying budget has to be varied. These deviations do not exceed 5% of the budget.<br /><br />"),
               HTML("Regarding the global emission budget, we refer in particular to the IPCC Special Report 2018 (chapter 2, table 2.2, <a href = 'http://ipcc.ch/sr15'>www.ipcc.ch/sr15/</a>. Staying below 1.5°C of global warming with a probability of 66% sets the remaining carbon budget to 420 Gt CO2.).<br /><br />"),
               actionLink("close_info_budget", icon = icon("window-close"), label = "Close")))
  })
  
  observeEvent(input$close_info_budget, {
    shinyjs::hide("info_budget")
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
    For information regarding the Extended Smooth Pathway Model, refer to <a href = 'http://save-the-climate.info'>www.save-the-climate.info</a>.<br /><br />
    For more information, refer to my <a href = 'https://danielwiegand.github.io/'>personal website</a>. All code to create this website is available on my <a href = 'https://github.com/danielwiegand/espm'>GitHub page</a>. For comments and suggestions contact me on daniel.a.wiegand [at] posteo.de.<br />"),
               actionLink("close_author", icon = icon("window-close"), label = "Close", style = "float:right;")))
  })
  
  # Negative emissions
  output$box_info_negative_emissions <- renderUI({
    hidden(div(class = "info-box", style = "left:580px; width:500px;", id = "info_negative_emissions", 
               HTML("An important question concerns the possibility of future negative emissions. The app allows you specify the potential 
               for net negative emissions by specifying a percentage that is applied to the current EU emissions. This percentage then 
               determines the minimum value of the emission paths by 2100.<br />If net negative emissions are allowed, the EU budget 
               may be temporarily exceeded. This overshoot will then be offset by net negative emissions by 2100. However, it should be 
               noted that overshoot can also lead to dangerous tipping points in the climate system being exceeded.<br />"),
               actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
  })
  
  observeEvent(input$link_info_negative_emissions, {
    shinyjs::toggle("info_negative_emissions")
  })
  
  observeEvent(input$close_info_negative_emissions, {
    shinyjs::hide("info_negative_emissions")
  })

  
}
