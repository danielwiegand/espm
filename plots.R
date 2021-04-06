"Plots of the ESPM web app"

# Table: Change compared to the reference year

comparison_reference_year <- reactive({
  result() %>%
    filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2050)) %>%
    mutate(change = paste0(as.character(round((1 - emissions / input$reference_year_emissions) * -100, 0)), "%"),
           year = as.character(year)) %>%
    select(year, rm, change) %>%
    pivot_wider(names_from = rm, values_from = change) #%>%
    # tableGrob(theme = mytheme, rows = NULL, cols = colnames(.))
})

output$comparison_reference_year <- renderTable(
  comparison_reference_year()
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
      mutate(rr_eff = ifelse(emissions > THRESHOLD_LINEAR_RM1, (emissions / lag(emissions) -1) * 100, 0)) %>%
      rownames_to_column("data_id") %>%
      filter(year <= date_display_range() & year > 2019,
             rr_eff < 0 | rr_eff > 0) %>%
      ggplot(aes(x = year, y = rr_eff, color = rm)) +
      geom_line_interactive(aes(data_id = rm, hover_css = "fill:none;", tooltip = rm)) +
      geom_point_interactive(aes(tooltip = paste0(rm, " (", year, "): ", round(rr_eff, 2), " %"), data_id = data_id), 
                             size = 0.6) +
      theme_classic() +
      scale_x_continuous(breaks = scales::extended_breaks(n = 16)(2020:2100)) +
      # scale_y_continuous(limits = c(-30, 0)) +
      theme(axis.text.x = element_text(size = 10),
            legend.position = "none") +
      labs(x = "", y = "Change (%)") +
      scale_color_manual(values = c(colors_to_display()))
  }
})

output$emission_change_rates <- renderGirafe(
  girafe(ggobj = emission_change_rates(), width_svg = 4.8, height_svg = 2.2) %>%
    girafe_options(opts_hover(css = "fill:black; stroke:black;"),
                   opts_selection(type = "none"))
)