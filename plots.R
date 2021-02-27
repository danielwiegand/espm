"Plots of the ESPM web app"

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
      mutate(change = (1 - emissions / EU_EMISSIONS_1990) * -100,
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
      mutate(rr_eff = ifelse(emissions > THRESHOLD_LINEAR_RM1, (emissions / lag(emissions) -1) * 100, 0)) %>%
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