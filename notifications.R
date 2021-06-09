"Notifications and warning / info texts"

# General info texts ####

output$title <- renderUI({
  output = tagList()
  output[[1]] <- HTML("Extended Smooth Pathway Model (ESPM)<br><span style = 'font-size:20px;'>Calculating Paris compatible emission paths and targets using the example of the EU</span>")
  output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
  output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
                            HTML("The Extended Smooth Pathway Model (ESPM) is a model to determine 
    emission paths which are in line with the Paris Agreement. It consists of two calculation steps: Determination 
    of a <b>national budget</b> and derivation of plausible <b>national emission paths</b> from this budget.<br /><br />
    This app focuses on the EU. A <b>weighting model</b> is offered to determine its <b>emission budget</b>. The 
    weighting is based on the EU's share of global emissions and of global population. The weighted 
    key is then applied to the global budget to determine the EU's budget 2020 - 2100. <br /><br />The scenario 
    types used to determine the emission paths differ in their <b>assumptions</b> about the <b>annual emission 
    changes</b> (see plot 'Annual emission change rates').<br /> <br />
    An important question concerns the possibility of future <b>negative emissions</b>. The app allows you specify the 
    potential for net negative emissions. Please refer to the notes in the corresponding help text.<br /><br />
    A <b>brief background</b> paper on the <b>ESPM</b> can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target = '_blank'>here</a>. More information about the ESPM and other tools at: <a href = 'http://www.save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>.<br /><br />"),
                            actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
  
  return(output)
})

showModal(modalDialog(
  title = "The Extended Smooth Pathway Model (ESPM)",
  HTML("The Extended Smooth Pathway Model (ESPM) is a model to determine emission paths which are in line with the Paris Agreement.
    It consists of two calculation steps: Determination of a <b>national budget</b> and derivation of plausible <b>national emission paths</b> from this budget.<br /><br />
    This app focuses on the EU. A <b>weighting model</b> is offered to determine its <b>emission budget</b>. The weighting is based on the EU's share
    of global emissions and of global population. The weighted key is then applied to the global budget to determine the EU's budget 2020 - 2100.<br /><br />
    The scenario types used to determine the emission paths differ in their <b>assumptions</b> about the <b>annual emission changes</b> (see plot 'Annual emission change rates').<br /><br />
    An important question concerns the possibility of future <b>negative emissions</b>. The app allows you specify the potential for net negative emissions. Please refer to the notes in the corresponding help text.<br /><br />
    A <b>brief background</b> paper on the <b>ESPM</b> can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target = '_blank'>here</a>. More information about the ESPM and other tools at: <a href = 'http://www.save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>."),
  easyClose = FALSE,
  footer = modalButton("Close")
))

# Scenario type ####

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

# General ####

observeEvent(input$link_info_general, {
  shinyjs::toggle("info_general")
})

observeEvent(input$close_info_general, {
  shinyjs::hide("info_general")
})

# Global emission budget ####

observeEvent(input$link_info_budget, {
  shinyjs::toggle("info_budget")
})

output$base_data_for_display <- renderTable(
  tibble(
    "Data" = c("Annual emissions EU27", "Annual global emissions"),
    "1990" = c(3.81, ""),
    "2019" = c(2.92, 43.05),
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

# Emissions of 2018 ####

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

# EU emission budget ####

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

# Author & Contact ####

observeEvent(input$link_author, {
  shinyjs::toggle("info_contact")
})

observeEvent(input$close_author, {
  shinyjs::hide("info_contact")
})

output$box_contact <- renderUI({
  hidden(div(class = "author-box", id = "info_contact", HTML("<img src = 'daniel_wiegand.gif', style = 'float:left; width:200px; margin-right:20px'>Daniel Wiegand works as a CSR consultant and data scientist. Currently he is doing his doctorate in business ethics at the university of philosophy in Munich.<br /><br />
    For information regarding the Extended Smooth Pathway Model, refer to <a href = 'http://save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>.<br /><br />
    All code to create this website is available on my <a href = 'https://github.com/danielwiegand/espm'>GitHub page</a>. For comments and suggestions contact me on daniel.a.wiegand [at] posteo.de.<br />"),
             actionLink("close_author", icon = icon("window-close"), label = "Close", style = "float:right;")))
})

# Negative emissions ####

output$box_info_negative_emissions <- renderUI({
  hidden(div(class = "info-box", style = "left:390px; width:500px;", id = "info_negative_emissions", 
             HTML("An important question concerns the possibility of future negative emissions. The app allows you specify the potential 
               for net negative emissions by specifying a percentage that is applied to the current EU emissions. This percentage then 
               determines the minimum value of the emission paths by 2100.<br /><br />If net negative emissions are allowed, the EU budget 
               may be temporarily exceeded. This overshoot will then be offset by net negative emissions by 2100. However, it should be 
               noted that overshoot can also lead to dangerous tipping points in the climate system being exceeded. Also, it should be 
               pointed out that the costs of actively capturing CO2 are still unclear and that there are major methodological and substantive 
               problems in quantifying sinks.<br /><br />The actual overshoot
               per scenario type is displayed in the table above the emission paths.<br /><br />
                  Negative CO2 emissions will be necessary to compensate for other greenhouse gases like methane and nitrous oxide, e.g. from agriculture, in order to achieve climate neutrality. These negative CO2 emissions are not considered here and must be provided additionally.<br /><br />"),
             actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
})

observeEvent(input$link_info_negative_emissions, {
  shinyjs::toggle("info_negative_emissions")
})

observeEvent(input$close_info_negative_emissions, {
  shinyjs::hide("info_negative_emissions")
})


# Warning: Too high budget ####

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

