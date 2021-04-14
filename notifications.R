"Notifications and warning / info texts"

# General info texts ####

output$title <- renderUI({
  output = tagList()
  output[[1]] <- HTML("Extended Smooth Pathway Model<br><span style = 'font-size:20px;'>Comprehensive tool for calculating emission paths that adhere to a predefined emission budget</span>")
  output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
  output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
                            HTML("
                                 The ESPM makes it possible to identify emission paths that adhere to a predefined emission budget. In this universal tool, this emission budget can be global or based on a national, regional or any other entity.<br /><br />
                                 The scenario types used to determine the emission paths differ in their assumptions about the annual emission changes (see plot 'Annual emission change rates'). The scenario types offered cover the range of plausible emission paths well.<br /><br />
                                 The app allows you specify the minimum annual emissions between 2020 and 2100. Please note to the constraints mentioned in the appropriate help text on this page.<br /><br />
                                 You can find other tools for the calculation of Paris-compatible emission paths at <a href ='http://www.save-the-climate.info'>www.save-the-climate.info</a>.<br /><br />
                                 "),
                            actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
  
  return(output)
})

showModal(modalDialog(
  title = "Extended Smooth Pathway Model",
  HTML("
    The ESPM makes it possible to identify emission paths that adhere to a predefined emission budget. In this universal tool, this emission budget can be global or based on a national, regional or any other entity.<br /><br />
    The scenario types used to determine the emission paths differ in their assumptions about the annual emission changes (see plot 'Annual emission change rates'). The scenario types offered cover the range of plausible emission paths well.<br /><br />
    The app allows you specify the minimum annual emissions between 2020 and 2100. Please note the constraints mentioned in the appropriate help text on this page.<br /><br />
    You can find other tools for the calculation of Paris-compatible emission paths at <a href ='http://www.save-the-climate.info'>www.save-the-climate.info</a>.<br />
       "),
  easyClose = FALSE,
  footer = modalButton("Close")
))

# Scenario type ####

observeEvent(input$link_info_scenario_type, {
  shinyjs::toggle("info_scenario_type")
})

output$box_info_scenario_type <- renderUI({
  hidden(div(class = "info-box", style = "left:330px; width: 500px;", id = "info_scenario_type", HTML(
  "Scenario types differ regarding the annual emission changes associated with them (see plot 'Annual emission change rates'). Get more information on scenario types here:
  <ul>
    <li>
      <a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types_short.pdf', target = '_blank'>Brief description</a>
    </li>
    <li>
      <a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf', target = '_blank'>Comprehensive mathematical description</a>
    </li>
  </ul>
  Compared to the MS Excel tools which can be downloaded at <a href = 'http://save-the-climate.info'>http://save-the-climate.info</a> some simplifications are applied for this online tool:
  <ul>
    <li>
      The 2020 rate of change for the scenario types RM 3-5 is 50% of the change rate of RM-6 (where the emission path is a straight line). In the Excel tools, this change rate is an input value.
    </li>
    <li>
      In the scenario types RM 1 and 3-5, the emission path becomes a straight line when the emissions fall below a given threshold. This threshold is 3.5% of the 2019 emissions for the scenario types RM 3-6 and 4.5% for RM-1 in this online tool. In the Excel tools, this threshold is an input value.
    </li>
  </ul>"),
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

output$box_info_budget <- renderUI({
  hidden(div(class = "info-box", style = "left:390px; width:650px;", id = "info_budget", 
             HTML("
Indicate the emission budget for the timeframe 2020 - 2100, which should be met by the emission paths. This emission budget can be global or based on a national, regional or any other entity. Please ensure that the emissions of the budget and those of the base or reference year refer to the same emissions in terms of content. For example, if the emission budget covers emissions from land use change, the emissions in the base and reference year should include emissions from land use change as well. <br /><br />Regarding the global emission budget, we refer in particular to the IPCC Special Report 2018 (<a href = 'www.ipcc.ch/sr15/' target = '_blank'>www.ipcc.ch/sr15/</a>). <a href ='https://www.klima-retten.info/PDF/IPCC_SR15_Remaining_Carbon_Budgets.pdf', target = '_blank'>Here</a> we have summarized the statements of the IPCC.<br /><br />
For the determination of national budgets, we refer to e.g.:
<ul>
  <li>Our web app determining Paris compatible emission paths for the EU: <a href = 'http://eu.climate-calculator.info' target = '_blank'>http://eu.climate-calculator.info</a></li>
  <li>Our further Excel tools for the determination of Paris compatible emission paths for all countries: <a href = 'http://www.save-the-climate.info/' target = '_blank'>http://www.save-the-climate.info/</a></li>
  <li>Possible data sources: Global Carbon Project (<a href = 'https://www.globalcarbonproject.org/' target = '_blank'>GCP</a>), EU Emissions Database for Global Atmospheric Research (<a href = 'https://edgar.jrc.ec.europa.eu/' target = '_blank'>EDGAR</a>), European Environment Agency (<a href = 'https://www.eea.europa.eu/' target = '_blank'>EEA</a>)</li>
</ul>
The pre-filled figures refer to the global CO2 emissions including emissions from land use change. The budget used is based on a global emission budget of 420 Gt from 2018 on, which was lowered by the emissions of the years 2018 and 2019 (about 86 Gt CO2). 2010 was chosen as a reference year.<br /><br />
The emission paths presented here may show a small divergence in relation to the emission budget specified. This is due to technical reasons: In some cases, the optimization algorithm does not yield a solution, so that the underlying budget has to be varied. The budget which is actually used is displayed in the table above the emission paths.<br /><br />
"), actionLink("close_info_budget", icon = icon("window-close"), label = "Close")))
})

observeEvent(input$close_info_budget, {
  shinyjs::hide("info_budget")
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
    For further information regarding the Extended Smooth Pathway Model, refer to <a href = 'http://save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>.<br /><br />
    All code to create this website is available on my <a href = 'https://github.com/danielwiegand/espm'>GitHub page</a>. For comments and suggestions contact me on daniel.a.wiegand [at] posteo.de.<br />"),
             actionLink("close_author", icon = icon("window-close"), label = "Close", style = "float:right;")))
})

# Negative emissions ####

output$box_info_negative_emissions <- renderUI({
  hidden(div(class = "info-box", style = "left:390px; width:500px;", id = "info_negative_emissions", 
             HTML("
The app allows you specify the minimum annual emissions. An indication of 0 is equivalent to net zero emissions by 2100, while a negative value assumes that net negative annual emissions are possible. Please note that setting a positive value as minimum means that the given emission budget would be exceeded from 2101 on.<br /><br />
If net negative emissions are allowed, the budget may be temporarily exceeded. These overshoot amounts will then be offset by net negative emissions by 2100. The actual overshoot amounts per scenario type are displayed in the table above the emission paths. However, it should be noted that the overshoot amounts can also lead to dangerous <b>tipping points</b> in the climate system being exceeded. Also, it should be pointed out that the <b>costs</b> of actively capturing CO2 are still unclear and that there are major methodological and substantive problems in <b>quantifying</b> sinks and questions regarding their sustainable value.<br /><br />
                  "),
             actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
})

observeEvent(input$link_info_negative_emissions, {
  shinyjs::toggle("info_negative_emissions")
})

observeEvent(input$close_info_negative_emissions, {
  shinyjs::hide("info_negative_emissions")
})

# observeEvent(input$emission_unit, {
#   updateNumericInput(session, "min_emissions", label = paste0("Minimum possible annual emissions (", input$emission_unit, ")"))
# })

observeEvent(input$toggle_report_questions, {
  shinyjs::show("report_questions")
}, ignoreInit = T)

observeEvent(input$close_report_questions, {
  shinyjs::hide("report_questions")
})