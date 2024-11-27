"Notifications and warning / info texts"

# General info texts ####

output$title <- renderUI({
  output = tagList()
  output[[1]] <- HTML("Extended Smooth Pathway Model<br><span style = 'font-size:20px;'>Comprehensive tool for calculating emission paths that adhere to a predefined emission budget</span>")
  output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
  output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
                            HTML("

The Extended Smooth Pathway Model (ESPM) allows Paris-compatible emission targets to be identified. A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target= '_blank'>here</a>.<br><br>
Starting from a budget you set, this web app allows you to derive the full range of plausible emission paths that meet that budget.<br><br>
Here we offer a possibility to calculate national CO2 budgets for all countries of the world with a web app: <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator</a>.<br><br>
Here is an overview of our tools and recent publications: <a href = 'http://www.climate-calculator.info', target= '_blank'>www.climate-calculator.info</a>.<br><br>
Our main page: <a href = 'http://www.save-the-climate.info', target= '_blank'>www.save-the-climate.info</a>.<br><br>

"),
actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
  
  return(output)
})

showModal(modalDialog(
  title = "Extended Smooth Pathway Model",
  HTML("

The Extended Smooth Pathway Model (ESPM) allows Paris-compatible emission targets to be identified. A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target= '_blank'>here</a>.<br><br>
Starting from a budget you set, this web app allows you to derive the full range of plausible emission paths that meet that budget.<br><br>
Here we offer a possibility to calculate national CO2 budgets for all countries of the world with a web app: <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator</a>.<br><br>
Here is an overview of our tools and recent publications: <a href = 'http://www.climate-calculator.info', target= '_blank'>www.climate-calculator.info</a>.<br><br>
Our main page: <a href = 'http://www.save-the-climate.info', target= '_blank'>www.save-the-climate.info</a>.<br><br>

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
    "

The Scenario types differ regarding the annual emission changes associated with them (see plot 'Annual emission change rates'). More information about the RM Scenario Types can be found here:
  <ul>
    <li>
      <a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types_short.pdf', target = '_blank'>Brief description</a>
    </li>
    <li>
      <a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types.pdf', target = '_blank'>Comprehensive mathematical description</a>
    </li>
  </ul>

<b>Starting rate of change 2020 RM 3 – 5</b><br><br>
The rate of change for 2020 is basically an input value in the RM Scenario Types 3 - 5, which serves as a starting value for the determination of the following rates of change. However, due to the temporary corona effect in 2020, it does not make sense to use the actual value for 2020. A normalised value should be used.<br><br>
<b>Start rate of change 2020 in the web app</b><br><br>
In this web app, the change in emissions from 2018 to 2019 is generally used as the normalised starting change rate 2020.<br><br>
However, the iterative solution procedure in this web app cannot process a positive starting change rate. Therefore, if emissions increase from 2018 to 2019, 0% is set as the starting rate of change for 2020.<br><br>
In the corresponding Excel tool (download <a href = 'https://doi.org/10.5281/zenodo.4568839', target= '_blank'>here</a>), a positive starting change rate can also be used.<br><br>
<b>Emissions after the base year 2019</b><br><br>
In the corresponding Excel tool (download <a href = 'https://doi.org/10.5281/zenodo.4568839', target= '_blank'>here</a>), actual emissions after the base year 2019 can also be taken into account.<br><br>

"),
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

<b>Data input</b><br><br>
In order to calculate emission paths and emission targets with this tool, the budget to be adhered to and the emissions in 2018, 2019 and a reference year must be entered.<br><br>

Indicate the emission budget for the timeframe 2020 - 2100, which should be met by the emission paths. This emissions budget can be global or relate to a national, regional or other entity such as a company.<br><br>
Please ensure that the emissions of the budget and annual emissions refer to the same emissions in terms of content. For example, if the emission budget covers emissions from land-use change, the annual emissions should include emissions from land-use change as well.<br><br>
<b>Determination of emission budgets</b><br><br>
Regarding the global emission budget, we refer in particular to the <a href = 'https://www.ipcc.ch/assessment-report/ar6/', target= '_blank'>IPCC Report AR6/WGI</a> from 2021. <a href = 'https://www.klima-retten.info/PDF/IPCC_AR6_Remaining_Carbon_Budgets.pdf', target= '_blank'>Here</a> we have summarized the statements of the IPCC.<br><br>

We offer a web app that can be used to derive <b>national CO2 budgets</b> from a global budget for <b>all countries</b> in the world using a weighted distribution key: <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator.info</a>. The other data are also given there in order to be able to derive corresponding emission paths here.<br><br>
<b>Data sources</b><br><br>
We recommend the EU's 'Emissions Database for Global Atmospheric Research (<a href = 'https://edgar.jrc.ec.europa.eu/', target= '_blank'>EDGAR</a>) and the <a href = 'https://www.globalcarbonproject.org', target= '_blank'>Global Carbon Project</a>. On the EU, we refer to the European Environment Agency (<a href = 'https://www.eea.europa.eu/', target= '_blank'>EEA</a>).<br><br>
Our web app <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator.info</a> provides the corresponding data for all countries in the world and also the global values.<br><br>
<b>Pre-filled figures</b><br><br>
The pre-filled values refer to global CO2 emissions. The global CO2 budget of 400 Gt corresponds to a 67% probability of keeping the 1.5°C limit according to IPCC. 2019 was chosen as the reference year. Source for global CO2 emissions: Global Carbon Project resp. our web app <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator.info</a>.<br><br>
<b>Divergence of emission budget</b><br><br>
In some cases, the optimisation algorithm does not lead to a solution, so the underlying budget has to be varied slightly. The final budget used is shown in the table above the emission path graph.<br><br>

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
  hidden(div(class = "author-box", id = "info_contact", HTML("<img src = 'daniel_wiegand.gif', style = 'float:left; width:200px; margin-right:20px'>

Programmed by Daniel Wiegand. All code to create this website is available on my <a href = 'https://github.com/danielwiegand/espm/tree/universal', target= '_blank'>GitHub page</a>. For comments and suggestions contact me on daniel.a.wiegand [at] posteo.de.<br><br>
For further information regarding the Extended Smooth Pathway Model, refer to <a href = 'http://save-the-climate.info', target= '_blank'>www.save-the-climate.info</a>.<br>

"),
actionLink("close_author", icon = icon("window-close"), label = "Close", style = "float:right;")))
})

# Negative emissions ####

output$box_info_negative_emissions <- renderUI({
  hidden(div(class = "info-box", style = "left:390px; width:500px;", id = "info_negative_emissions", 
             HTML("

The app allows you specify the minimum annual emissions. An indication of 0 is equivalent to net zero emissions by 2100, while a negative value assumes that net negative annual emissions are possible.<br><br>
Please note that setting a positive value as minimum means that the given emission budget would be exceeded from 2101 on.<br><br>

If net negative emissions are allowed, the budget may be temporarily exceeded. These overshoot amounts will then be offset by net negative emissions by 2100. The actual overshoot amounts per scenario type are displayed in the table above the emission paths.<br><br>
<a href = 'http://luc.climate-calculator.info', target= '_blank'>Here</a> is a background text on determining the scope of net negative CO2 emissions.<br><br>

"),
actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
})

output$infotext_initial_change_rate <- renderUI({
  tags$div(paste("Percental emissions change between", input$start_year - 1, "and", input$start_year), style = "font-weight:bold; float:left;")
})

output$infotext_emission_budget <- renderUI({
  tags$div(paste("Emission budget between", input$start_year, "and 2100"), style = "font-weight:bold; float:left;")
})

output$infotext_base_year_emissions <- renderUI({
  tags$div(paste0("Base year (", input$start_year - 1, ") emissions"), style = "font-weight:bold; float:left;")
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