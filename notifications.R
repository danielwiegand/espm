"Notifications and warning / info texts"

# General info texts ####

output$title <- renderUI({
  output = tagList()
  output[[1]] <- HTML("Extended Smooth Pathway Model<br><span style = 'font-size:20px;'>Comprehensive tool for calculating emission paths that adhere to a predefined emission budget</span>")
  output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
  output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
                            HTML("

The Extended Smooth Pathway Model (ESPM) allows Paris-compatible emission targets to be identified. A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target= '_blank'>here</a>.<br><br>
Starting from a budget you set, this web app allows you to derive the full range of <b>plausible emission paths</b> that meet that budget.<br><br>
Here we offer a possibility to calculate <b>national CO2 budgets</b> for all countries of the world with a web app: <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator</a>.<br><br>
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
Starting from a budget you set, this web app allows you to derive the full range of <b>plausible emission paths</b> that meet that budget.<br><br>
Here we offer a possibility to calculate <b>national CO2 budgets</b> for all countries of the world with a web app: <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator</a>.<br><br>
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

<b>The Regensburg Model Scenario Types</b><br><br>
The scenario types differ regarding the annual emission changes associated with them (see plot 'Annual emission change rates'). More information about the RM Scenario Types can be found here:
  <ul>
    <li><a href = 'https://www.klima-retten.info/Downloads/RM-Scenario-Types_short.pdf', target = '_blank'>Brief description</a></li>
    <li><a href = 'https://doi.org/10.5281/zenodo.4540475', target = '_blank'>Comprehensive mathematical description</a></li>
  </ul>
<br>
The following questions may help you decide on a scenario type:
<ul>
	<li>Which annual reduction rates are realistic and when?</li>
	<li>Do initially slowly increasing annual reduction rates (RM-4 and RM-6) imply an unjustifiable duty for the future, as they imply higher reduction rates later?</li>
	<li>Do high later reduction rates make sense, if they provide a longer lead time for the neces-sary investments and the investments could then rather be made within the framework of normal investment cycles? However, this requires a very credible climate policy backed by effective instruments.</li>
	<li>Do initially rapidly increasing reduction rates (RM-3 and RM-5) convey a more credible climate protection policy that creates planning security for public and private investments in a fossil-free future?</li>
</ul>
<br>
Scenario types RM 3 - 5 have the advantage that the starting rate of change can be specified on the basis of the real situation (see below). In RM-1 (constant annual reduction rate) and RM-6 (constant annual reduction amount, linear emissions path), on the other hand, the starting rate of change is endogenous.

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

To use this tool to calculate emission paths and emission targets, the following base data must be entered:<br>
<ul>
	<li>Start year of the budget period (<i>SY</i>)</li>
	<li>Budget to be adhered</li>
	<li>Actual emissions in the base year (<i>SY</i>-1)</li>
	<li>Acutal emissions in a freely selectable reference year</li>
	<li>Start change rate in the start year (see the corresponding notes there)</li>
</ul>
<br>

The emissions budget can be global or relate to a national, regional or other entity such as a company. Please ensure that the emissions of the budget and annual emissions refer to the same emissions in terms of content. For example, if the emission budget covers emissions from land-use change, the annual emissions should include emissions from land-use change as well.<br><br>

<b>Determination of emissions budget</b><br><br>

Regarding the <b>global emissions budget</b>, we refer in particular to the IPCC Report AR6 from 2021. <a href = 'http://ipcc-co2-budgets-short.climate-calculator.info', target= '_blank'>Here</a> we provide an overview of the budgets according to the IPCC.<br><br>

We offer a web app that can be used to derive <b>national CO2 budgets</b> from a global budget for <b>all countries</b> in the world using a weighted distribution key: <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>http://national-budgets.climate-calculator.info</a>. The other data are also given there in order to be able to derive corresponding emission paths here.<br><br>

<b>Data sources</b><br><br>

We recommend:
<ul>
	<li>Fossil CO2 emissions data for all countries in the world: EU's Emissions Database for Global Atmospheric Research (<a href = 'https://edgar.jrc.ec.europa.eu/', target= '_blank'>EDGAR</a>)</li>
	<li>Data on global emissions: <a href = 'https://www.globalcarbonproject.org', target= '_blank'>Global Carbon Project (GCP)</a></li>
	<li>Total CO2 emissions data EU and EU members: European Environment Agency (<a href = 'https://www.eea.europa.eu/', target= '_blank'>EEA</a>)</li>
</ul>
<br>
Our <a href = 'http://national-budgets.climate-calculator.info', target= '_blank'>web app</a>, which is based on the EDGAR database, provides the corresponding data for all countries in the world and also the corresponding global values. The app for the <a href = 'http://EU.national-budgets.climate-calculator.info', target= '_blank'>EU</a>, provides the corresponding data based on the EEA and GCP databases.<br><br>

<b>Pre-filled figures</b><br><br>

The pre-filled values refer to global CO2 emissions. The global CO2 budget of 400 Gt from 2020 on corresponds to a 67% probability of keeping the 1.5°C limit according to IPCC. 2019 was chosen as the reference year. Source for global CO2 emissions: GCP resp. our <a href = 'http://EU.national-budgets.climate-calculator.info', target= '_blank'>web app</a>.<br><br>

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

Here you can set the lowest possible value for annual emissions. An indication of 0 is equivalent to net zero emissions by 2100, while a negative value assumes that net negative annual emissions are possible.<br><br>

Please note that setting a positive value as minimum means that the given emission budget would be exceeded from 2101 on.<br><br>

If net negative emissions are allowed, the budget may be temporarily exceeded. These overshoot amounts will then be offset by net negative emissions by 2100. The actual overshoot amounts per scenario type are displayed in the table above the emission paths.<br><br>
<a href = 'http://luc.climate-calculator.info', target= '_blank'>Here</a> is a background text on determining the scope of net negative CO2 emissions.<br><br>

"),
actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
})

# Initial change rate ####

output$infotext_initial_change_rate <- renderUI({
  tags$div(paste("percental emissions change between", input$start_year - 1, "vs.", input$start_year), style = "font-weight:bold; float:left;")
})

output$box_info_initial_change_rate <- renderUI({
  hidden(div(
    class = "info-box", style = "left:390px; width:500px;", id = "info_initial_change_rate",
    paste0("In the scenario types RM 3 - 5, the start change rate must be specified: How much should emissions change between ", input$start_year - 1, " and ", input$start_year, "?"),
    HTML("<br/><br/>

Example: Input \"-1.5\" if emissions went down by 1.5 percent.<br><br>

Since this starting change rate is the basis for all the following ones, it should represent a <b>normalised value</b> (without exceptional events). The actual change rate from 2019 can be helpful for this.<br><br>

Due to the solution algorithm used here, no positive starting change rate is possible. If you want to use a positive value, we can refer to our <a href = 'http://climate-calculator.info', target= '_blank'>Excel tools</a>.<br /><br />

"),actionLink("close_info_initial_change_rate", icon = icon("window-close"), label = "Close")
    ))
})

# Others ####

observe({
  if (input$initial_change_rate > 0) {
    showModal(
    modalDialog(
      title = "Input restriction", p("Initial change rate can only be 0 or negative.", style = "font-size:16px"),
      footer = modalButton("Close")
    ))
    updateNumericInput(session, "initial_change_rate", value = 0)
  }
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

observeEvent(input$toggle_report_questions, {
  shinyjs::show("report_questions")
}, ignoreInit = T)

observeEvent(input$close_report_questions, {
  shinyjs::hide("report_questions")
})

observeEvent(input$link_info_initial_change_rate, {
  shinyjs::toggle("info_initial_change_rate")
})

observeEvent(input$close_info_initial_change_rate, {
  shinyjs::hide("info_initial_change_rate")
})