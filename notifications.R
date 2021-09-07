"Notifications and warning / info texts"

# General info texts ####

output$title <- renderUI({
  output = tagList()
  output[[1]] <- HTML("Extended Smooth Pathway Model<br><span style = 'font-size:20px;'>Comprehensive tool for calculating emission paths that adhere to a predefined emission budget</span>")
  output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
  output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
                            HTML("
                                 The ESPM makes it possible to identify emission paths that adhere to a predefined emission budget. This allows <b>Paris-compatible emission targets</b> to be identified.<br /><br />
                                 Starting from a <b>budget you set</b>, this universal ESPM tool allows you to derive the <b>full range of plausible emission paths</b> that meet that budget. This range is provided by the RM Scenario Types, which differ in their <b>assumptions</b> about the <b>course</b> of <b>annual changes</b> (see plot 'Annual emission change rates'). Which scenario type is appropriate in each case must be decided from a scientifically based overall climate policy view.<br /><br />
                                 The emissions budget you set can be global or refer to a national, regional, or other unit.<br /><br />
                                 A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target = '_blank'>here</a>. Further tools with additional features for calculating emission paths can be found at <a href ='http://www.save-the-climate.info'>www.save-the-climate.info</a>. These tools can also be used to determine Paris-compatible national budgets.<br /><br />
                                 "),
                            actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
  
  return(output)
})

showModal(modalDialog(
  title = "Extended Smooth Pathway Model",
  HTML("
  The ESPM makes it possible to identify emission paths that adhere to a predefined emission budget. This allows <b>Paris-compatible emission targets</b> to be identified.<br /><br />
  Starting from a <b>budget you set</b>, this universal ESPM tool allows you to derive the <b>full range of plausible emission paths</b> that meet that budget. This range is provided by the RM Scenario Types, which differ in their <b>assumptions</b> about the <b>course</b> of <b>annual changes</b> (see plot 'Annual emission change rates'). Which scenario type is appropriate in each case must be decided from a scientifically based overall climate policy view.<br /><br />
  The emissions budget you set can be global or refer to a national, regional, or other unit.<br /><br />
  A brief background paper on the ESPM can be found <a href = 'https://www.klima-retten.info/PDF/ESPM_Background.pdf', target = '_blank'>here</a>. Further tools with additional features for calculating emission paths can be found at <a href ='http://www.save-the-climate.info'>www.save-the-climate.info</a>. These tools can also be used to determine Paris-compatible national budgets.<br /><br />
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
  Compared to the Excel tools (download at <a href = 'http://save-the-climate.info' target = '_blank'>http://save-the-climate.info</a>), here the rate of change for 2020 in the scenario types RM 3 - 5 is 50% of the rate of change that results for RM-6. In the Excel tools, this rate of change 2020 is an input value and a temporary corona effect can also be taken into account there.<br /><br />"),
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
<h4>Data input</h4>
In order to calculate emission paths and emission targets with this tool, you need to know the budget to be met and the emissions from 2019 and a reference year.<br /><br />
Indicate the emission budget for the timeframe 2020 - 2100, which should be met by the emission paths. This emission budget can be global or based on a national, regional or any other entity. Please ensure that the emissions of the budget and those of the base or reference year refer to the same emissions in terms of content. For example, if the emission budget covers emissions from land use change, the emissions in the base and reference year should include emissions from land use change as well.<br /><br />
<h4>Determination of emission budgets</h4>
Regarding the global emission budget, we refer in particular to the <a href = 'https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_SPM.pdf?__cf_chl_jschl_tk__=pmd_8f233c9747284ea4b54a61a50c00463f9d293c81-1628498145-0-gqNtZGzNAg2jcnBszQeO', target = '_blank'>IPCC Report AR6/WGI</a> from 2021. According to this report, compliance with the 1.5°C limit corresponds with a probability of 67% to a remaining CO2 budget of 400 Gt. <a href ='https://www.klima-retten.info/PDF/IPCC_AR6_Remaining_Carbon_Budgets.pdf', target = '_blank'>Here</a> we have summarized the statements of the IPCC. The following table summarizes the main results:<br /><br />
Regarding a remaining budget for the EU and Germany, the '<a href = 'https://www.klima-retten.info/PDF/2020_08_environmental_report_chapter_02.pdf' target = '_blank'>Environmental Report 2020</a>' by the German Advisory Council on the Environment can be of use.<br /><br />
The following principles can be relevant for the calculation of country budgets:
<ul>
  <li>historical responsibility</li>
  <li>economical capacities</li>
  <li>equality / justice</li>
  <li>present state of things (grandfathering)</li>
  <li>Cost efficiency</li>
</ul>
This tool is intended to enable users to derive emission paths for a country budget which has been derived with whatever way.<br /><br />
With our Excel tools for the calculation of Paris-compatible emission paths (download under <a href = 'http://save-the-climate.info' target ='_blank'>www.save-the-climate.info</a>), national emission budgets can be determined. For the EU, we provide a dedicated web app (<a href = 'http://eu.climate-calculator.info' target = '_blank'>http://eu.climate-calculator.info</a>) which determines a budget for the EU as well.<br /><br />
<h4>Data sources</h4>
We recommend the EU's 'Emissions Database for Global Atmospheric Research' (<a href = 'https://edgar.jrc.ec.europa.eu/' target = '_blank'>EDGAR</a>) and the <a href = 'https://www.globalcarbonproject.org' target = '_blank'>Global Carbon Project</a>. For detailed information about the EU's emissions, we refer to the European Environment Agency (<a href = 'https://www.eea.europa.eu/' target = '_blank'>EEA</a>).<br /><br />
<h4>Pre-filled figures</h4>
The pre-filled budget of 400 Gt corresponds to a 67% probability of staying below the 1.5°C threshold according to IPCC. 2010 was chosen as a reference year.<br /><br />
<h4>Divergence of emission budget</h4>
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
If net negative emissions are allowed, the budget may be temporarily exceeded. These overshoot amounts will then be offset by net negative emissions by 2100. The actual overshoot amounts per scenario type are displayed in the table above the emission paths. <br /><br />However, it should be noted that the overshoot amounts can also lead to dangerous <b>tipping points</b> in the climate system being exceeded. Also, it should be pointed out that the <b>costs</b> of actively capturing CO2 are still unclear and that there are major methodological and substantive problems in <b>quantifying</b> sinks and questions regarding their sustainable value.<br /><br />
Negative CO2 emissions will be a necessity to compensate for other greenhouse gases like methane and nitrous oxide e.g. from agriculture. If you use an emissions budget covering CO2 only, these negative CO2 emissions are not considered here and have to be provided in addition.<br /><br />
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