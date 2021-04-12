"Notifications and warning / info texts"

# General info texts ####

output$title <- renderUI({
  output = tagList()
  output[[1]] <- HTML("Extended Smooth Pathway Model<br><span style = 'font-size:20px;'>Comprehensive tool for calculating emission paths that adhere to a predefined emission budget</span>")
  output[[2]] <- actionLink("link_info_general", "", icon = icon("info-circle"), style = "font-size:20px; margin-top:20px; margin-left:10px;")
  output[[3]] <- hidden(div(class = "info-box", style = "left:330px; width:500px;", id = "info_general", 
                            HTML("
                                 The ESPM makes it possible to identify emission paths that adhere to a predefined emission budget. In this universal tool, this emission budget can be global or based on a national, regional or any other entity.<br /><br />
                                 The scenario types used to determine the emission paths differ in their assumptions about the annual emission changes (see plot 'Annual emission change rates'). The scenario types RM 1 – 6 cover a full bandwidth of plausible emission paths.<br /><br />
                                 An important question concerns the possibility of future negative emissions. The app allows you specify the potential for net negative emissions. Please note to the constraints mentioned in the appropriate help text on this page.<br /><br />
                                 You can find other tools for the calculation of Paris-compatible emission paths at <a href ='http://www.save-the-climate.info'>www.save-the-climate.info</a>.<br /><br />
                                 "),
                            actionLink("close_info_general", icon = icon("window-close"), label = "Close")))
  
  return(output)
})

showModal(modalDialog(
  title = "Smooth Emission Paths",
  HTML("
    The Smooth Emission Paths approach makes it possible to determine emission paths that adhere to a predefined emission budget. The emission budget can be global or related to a nation or region.<br /><br />
    The scenario types used to determine the emission paths differ in their assumptions about the annual emission changes (see plot 'Annual emission change rates'). The scenario types offered cover the range of plausible emission paths well.<br /><br />
    An important question concerns the possibility of future negative emissions. The app allows you specify the potential for net negative emissions. Please note to the constraints mentioned in the appropriate help text on this page.<br /><br />
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
  hidden(div(class = "info-box", style = "left:330px;", id = "info_scenario_type", HTML(
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
      The 2020 rate of change for the scenario types RM 3-5 is 50% of the change rate of RM-6 (where the emission path is a straight line). In the Excel tools, this change rate is an input value
    </li>
    <li>
      In the scenario types RM 1-5, the emission path becomes a straight line when the emissions fall below a given threshold. This threshold is 3.5% of the 2019 emissions for this online tool.
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
Please ensure that the emissions of the budget and those of the base or reference year refer to the same emissions in terms of content. For example, if the emission budget covers emissions from land use change, the emissions in the base and reference year should include emissions from land use change as well.<br />
The pre-filled figures refer to the global CO2 emissions including emissions from land use change. The budget used is based on a global emission budget of 420 Gt from 2018 on, which was lowered by the emissions of the years 2018 and 2019. 2010 was chosen as a reference year. Regarding the global emission budget, we refer in particular to the IPCC Special Report 2018 (<a href = 'www.ipcc.ch/sr15/' target = '_blank'>www.ipcc.ch/sr15/</a>).<br />
<a href ='https://www.klima-retten.info/PDF/IPCC_SR15_Remaining_Carbon_Budgets.pdf', target = '_blank'>Here</a> we have summarized the statements of the IPCC. For the determination of national budgets, we refer to e.g.:
<ul>
  <li>Our web app determining Paris compatible emission paths for the EU: <a href = 'http://eu.climate-calculator.info' target = '_blank'>http://eu.climate-calculator.info</a></li>
  <li>Our further Excel tools for the determination of Paris compatible emission paths for all countries: <a href = 'http://www.save-the-climate.info/' target = '_blank'>http://www.save-the-climate.info/</a></li>
  <li>Possible data sources: <a href = 'https://www.globalcarbonproject.org/' target = '_blank'>GCP</a>, <a href = 'https://edgar.jrc.ec.europa.eu/' target = '_blank'>EDGAR</a>, EFA</li>
</ul>
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
    For information regarding smooth national emission paths, refer to <a href = 'http://save-the-climate.info', target = '_blank'>www.save-the-climate.info</a>.<br /><br />
    All code to create this website is available on my <a href = 'https://github.com/danielwiegand/espm'>GitHub page</a>. For comments and suggestions contact me on daniel.a.wiegand [at] posteo.de.<br />"),
             actionLink("close_author", icon = icon("window-close"), label = "Close", style = "float:right;")))
})

# Negative emissions ####

output$box_info_negative_emissions <- renderUI({
  hidden(div(class = "info-box", style = "left:390px; width:500px;", id = "info_negative_emissions", 
             HTML("
 An important question concerns the possibility of future negative emissions. The app allows you specify the potential for net negative emissions by specifying a percentage that is applied to the current emissions. This percentage then determines the negative minimum value of the emission paths by 2100. An indication of 0% is equivalent to net zero emissions by 2100. Setting a positive value as minimum would not make sense in the given context, because the given emission budget would be exceeded through the emissions after the year 2100.<br />
 If net negative emissions are allowed, the budget may be temporarily exceeded. These overshoot amounts will then be offset by net negative emissions by 2100. However, it should be noted that the overshoot amounts can also lead to dangerous <b>tipping points</b> in the climate system being exceeded. Also, it should be pointed out that the <b>costs</b> of actively capturing CO2 are still unclear and that there are major methodological and substantive problems in <b>quantifying</b> sinks and questions regarding their sustainable value.<br />
 The actual overshoot amounts per scenario type are displayed in the table above the emission paths.<br />
                  "),
             actionLink("close_info_negative_emissions", icon = icon("window-close"), label = "Close")))
})

observeEvent(input$link_info_negative_emissions, {
  shinyjs::toggle("info_negative_emissions")
})

observeEvent(input$close_info_negative_emissions, {
  shinyjs::hide("info_negative_emissions")
})
