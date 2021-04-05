"Report which can be exported from the tool"

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
    params <- list(first_year = FIRST_YEAR,
                   result = result(),
                   date_display_range = date_display_range(),
                   eu_past_emissions = EU_PAST_EMISSIONS,
                   overshoot_amounts = overshoot_amounts(),
                   colors_to_display = colors_to_display(),
                   eu_emissions_1990 = EU_EMISSIONS_1990,
                   threshold_linear_rm1 = THRESHOLD_LINEAR_RM1,
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

