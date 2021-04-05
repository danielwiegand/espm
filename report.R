"Report which can be exported from the tool"

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
                   overshoot_amounts = overshoot_amounts(),
                   colors_to_display = colors_to_display(),
                   threshold_linear_rm1 = THRESHOLD_LINEAR_RM1,
                   emission_budget = input$emission_budget,
                   max_negative_emissions_gt = max_negative_emissions_gt()*-1
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

