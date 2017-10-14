# Server Code for Calculator
# Date: July 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shiny)
library(stringr)
library(dplyr)
library(reshape2)
library(DT)
library(ggplot2)
theme_set(theme_bw(base_size = 16))

# pull in code
source("scripts/util.R")
source("scripts/compound_interest_calc.R")

# server functions
shinyServer(function(input, output) {

  # checks on input
  input_checks <- reactive({
    need_vars <- c("growth_rate", "nontax_starting_principle", "nontax_yearly_add", "retire_early", "start_age", "tax_starting_principle", "tax_yearly_add", "yearly_spend")
    missing_vars <- make_error_msg(!all(need_vars %in% names(input)), "Please provide all requested inputs")
    neg_vars <- make_error_msg(!all(sapply(need_vars, function(x) input[[x]] >= 0)), "All values must be >= 0")
    err_perc_growth <- make_error_msg((input$growth_rate < 0 | input$growth_rate > 1), "Growth rate must be between 0 and 1")
    err_progressive_age <- make_error_msg(input$retire_age <= input$current_age, "Retire age must be less than current age")

    return(list(missing_vars, neg_vars, err_perc_growth, err_progressive_age))
  })

  min_retire <- reactiveValues(age = 55)
  observeEvent(input$start_age, {
    min_retire$age <- input$start_age + 1
  })

  # set limit on the retire early age (> start age)
  output$retire_age <- renderUI({
    numericInput("retire_early", "Retire Age", 55, min = min_retire$age, max = 100)
  })

  # run the retirement calculator
  run_calc <- reactive({
    checks <- input_checks()
    throw_error <- check_throw_error(checks)

    if( !any(throw_error) ){
      retire(
        start_age = input$start_age,
        retire_age = input$retire_early,
        growth_rate = input$growth_rate,
        yearly_spend = input$yearly_spend,
        tax_starting_principle = input$tax_starting_principle,
        nontax_starting_principle = input$nontax_starting_principle,
        tax_yearly_add = input$tax_yearly_add,
        nontax_yearly_add = input$nontax_yearly_add
      )
    }
  })

  # print out summary of results
  output$summary <- renderUI({

    checks <- input_checks()
    throw_error <- check_throw_error(checks)

    # write error or display summary
    if( any(throw_error) ){
      msg <- checks[[which(throw_error)[1]]]
      HTML(msg)

    } else{
      # obtain data
      age <- input$start_age:100
      retire_data <- run_calc()
      process_summary_data(l = retire_data, age = age, retire_early_age = input$retire_early)

    }
  })

  # output data as csv
  output$downloadData <- downloadHandler(
    filename = function() paste0("Retire_at_", input$retire_early, "_spending_", input$yearly_spend, "_at_", input$growth_rate, "_growth", ".csv"),
    content = function(file) write.csv(run_calc()$data, file, row.names = FALSE)
  )

  # generate plots
  plot_data <- reactive( melt(run_calc()$data, id.vars = c("year", "age")) )

  output$interestPlot <- renderPlot({
    checks <- input_checks()
    throw_error <- check_throw_error(checks)
    if( !any(throw_error) ) make_interest_plot( plot_data(), input$yearly_spend )
  })

  output$totalPlot <- renderPlot({
    checks <- input_checks()
    throw_error <- check_throw_error(checks)
    if( !any(throw_error) ) make_total_plot( plot_data() )
  })

})
