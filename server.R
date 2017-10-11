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
theme_set(theme_bw())

source("compound_interest_calc.R")

# server functions
shinyServer(function(input, output) {

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

    retire(
      start_age = input$start_age, retire_early = input$retire_early,
      access_nontax = 60, yearly_spend = input$yearly_spend,
      tax_starting_principle = input$tax_starting_principle, nontax_starting_principle = input$nontax_starting_principle,
      growth_rate = input$growth_rate, tax_yearly_add = input$tax_yearly_add, nontax_yearly_add = input$nontax_yearly_add
    )

  })

  # print out summary of results
  output$summary <- renderUI({

    # obtain data
    age <- input$start_age:100
    retire_data <- run_calc()
    d2 <- subset(retire_data$data, age == (input$retire_early - 1))
    year_access_nontax <- retire_data$nontax_access

    # process data for Roth Ladder
    roth_ladder <- ifelse(retire_data$changed_year_nontax, paste("Age Start Roth Ladder:", max(age[year_access_nontax] - 5, input$retire_early + 1), "<br/>"), "")

    # obtain text
    HTML(paste(
      "Years Working:", input$retire_early - input$start_age, "<br/>",
      "Assets in Taxable Account at RE ($):", d2$tax_total, "<br/>",
      "Assets in Retirement Account at RE ($):", d2$nontax_total, "<br/>",
      "Assets Total at RE ($):", d2$net_worth, "<br/>",
      "<br/>",
      "Age Retire:", input$retire_early, "<br/>",
      roth_ladder,
      "Age Access Retirement Accounts:", age[year_access_nontax], "<br/>",
      retire_data$went_broke_tax, retire_data$went_broke_nontax,
      "<br/>",
      "Assets in Taxable Account at 100 ($):", run_calc()$data$tax_total[length(age)], "<br/>",
      "Assets in Retirement Account at 100 ($):", run_calc()$data$nontax_total[length(age)], "<br/>"
    ))

  })

  # output data as csv
  output$downloadData <- downloadHandler(
    filename = function() paste0("Retire_at_", input$retire_early, "_spending_", input$yearly_spend, "_at_", input$growth_rate, "_growth", ".csv"),
    content = function(file) write.csv(run_calc()$data, file, row.names = FALSE)
  )

  # generate plots
  plot_data <- reactive( melt(run_calc()$data, id.vars = c("year", "age")) )

  output$interestPlot <- renderPlot({

    plot_data() %>%
      subset(str_detect(variable, "interest")) %>%
      mutate(variable = factor(ifelse(variable == "tax_interest", "taxable", "nontaxable"), levels = c("taxable", "nontaxable"))) %>%
      ggplot(aes(age, value)) +
      geom_line(size = 1.1) +
      geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "red") +
      facet_grid(~ variable) +
      labs(x = "Age", y = "Interest Earned Per Year ($)", title = "Interested Earned Per Year by Age")

  })

  output$totalPlot <- renderPlot({


    plot_data() %>%
      subset(str_detect(variable, "total")) %>%
      mutate(variable = factor(ifelse(variable == "tax_total", "taxable", "nontaxable"), levels = c("taxable", "nontaxable"))) %>%
      ggplot(aes(age, value)) +
      geom_line(size = 1.1) +
      geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "red") +
      facet_grid(~ variable) +
      labs(x = "Age", y = "Total ($)", title = "Account Assets by Age")

  })


})
