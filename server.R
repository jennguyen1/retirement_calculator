# Server Code for Calculator
# Date: July 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shiny)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(DT)
library(htmltools)
library(ggplot2)
theme_set(theme_bw(base_size = 16))

# pull in code
source("www/util.R")
source("www/compound_interest_calc.R")

# server functions
shinyServer(function(input, output) {

  # CONFIGURE INPUTS 

  my_inputs <- reactiveValues(inputs = NULL, clicked_once = FALSE)
  observeEvent(input$submit, {
    my_inputs$clicked_once <- TRUE
    my_inputs$inputs <- list(
      start_age = input$start_age,
      retire_age = input$retire_age,
      growth_rate = input$growth_rate,
      savings_increase = input$savings_increase,
      yearly_spend = input$yearly_spend,
      tax_starting_principle = input$tax_starting_principle,
      tax_defer_starting_principle = input$tax_defer_starting_principle,
      tax_exempt_starting_principle = input$tax_exempt_starting_principle,
      tax_exempt_starting_contributions = input$tax_exempt_starting_contributions,
      tax_yearly_add = input$tax_yearly_add,
      tax_defer_yearly_add = input$tax_defer_yearly_add,
      tax_exempt_yearly_add = input$tax_exempt_yearly_add
    )
  })

  # store/load values for bookmarking
  onBookmark(function(state) {
    state$values$inputs <- my_inputs$inputs
  })
  onRestore(function(state) {
    my_inputs$inputs <- state$values$inputs
  })
  setBookmarkExclude(c("submit", "sidebarCollapsed", "sidebarItemExpanded"))

  # input vars event reactive
  get_inputs <- reactive({

    validate(
      need(my_inputs$inputs, "")
    )

    need_args <- c(
      "start_age", "retire_age", 
      "growth_rate", "savings_increase", "yearly_spend", 
      "tax_starting_principle", "tax_defer_starting_principle", "tax_exempt_starting_principle", "tax_exempt_starting_contributions",
      "tax_yearly_add", "tax_defer_yearly_add", "tax_exempt_yearly_add"
    )
    need_inputs <- map(need_args, ~ my_inputs$inputs[[.x]])
    validate(
      need(all(purrr::map_lgl(need_inputs, ~ !is.na(.x))), "Please provide all requested inputs"),
      need(all(purrr::map_lgl(need_inputs, ~ .x >= 0)), "All values must be >= 0"),
      need(dplyr::between(my_inputs$inputs$start_age, 16, 99), "Start age must be between 16 and 99"),
      need(dplyr::between(my_inputs$inputs$retire_age, 18, 100), "Retire age must be between 18 and 100"),
      need(my_inputs$inputs$retire_age > my_inputs$inputs$start_age, "Retire age must be greater than current age"),
      need(dplyr::between(my_inputs$inputs$growth_rate, 0, 0.5), "Growth rate must be between 0 and 0.5"),
      need(dplyr::between(my_inputs$inputs$savings_increase, 0, 0.5), "Savings increase must be between 0 and 0.5")
    )

    my_inputs$inputs
  })

  # RUNNING THE APP ######################

  # run the retirement calculator
  run_calc <- reactive({
    retire(
      start_age = get_inputs()$start_age,
      retire_age = get_inputs()$retire_age,
      growth_rate = get_inputs()$growth_rate,
      savings_increase = get_inputs()$savings_increase,
      yearly_spend = get_inputs()$yearly_spend,
      tax_starting_principle = get_inputs()$tax_starting_principle,
      tax_defer_starting_principle = get_inputs()$tax_defer_starting_principle,
      tax_exempt_starting_principle = get_inputs()$tax_exempt_starting_principle,
      tax_exempt_starting_contributions = get_inputs()$tax_exempt_starting_contributions,
      tax_yearly_add = get_inputs()$tax_yearly_add,
      tax_defer_yearly_add = get_inputs()$tax_defer_yearly_add,
      tax_exempt_yearly_add = get_inputs()$tax_exempt_yearly_add
    )
  })

  # print out summary of results
  output$summary <- renderUI({
    age <- get_inputs()$start_age:100
    retire_data <- run_calc()
    process_summary_data(l = retire_data, age = age)
  })

  # output data as csv
  output$downloadData <- downloadHandler(
    filename = function(){ 
      stringr::str_interp("Retire_at_${{get_inputs()$retire_age}}_spending_${{get_inputs()$yearly_spend}}_at_${{get_inputs()$growth_rate}}.csv")
    },
    content = function(file) {
      dat <- annotate_data(run_calc()) %>% 
        purrr::set_names(c(
          "Year", "Age", 
          "Tax Accounts Principle", "Tax Accounts Contribution", "Tax Accounts Interest", "Tax Accounts Total", 
          "Tax-Deferred Accounts Principle", "Tax-Deferred Accounts Contribution", "Tax-Deferred Accounts Interest", "Tax-Deferred Accounts Total", 
          "Tax-Exempt Accounts Principle", "Tax-Exempt Accounts Contribution", "Tax-Exempt Accounts Interest", "Tax-Exempt Cumulative Contributions", "Tax-Exempt Accounts Total", 
          "Net Worth", "Note"
        ))
      write.csv(dat, file, row.names = FALSE)
    }
  )

  # generate plots
  plot_data <- reactive( 
    annotate_data(run_calc()) %>% 
      tidyr::pivot_longer(-c(year, age, note), names_to = "variable", values_to = "value")
  )
  
  output$networthPlot <- renderPlot({
    make_networth_plot( plot_data() )
  })

  # tables
  output$table <- DT::renderDataTable({

    # obtain the data
    dat <- format_table_for_display(run_calc()$data) 

    # render the table
    d <- datatable(
      dat,
      rownames = FALSE,
      container = format_header(),
      extensions = "Buttons",
      options = list(
        dom = "t",
        ordering = FALSE,
        scrollX = TRUE, scrollY = 400,
        pageLength = 200
      )
    ) %>%
      formatCurrency(purrr::discard(colnames(dat), ~ .x == "Age"), digits = 0) %>% 
      formatStyle("Age", target = "row", backgroundColor = make_tab_colors(run_calc())) # row color retirement stage 
    d
  })
  
  # ui elemnts
  # color key 
  legend <- p(span(
    strong("white")), " = work; ", 
    span(strong("blue")), " = early retirement via tax-exempt contributions and/or taxable accounts; ", 
    span(strong("light green")), " = early retirement via Roth ladder; ", 
    span(strong("green")), " = retirement via locked retirement accounts; ", 
    span(strong("red")), " = ran out of money"
  )
  
  output$summary_box <- renderUI({
    box(
      title = "Summary of Results", width = NULL,
      solidHeader = TRUE, status = "success", 
      collapsible = TRUE, collapsed = !my_inputs$clicked_once,
        
      fluidRow(
        column(width = 4, h4(htmlOutput("summary"))), 
        column(width = 8, plotOutput("networthPlot"))
      ),
      legend
    )
  })
  
  output$detail_box <- renderUI({
    box(
      title = "Detailed Results", width = NULL,
      solidHeader = TRUE, status = "success",
      collapsible = TRUE, collapsed = TRUE,
      
      downloadButton('downloadData', 'Download'),
      p(),
      DT::dataTableOutput("table"),
      
      p(),
      legend
    )
  })
})
