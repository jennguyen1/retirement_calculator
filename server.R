# Server Code for Calculator
# Date: July 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shiny)
library(stringr)
library(dplyr)
library(purrr)
library(reshape2)
library(DT)
library(htmltools)
library(ggplot2)
theme_set(theme_bw(base_size = 16))

# pull in code
source("scripts/util.R")
source("scripts/compound_interest_calc.R")

# server functions
shinyServer(function(input, output) {

  # CONFIGURE INPUTS #####################
  # ######################################
  
  my_inputs <- reactiveValues(inputs = NULL)
  observeEvent(input$submit, {
    my_inputs$inputs <- list(
      start_age = input$start_age,
      retire_age = input$retire_age,
      growth_rate = input$growth_rate,
      yearly_spend = input$yearly_spend, 
      yearly_spend = input$yearly_spend,
      tax_starting_principle = input$tax_starting_principle,
      nontax_starting_principle = input$nontax_starting_principle,
      tax_yearly_add = input$tax_yearly_add,
      nontax_yearly_add = input$nontax_yearly_add
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
    
    need_args <- c("start_age", "retire_age", "growth_rate", "yearly_spend", "tax_starting_principle", "nontax_starting_principle", "tax_yearly_add", "nontax_yearly_add")
    need_inputs <- map(need_args, ~ my_inputs$inputs[[.x]])
    validate(
      need(all(purrr::map_lgl(need_inputs, ~ !is.na(.x))), "Please provide all requested inputs"),
      need(all(purrr::map_lgl(need_inputs, ~ .x >= 0)), "All values must be >= 0"),
      need(dplyr::between(my_inputs$inputs$start_age, 16, 99), "Start age must be between 16 and 99"),
      need(dplyr::between(my_inputs$inputs$retire_age, 18, 100), "Retire age must be between 18 and 100"),
      need(my_inputs$inputs$retire_age > my_inputs$inputs$start_age, "Retire age must be less than current age"),
      need(dplyr::between(my_inputs$inputs$growth_rate, 0, 0.5), "Growth rate must be between 0 and 0.5")
    )
    
    my_inputs$inputs
  })
  
  # RUNNING THE APP ######################
  # ######################################
  
  # run the retirement calculator
  run_calc <- reactive({
    retire(
      start_age = get_inputs()$start_age,
      retire_age = get_inputs()$retire_age,
      growth_rate = get_inputs()$growth_rate,
      yearly_spend = get_inputs()$yearly_spend,
      tax_starting_principle = get_inputs()$tax_starting_principle,
      nontax_starting_principle = get_inputs()$nontax_starting_principle,
      tax_yearly_add = get_inputs()$tax_yearly_add,
      nontax_yearly_add = get_inputs()$nontax_yearly_add
    )
  })

  # print out summary of results
  output$summary <- renderUI({

    # obtain data
    age <- get_inputs()$start_age:100
    retire_data <- run_calc()
    process_summary_data(l = retire_data, age = age, retire_early_age = get_inputs()$retire_age)

  })

  # output data as csv
  output$downloadData <- downloadHandler(
    filename = function(){
      str_interp("Retire_at_${{get_inputs()$retire_age}}_spending_${{get_inputs()$yearly_spend}}_at_${{get_inputs()$growth_rate}}.csv")
    },
    content = function(file) {
      l <- run_calc()
      dat <- l$data %>%
        mutate(
          Notes = case_when(
            age == get_inputs()$start_age ~ "work",
            age == get_inputs()$retire_age ~ "early retirement via taxable accounts",
            age == l$roth_access & l$roth_access != l$retire_access ~ "early retirement via roth ladder",
            age == l$retire_access ~ "regular retirement via retirement accounts",
            TRUE ~ ""
        ))
      colnames(dat) <- c(
        "Year", "Age",
        "Tax Accounts Principle", "Tax Accounts Interest", "Tax Accounts Total",
        "Retirement Accounts Principle", "Retirement Accounts Interest", "Retirement Accounts Total",
        "Net Worth", "Notes"
      )
      write.csv(dat, file, row.names = FALSE)
    }
  )

  # generate plots
  plot_data <- reactive( melt(run_calc()$data, id.vars = c("year", "age")) )

  output$interestPlot <- renderPlot({
    make_interest_plot( plot_data(), get_inputs()$yearly_spend )
  })

  output$totalPlot <- renderPlot({
    make_total_plot( plot_data() )
  })

  # tables
  output$table <- DT::renderDataTable({
    
    # obtain the data
    dat <- format_table_for_display(run_calc()$data)

    # color the table
    col <- make_tab_colors(
      retire_age = get_inputs()$retire_age, 
      roth_access_age = run_calc()$roth_access, 
      retire_access_age = run_calc()$retire_access
    )
    milestones <- col$milestones
    colors <- col$colors

    # format header
    sketch = format_header()


    #' render the table
    d <- datatable(dat,
      rownames = FALSE,
      container = sketch,
      extensions = "Buttons",
      options = list(
        dom = "Bt",
        ordering = FALSE,
        buttons = I('colvis'),
        scrollX = TRUE, scrollY = 400,
        pageLength = 200
      )
    ) %>%

      # format currencies
      formatCurrency(purrr::discard(colnames(dat), ~ .x == "Age"), digits = 0) %>%

      # color rows based on retirement stage
      formatStyle(
        "Age",
        target = "row",
        backgroundColor = styleInterval(milestones, colors)
      )
    d
  })
})
