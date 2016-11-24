
# open libraries
library(shiny)
library(stringr)
library(dplyr)
library(reshape2)
library(DT)
library(ggplot2)
theme_set(theme_bw())



# function to calculate retirement
retire <- function(retire_early, yearly_spend, tax_starting_principle, nontax_starting_principle,
                   start_age = 25, access_nontax = 60,
                   growth_rate = 0.05, tax_yearly_add = 0, nontax_yearly_add = 0
){

  # initialize parameters
  age <- start_age:100
  years_from_25 <- 1:length(age)
  year_retire_early <- which(retire_early == age)
  year_access_nontax <- which(access_nontax == age)

  # initiate accounts
  initiate <- function() numeric(length(age))
  tax_principle <- initiate()
  tax_interest <- initiate()
  tax_total <- initiate()
  nontax_principle <- initiate()
  nontax_interest <- initiate()
  nontax_total <- initiate()

  # set account principles
  tax_principle[1] <- tax_starting_principle
  nontax_principle[1] <- nontax_starting_principle

  # set variable for changing withdrawals from accounts
  changed_year_nontax <- FALSE

  # calculate account values
  for(year in years_from_25){

    #####################################
    # RESET NONTAX ACCESS - ROTH LADDER #
    #####################################
    if(year > year_retire_early & !changed_year_nontax){
      if(tax_total[year - 1] < yearly_spend * 6){
        if(year_access_nontax > (year + 5)){

          # change if previous year's taxable account total isn't enough (depending on yearly spend)
          # no need to change if don't need roth ladder
          previous_year_access <- year_access_nontax
          year_access_nontax <- year + 5
          changed_year_nontax <- TRUE

        }
      }
    }

    ##################
    # REGULAR RETIRE #
    ##################
    if(year >= year_access_nontax){

      # taxable account
      tax_principle[year] <- tax_total[year - 1]
      tax_interest[year] <- tax_principle[year] * growth_rate
      tax_total[year] <- tax_principle[year] + tax_interest[year]

      # nontaxable accounts
      nontax_principle[year] <- nontax_total[year - 1]
      nontax_interest[year] <- nontax_principle[year] * growth_rate
      nontax_total[year] <- max(nontax_principle[year] + nontax_interest[year] - yearly_spend, 0)

      ################
      # EARLY RETIRE #
      ################
    } else if(year >= year_retire_early){

      # taxable account
      tax_principle[year] <- tax_total[year - 1]
      tax_interest[year] <- tax_principle[year] * growth_rate
      tax_total[year] <- max(tax_principle[year] + tax_interest[year] - yearly_spend, 0)

      # nontaxable accounts
      nontax_principle[year] <- nontax_total[year - 1]
      nontax_interest[year] <- nontax_principle[year] * growth_rate
      nontax_total[year] <- nontax_principle[year] + nontax_interest[year]

      #################
      # WORKING YEARS #
      #################
    } else if(year != 1){

      # taxable account
      tax_principle[year] <- tax_total[year - 1] + tax_yearly_add
      tax_interest[year] <- tax_principle[year] * growth_rate
      tax_total[year] <- tax_principle[year] + tax_interest[year]

      # nontaxable accounts
      nontax_principle[year] <- nontax_total[year - 1] + nontax_yearly_add
      nontax_interest[year] <- nontax_principle[year] * growth_rate
      nontax_total[year] <- nontax_principle[year] + nontax_interest[year]

      ##############
      # INITIALIZE #
      ##############
    } else if(year == 1){

      # taxable account
      tax_interest[1] <- 0
      tax_total[1] <- tax_principle[1] + tax_interest[1]

      # nontaxable accounts
      nontax_interest[1] <- 0
      nontax_total[1] <- nontax_principle[1] + nontax_interest[1]
    }

  }

  # create a dataframe for data
  d <- data.frame(year = years_from_25, age,
                  tax_principle, tax_interest, tax_total,
                  nontax_principle, nontax_interest, nontax_total)

  # return data
  return( list(data = d, nontax_access = year_access_nontax) )

}



# server functions
shinyServer(function(input, output) {


  # run the retirement calculator
  run_calc <- reactive({

    retire(
      retire_early = input$retire_early, yearly_spend = input$yearly_spend, tax_starting_principle = input$tax_starting_principle, nontax_starting_principle = input$nontax_starting_principle,
      start_age = input$start_age, access_nontax = 60,
      growth_rate = input$growth_rate, tax_yearly_add = input$tax_yearly_add, nontax_yearly_add = input$nontax_yearly_add
    )

  })

  # print out summary of results
  output$summary <- renderUI({

    # obtain data
    age <- input$start_age:100
    retire_data <- run_calc()
    d2 <- subset(retire_data$data, age == input$retire_early)
    year_access_nontax <- retire_data$nontax_access

    # obtain text
    HTML(paste(
      "Years Working:", input$retire_early - input$start_age, "<br/>",
      "Assets in Taxable Account at RE ($):", round(d2$tax_total, 2), "<br/>",
      "Assets in Untaxable Account at RE ($):", round(d2$nontax_total, 2), "<br/>",
      "Assets Total at RE ($):", round(d2$tax_total + d2$nontax_total, 2), "<br/>",
      "<br/>",
      "Age Retire:", input$retire_early, "<br/>",
      "Age Start Roth Ladder:", age[year_access_nontax] - 5, "<br/>",
      "Age Access Retirement Accounts:", age[year_access_nontax], "<br/>",
      "<br/>",
      "Assets in Taxable Account at 100 ($):", round(run_calc()$data$tax_total[length(age)], 2), "<br/>",
      "Assets in Untaxable Account at 100 ($):", round(run_calc()$data$nontax_total[length(age)], 2), "<br/>"
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

    ggplot(plot_data() %>% subset(grepl("interest", variable)), aes(age, value)) +
      geom_line(size = 1.1) +
      geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "red") +
      facet_grid(~ variable) +
      labs(x = "Age", y = "Interest Earned ($)", title = "Interested Earned by Age")

  })

  output$totalPlot <- renderPlot({

    ggplot(plot_data() %>% subset(grepl("total", variable)), aes(age, value)) +
      geom_line(size = 1.1) +
      geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "red") +
      facet_grid(~ variable) +
      labs(x = "Age", y = "Total ($)", title = "Account Assets by Age")

  })


})
