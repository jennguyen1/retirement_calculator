# UI Code for Calculator
# Date: July 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shiny)
library(shinydashboard)

# UI functions
dashboardPage(skin = "green",

  # app title
  dashboardHeader(title = "Retirement Calculator"),

  # disable sidebar
  dashboardSidebar(disable = TRUE),

  dashboardBody(verticalLayout(

    # inputs
    box(
      title = "Set Input Parameters", width = NULL,
      solidHeader = TRUE, status = "success", collapsible = TRUE,


      tabBox(width = NULL, selected = "Input Data",

        tabPanel("Instructions",
          p("This is a R Shiny application that computes rough projections for retirement savings."),
          p("The retirement account balances are initialized with the amounts in the ", span(strong("Initial Principle")), " fields. In the first year, the ", span(strong("Amount Added")), " will be added to the ", span(strong("Initial Principle")), ". Note that nontaxable accounts refer to accounts in which there are restrictions on withdrawal without penalty such as 401k, traditional IRA, Roth IRA, and HSA whereas taxable accounts refers to accounts in which there are no restrictions on withdrawal in early retirement such as 457b or a personal brokerage account."),
          p("During the working years, the ", span(strong("Amount Added")), " value is added to the principle of the prior year in each respective account. Interest for that given year is computed based on the new principle at a rate of ", span(strong("Growth Rate")), "."),
          p("After the ", span(strong("Retire Age")), ", the ", span(strong("Amount Added")), " value is reduced to 0 and the accounts continue to earn interest at the specified rate. In addition, the ", span(strong("Spending per Year")), " amount is deducted from the total. The ", span(strong("Spending per Year")), " is withdrawn from the taxable account until it is critically low and then from the nontaxable account. The age for withdrawal from nontaxable accounts without penalty is 60 (rounded up from 59.5). If it is likely that the nontaxable account will be depleted before then, individuals have the option to apply the Roth Ladder Conversion."),
          p("The program uses the following algorithm for determining if a Roth Ladder Conversion is needed and when it should be applied. If the previous year’s taxable account principle is less than 6 times the spending per year and there is more than 5 years before age 60, then the program recommends starting the Roth Ladder on that year so that the nontaxable accounts can be accessed 5 years after that. The program will issue a warning if the taxable account is too low to sustain the spending per year for 5 years necessary adequately setup the Roth Ladder. Once the nontaxable account can be accessed, the taxable account balance is transferred to the nontaxable account and all further withdrawals occur in the nontaxable account. The program will issue a warning if the nontaxable account is depleted before age 100."),
          p("The program projects balances until age 100. A summary of results will be printed out and a csv file of yearly account details will be available for download in the Summary of Results tab. The Plots tab contains plots of Account Totals by Age for each respective account as well as Interest Earned Per Year by Age.")
        ),

        tabPanel("Input Data",
          fluidRow(
            column(width = 6, numericInput("start_age", "Current Age", 25)),
            column(width = 6, uiOutput("retire_age"))
          ),
          fluidRow(
            column(width = 6, numericInput("yearly_spend", "Spending Per Year", 30000, min = 0, step = 1000)),
            column(width = 6, numericInput("growth_rate", "Growth Rate", 0.05, min = 0, max = 0.20, step = 0.01))
          ),
          fluidRow(
            column(width = 6, numericInput("tax_starting_principle", "Initial Principle in Taxable Accounts", 0, min = 0, step = 500)),
            column(width = 6, numericInput("nontax_starting_principle", "Initial Principle in Retirement Accounts", 11000, min = 0, step = 500))
          ),
          fluidRow(
            column(width = 6, numericInput("tax_yearly_add", "Amount Added to Taxable Account Yearly", 18000, min = 0, step = 500)),
            column(width = 6, numericInput("nontax_yearly_add", "Amount Added to Retirement Account Yearly", 36000, min = 0, step = 500))
          )
        )
      )

    ),


    # outputs: summary
    box(
      title = "Summary of Results", width = NULL,
      solidHeader = TRUE, status = "success", collapsible = TRUE,

      h4(htmlOutput("summary")),
      downloadButton('downloadData', 'Download')
    ),

    # outputs: plots
    box(
      title = "Plots", width = NULL,
      solidHeader = TRUE, status = "success",
      collapsible = TRUE, collapsed = TRUE,

      plotOutput("totalPlot"),
      plotOutput("interestPlot")
    )
  ))
)


