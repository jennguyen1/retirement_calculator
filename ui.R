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
          p("The retirement account balances are initialized with the amounts in the ", span(strong("Initial Principle")), " fields. Contributions (", span(strong("Amount Added")), ") start on the same year as the ", span(strong("Start Age")), ". Note that retirement accounts refer to accounts in which there are restrictions on withdrawal without penalty such as 401k, traditional IRA, Roth IRA, and HSA whereas taxable accounts refers to accounts in which there are no restrictions on withdrawal in early retirement such as 457b or a personal brokerage account."),
          p("During the work years, the ", span(strong("Amount Added")), " value is added at the beginning of the year and compounded at the end of the year. Interest is calculated at a rate of ", span(strong("Growth Rate")), ". I recommend using an interest rate that has been adjusted for inflation."),
          p("After the ", span(strong("Retire Age")), ", the ", span(strong("Amount Added")), " value is reduced to 0 and the accounts continue to earn interest at the specified rate. In addition, the ", span(strong("Spending per Year")), " amount is deducted from the total. The ", span(strong("Spending per Year")), " is withdrawn from the taxable accounts until it is critically low and after which withdrawals switch to the retirement accounts. The age for withdrawal from retirement accounts without penalty is 60 (rounded up from 59.5). If it is likely that the taxable accounts are depleted before then, there is the option to apply the Roth Ladder Conversion."),
          p("The program uses the following algorithm for determining if a Roth Ladder Conversion is needed and when it should be applied. If the taxable accounts total is less than 6 times the spending per year and there are more than 5 years before one can access the retirement accounts without penalty, then the program recommends starting the Roth Ladder the next year so that the funds can be accessed 5 years after starting the Roth Ladder. The program will issue a warning if the taxable account balances are too low to sustain the spending per year for the 5 years necessary to adequately setup the Roth Ladder. Once the retirement accounts can be accessed (via Roth Ladder or regular retirement), the taxable account balances are transferred to the retirement accounts and all further withdrawals occur in the retirement accounts. The program will issue a warning if the retirement accounts are depleted before age 100."),
          p("The program projects balances until age 100. A summary of results is printed in the ", span(strong("Summary of Results")), " tab. The ", span(strong("Plots")), " tab contains plots of ", span(strong("Account Totals by Age")), " for each respective account type as well as ", span(strong("Interest Earned Per Year by Age")), ". A detailed table is printed and available for download in the ", span(strong("Detailed Results")), "tab.")
        ),

        tabPanel("Input Data",
          fluidRow(
            column(width = 6, numericInput("start_age", "Current Age", 25)),
            column(width = 6, uiOutput("retire_age"))
          ),
          fluidRow(
            column(width = 6, numericInput("yearly_spend", "Spending Per Year", 30000, min = 0, step = 1000)),
            column(width = 6, numericInput("growth_rate", "Growth Rate", 0.05, min = 0, max = 0.5, step = 0.01))
          ),
          fluidRow(
            column(width = 6, numericInput("tax_starting_principle", "Initial Principle in Taxable Accounts", 0, min = 0, step = 500)),
            column(width = 6, numericInput("nontax_starting_principle", "Initial Principle in Retirement Accounts", 11000, min = 0, step = 500))
          ),
          fluidRow(
            column(width = 6, numericInput("tax_yearly_add", "Amount Added to Taxable Account Yearly", 18000, min = 0, step = 500)),
            column(width = 6, numericInput("nontax_yearly_add", "Amount Added to Retirement Account Yearly", 36000, min = 0, step = 500))
          ), 
          actionButton("submit", "Submit")
        )
      )

    ),


    # outputs: summary
    box(
      title = "Summary of Results", width = NULL,
      solidHeader = TRUE, status = "success", collapsible = TRUE,

      h4(htmlOutput("summary"))
    ),

    # outputs: plots
    box(
      title = "Plots", width = NULL,
      solidHeader = TRUE, status = "success",
      collapsible = TRUE, collapsed = TRUE,

      plotOutput("totalPlot"),
      plotOutput("interestPlot")

    ),

    box(
      title = "Detailed Results", width = NULL,
      solidHeader = TRUE, status = "success",
      collapsible = TRUE, collapsed = TRUE,

      downloadButton('downloadData', 'Download'),
      p(),
      DT::dataTableOutput("table"),

      p(),
      p(span(strong("white")), " = work; ", span(strong("blue")), " = early retirement via taxable accounts; ", span(strong("light green")), " = early retirement via from roth ladder; ", span(strong("green")), " = regular retirement via retirement accounts")
    ), 
    span(p("Content contained or made available through the app is not intended to and does not constitute financial or investment advice. No one should make any financial decision without first conducting his or her own research and due diligence. To the maximum extent permitted by law, JN disclaims any and all liability in the event any information, analysis, advice and/or recommendations prove to be inaccurate, incomplete or unreliable, or result in any investment or other losses. You should consult with a professional to determine what may be the best for your individual needs."), style = "font-size:12px; color:grey")
  ))
)


