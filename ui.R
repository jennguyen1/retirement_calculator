
# open libraries
library(shiny)
library(shinydashboard)

# UI functions
dashboardPage(

  # app title
  dashboardHeader(title = "Retirement Calculator"),

  # disable sidebar
  dashboardSidebar(disable = TRUE),

  dashboardBody(verticalLayout(

    # inputs
    box(
      title = "Set Input Parameters", width = NULL,
      solidHeader = TRUE, status = "primary", collapsible = TRUE,

      fluidRow(
        column(width = 6, numericInput("start_age", "Current Age", 25)),
        column(width = 6, numericInput("retire_early", "Retire Age", 55, min = 35, max = 60))
      ),
      fluidRow(
        column(width = 6, numericInput("yearly_spend", "Spending Per Year", 30000, min = 0)),
        column(width = 6, numericInput("growth_rate", "Growth Rate", 0.05, min = 0, max = 0.20))
      ),
      fluidRow(
        column(width = 6, numericInput("tax_starting_principle", "Initial Principle in Taxable Accounts", 6000, min = 0)),
        column(width = 6, numericInput("nontax_starting_principle", "Initial Principle in Retirement Accounts", 11000, min = 0))
      ),
      fluidRow(
        column(width = 6, numericInput("tax_yearly_add", "Amount Added to Taxable Account Yearly", 5000, min = 0)),
        column(width = 6, numericInput("nontax_yearly_add", "Amount Added to Retirement Account Yearly", 26850, min = 0))
      )
    ),


    # outputs: summary
    box(
      title = "Summary of Results", width = NULL,
      solidHeader = TRUE, status = "primary", collapsible = TRUE,

      h4(htmlOutput("summary")),
      downloadButton('downloadData', 'Download')
    ),

    # outputs: plots
    box(
      title = "Plots", width = NULL,
      solidHeader = TRUE, status = "primary",
      collapsible = TRUE, collapsed = TRUE,

      plotOutput("totalPlot"),
      plotOutput("interestPlot")
    )
  ))
)


