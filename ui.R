# UI Code for Calculator
# Date: July 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shiny)
library(shinydashboard)


# setup sidebar
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Calculator", tabName = "main", icon = icon("calculator")),
    menuItem("Information", tabName = "information", icon = icon("info-circle")), 
    menuItem("Github Source Code", href = "https://github.com/jennguyen1/retirement_calculator", icon = icon("github"))
  )
)

# information
information <- tabItem(
  tabName = "information",
  verticalLayout( 
    box(width = NULL,
      p("This is a R Shiny application that computes rough projections for retirement savings."),
      p("The retirement account balances are initialized with the amounts in the ", span(strong("Initial Principle")), " fields. Contributions (", span(strong("Amount Added")), ") start on the same year as the ", span(strong("Start Age")), ". There are three account types: taxable (no early withdrawal restrictions), tax-deferred (such as traditional IRA with early withdrawal restrictions), and tax-exempt (such as Roth IRA with no early withdrawal restrictions on contributions only)."),
      p("During the work years, the ", span(strong("Amount Added")), " value is added at the beginning of the year and compounded at the end of the year. It increases at a rate of ", span(strong("Percent Increase in Savings")), " per year and applied to the taxable account only. Interest is calculated at a rate of ", span(strong("Growth Rate")), ". We recommend using an interest rate that has been adjusted for inflation."),
      p("After the ", span(strong("Retire Age")), ", the ", span(strong("Amount Added")), " value is reduced to 0 and the accounts continue to earn interest at the specified rate. In addition, the ", span(strong("Spending per Year")), " amount is deducted from the total. The ", span(strong("Spending per Year")), " is withdrawn first from the total cumulative tax-exempt contributions, followed by the taxable accounts until it is critically low. Withdrawals then continue in the tax-deferred and tax-exempt accounts. The age for withdrawal from retirement accounts without penalty is 60 (rounded up from 59.5). If it is likely that the taxable accounts are depleted before then, there is the option to apply the Roth Ladder Conversion."),
      p("The program projects balances until age 100. A summary of results is printed in the ", span(strong("Summary of Results")), " tab. A detailed table is printed and available for download in the ", span(strong("Detailed Results")), "tab.")
    )
  )
)


# UI functions setup
function(request){
dashboardPage(skin = "green",
  dashboardHeader(title = "Retirement Calculator"),
  sidebar, 
  dashboardBody(
    tabItems(
      information, 
      tabItem(
        tabName = "main",
        
        verticalLayout(
          box(
            title = "Set Input Parameters", width = NULL,
            solidHeader = TRUE, status = "success", collapsible = TRUE,
            
            fluidRow(
              column(width = 6, numericInput("start_age", "Current Age", 25, min = 16, max = 99, step = 1)),
              column(width = 6, numericInput("retire_age", "Retire Age", 55, min = 18, max = 100, step = 1))
            ),
            fluidRow(
              column(width = 6, numericInput("yearly_spend", "Spending Per Year", 45000, min = 0, step = 1000)),
              column(width = 6, numericInput("growth_rate", "Growth Rate", 0.05, min = 0, max = 0.5, step = 0.01))
            ),
            fluidRow(
              column(width = 6, numericInput("tax_starting_principle", "Initial Principle in Taxable Accounts", 2000, min = 0, step = 1000)),
              column(width = 6, numericInput("tax_yearly_add", "Amount Added to Taxable Account Yearly", 7000, min = 0, step = 1000))
            ),
            fluidRow(
              column(width = 6, numericInput("tax_defer_starting_principle", "Initial Principle in Tax-Deferred Accounts", 10000, min = 0, step = 1000)),
              column(width = 6, numericInput("tax_defer_yearly_add", "Amount Added to Tax-Defered Account Yearly", 11500, min = 0, step = 1000))
            ),
            fluidRow(
              column(width = 6, numericInput("tax_exempt_starting_principle", "Initial Principle in Tax-Exempt Accounts", 10000, min = 0, step = 1000)),
              column(width = 6, numericInput("tax_exempt_yearly_add", "Amount Added to Tax-Exempt Account Yearly", 11500, min = 0, step = 1000))
            ),
            fluidRow(
              column(width = 6, numericInput("tax_exempt_starting_contributions", "Cumulative Contributions Balance in Tax-Exempt Accounts", 10000, min = 0, step = 1000)),
              column(width = 6, numericInput("savings_increase", "Percent Increase in Savings", 0.01, min = 0.0, max = 0.5, step = 0.01))
            ),
            actionButton("submit", "Submit"), bookmarkButton()
          ),
          
          # outputs
          uiOutput("summary_box"),
          uiOutput("detail_box")
          
        )
      )
    ),
    span(p("Content contained or made available through the app is provided for informational purposes only and does not constitute financial, tax, or legal advice. No one should make any financial decisions without first conducting his or her own research and due diligence. To the maximum extent permitted by law, JN disclaims any and all liability in the event any information and/or analysis prove to be inaccurate, incomplete, unreliable, or result in any investment or other losses. You should consult with a professional to determine what may be the best for your individual needs."), style = "font-size:12px; color:grey"),
    span(p("Copyright (c) 2021 Jennifer N Nguyen under the MIT License"), style = "font-size:12px; color:grey")
  )
)}


