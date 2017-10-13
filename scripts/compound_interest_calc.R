# Compound Interest Calculations
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu


# pre-allocate vector for calculations
initiate <- function(age) numeric(length(age))

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
  tax_principle <- initiate(age)
  tax_interest <- initiate(age)
  tax_total <- initiate(age)
  nontax_principle <- initiate(age)
  nontax_interest <- initiate(age)
  nontax_total <- initiate(age)

  # set account principles
  tax_principle[1] <- tax_starting_principle
  nontax_principle[1] <- nontax_starting_principle

  # set variable for changing withdrawals from accounts
  changed_year_nontax <- FALSE

  # variable if ran out of money
  went_broke_tax <- FALSE
  went_broke_nontax <- FALSE
  went_broke_tax_age <- 0
  went_broke_nontax_age <- 0

  # calculate account values
  for(year in years_from_25){

    #####################################
    # RESET NONTAX ACCESS - ROTH LADDER #
    #####################################
    if(year > year_retire_early & !changed_year_nontax){ # check if have retired and ran out of money
      if(tax_total[year - 1] < yearly_spend * 6){ # previous year's taxable account total isn't enough until access nontas
        if(year_access_nontax > (year + 5)){ # adjust if there is need for the roth ladder hack

          year_access_nontax <- year + 5
          changed_year_nontax <- TRUE

        }
      }
    }

    ##################
    # REGULAR RETIRE #
    ##################
    if(year == year_access_nontax){

      # taxable account
      tax_principle[year] <- 0
      tax_interest[year] <- 0
      tax_total[year] <- 0

      # nontaxable accounts
      nontax_principle[year] <- tax_total[year - 1] + nontax_total[year - 1]
      nontax_interest[year] <- nontax_principle[year] * growth_rate
      nontax_total[year] <- max(nontax_principle[year] + nontax_interest[year] - yearly_spend, 0)

      # went broke: run out of money in nontax as well
      if(!went_broke_nontax & nontax_total[year] == 0){
        went_broke_nontax <- TRUE
        went_broke_nontax_age <- age[year]
      }

    } else if(year > year_access_nontax){

      # nontaxable accounts
      nontax_principle[year] <- nontax_total[year - 1]
      nontax_interest[year] <- nontax_principle[year] * growth_rate
      nontax_total[year] <- max(nontax_principle[year] + nontax_interest[year] - yearly_spend, 0)

      # went broke: run out of money in nontax as well
      if(!went_broke_nontax & nontax_total[year] == 0){
        went_broke_nontax <- TRUE
        went_broke_nontax_age <- age[year]
      }

      ################
      # EARLY RETIRE #
      ################
    } else if(year >= year_retire_early){

      # taxable account
      tax_principle[year] <- tax_total[year - 1]
      tax_interest[year] <- tax_principle[year] * growth_rate
      tax_total[year] <- max(tax_principle[year] + tax_interest[year] - yearly_spend, 0)

      # went broke: ran out of money before being able to access retirement funds
      went_broke_tax <- ifelse(tax_total[year] == 0, TRUE, FALSE)
      if(went_broke_tax){
        year_access_nontax <- year
        changed_year_nontax <- TRUE
        went_broke_tax_age <- age[year]
      }

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
      tax_total[1] <- tax_principle[1] + tax_interest[1] + tax_yearly_add

      # nontaxable accounts
      nontax_interest[1] <- 0
      nontax_total[1] <- nontax_principle[1] + nontax_interest[1] + nontax_yearly_add
    }
  }

  # process taxable amount
  went_broke_tax <- ifelse(went_broke_tax, paste("Warning: ran out of money in taxable account at", went_broke_tax_age, "; must access retirement accounts early<br/>"), "")
  went_broke_nontax <- ifelse(went_broke_nontax, paste("Warning: ran out of money in retirement accounts at", went_broke_nontax_age, "; consider working longer<br/>"), "")

  # create a dataframe for data
  d <- data.frame(year = years_from_25, age,
                  tax_principle = round(tax_principle,2), tax_interest = round(tax_interest,2), tax_total = round(tax_total,2),
                  nontax_principle = round(nontax_principle,2), nontax_interest = round(nontax_interest,2), nontax_total = round(nontax_total,2),
                  net_worth = round(tax_total + nontax_total, 2))

  # return data
  return( list(data = d, nontax_access = year_access_nontax, changed_year_nontax = changed_year_nontax, went_broke_tax = went_broke_tax, went_broke_nontax = went_broke_nontax) )
}
