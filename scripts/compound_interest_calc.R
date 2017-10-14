# Compound Interest Calculations
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu


# CALCULATIONS UTILTY FUNCTIONS
# ---------------------------------------------

# compound interest formula with annual contribution (made at start of compounding period)
formula <- function(n, principle, contribution, interest_rate){
  if(!dplyr::between(interest_rate, 0, 1)) stop("Interest rate must be between 0 and 1")

  r <- 1 + interest_rate
  val <- principle * r ^ (n) + contribution * r * (1 - r ^ n) / (1 - r)
  return(val)
}


# compute the total, principle, and interest; convert all neg values to 0
apply_calculations <- function(n, principle, contribution, interest_rate, label){

  tot <- formula(n, principle, contribution, interest_rate)
  interest <- diff(c(principle, tot)) - contribution
  principle <- tot - interest

  out <- data.frame(principle = principle, interest = interest, total = tot) %>%
    mutate_all(~ ifelse(.x < 0, 0, .x))
  colnames(out) <- paste(label, colnames(out), sep = "_")
  return(out)
}


# accumulation years: start contributions on start_age year
accumulate <- function(
  start_age, retire_age,
  tax_starting_principle, nontax_starting_principle,
  tax_yearly_add, nontax_yearly_add,
  growth_rate
){

  age <- start_age:(retire_age - 1)
  index <- 1:length(age)

  tax_accounts <- apply_calculations(
    n = index,
    principle = tax_starting_principle, contribution = tax_yearly_add,
    interest_rate = growth_rate, label = "tax"
  )
  nontax_accounts <- apply_calculations(
    n = index,
    principle = nontax_starting_principle, contribution = nontax_yearly_add,
    interest_rate = growth_rate, label = "nontax"
  )

  # balances up until retirement
  dat <- bind_cols(age = age, tax_accounts, nontax_accounts)
  return(dat)
}


# early retirement years: start withdrawals on retire_age; from tax account only
early_retire <- function(
  in_dat,
  retire_age, age_access_nontax,
  yearly_spend, growth_rate
){

  age <- retire_age:(age_access_nontax - 1)
  index <- 1:length(age)
  bal_before_retire <- tail(in_dat, 1)

  tax_accounts <- apply_calculations(
    n = index,
    principle = bal_before_retire$tax_total, contribution = -yearly_spend,
    interest_rate = growth_rate, label = "tax"
  )

  nontax_accounts <- apply_calculations(
    n = index,
    principle = bal_before_retire$nontax_total, contribution = 0,
    interest_rate = growth_rate, label = "nontax"
  )

  dat <- bind_cols(age = age, tax_accounts, nontax_accounts)
  return(dat)
}


# check early retirement years;
# need roth conversion? check for roth conversion only after you have retired (cuz taxes)
# went broke in tax account? went broke flag has a 1 year of spend buffer
early_retirement_checks <- function(in_dat1, in_dat2, yearly_spend, access_nontax_age){

  # check for roth conversion
  roth_check <- tail(subset(in_dat2, tax_total >= yearly_spend * 6), 1)
  roth_age <- roth_check$age + 5
  new_access_age <- ifelse(access_nontax_age > roth_age, roth_age, access_nontax_age)
  pre_nontax_dat <- in_dat2

  # check for premature switch to nontax accounts
  went_broke <- any(tail(pre_nontax_dat, 1)$tax_total < yearly_spend, nrow(roth_check) == 0)
  if( went_broke ){
    if( nrow(roth_check) == 0 ){
      pre_nontax_dat <- in_dat1
      new_access_age <- tail(in_dat1, 1)$age + 1
    } else{
      pre_nontax_dat <- subset(pre_nontax_dat, tax_total >= yearly_spend)
      new_access_age <- tail(pre_nontax_dat, 1)$age + 1
    }
  }

  out <- list(dat = pre_nontax_dat, access_age = new_access_age, went_broke = went_broke)
  return(out)
}


# regular retirement years: start withdrawals at roth age; from nontax account only, transfer tax account into nontax account
regular_retire <- function(
  in_dat,
  access_age, yearly_spend,
  growth_rate
){

  age <- access_age:100
  index <- 1:length(age)
  bal_before_retire <- tail(in_dat, 1)
  principle_after_transfer <- bal_before_retire$tax_total + bal_before_retire$nontax_total

  nontax_accounts <- apply_calculations(
    n = index,
    principle = principle_after_transfer, contribution = -yearly_spend,
    interest_rate = growth_rate, label = "nontax"
  )

  dat <- bind_cols(age = age, nontax_accounts)
  return(dat)
}



# RETIREMENT DATA CALCULATIONS
# ---------------------------------------------

# function to calculate retirement
retire <- function(
  retire_age, yearly_spend, tax_starting_principle, nontax_starting_principle,
  start_age = 25, access_nontax_age = 60,
  growth_rate = 0.05, tax_yearly_add = 0, nontax_yearly_add = 0
){


  # working years - build portfolio
  accumulate_data <- accumulate(
    start_age = start_age,
    retire_age = retire_age,
    tax_starting_principle = tax_starting_principle,
    nontax_starting_principle = nontax_starting_principle,
    tax_yearly_add = tax_yearly_add,
    nontax_yearly_add = nontax_yearly_add,
    growth_rate = growth_rate
  )

  # early retirement years - withdraw from tax account
  early_retire_data <- early_retire(
    in_dat = accumulate_data,
    retire_age = retire_age,
    age_access_nontax = access_nontax_age,
    yearly_spend = yearly_spend,
    growth_rate = growth_rate
  )

  # check tax account for whether you need a roth conversion
  early_retire_fix <- early_retirement_checks(
    in_dat1 = accumulate_data,
    in_dat2 = early_retire_data,
    yearly_spend = yearly_spend,
    access_nontax_age = access_nontax_age
  )
  early_retire_data <- early_retire_fix$dat
  access_age <- early_retire_fix$access_age
  went_broke_tax <- early_retire_fix$went_broke

  # regular retirement years - withdraw from nontax account
  regular_retire_data <- regular_retire(
    in_dat = early_retire_data,
    access_age = ifelse(went_broke_tax, access_age, access_nontax_age),
    yearly_spend = yearly_spend,
    growth_rate = growth_rate
  )

  # combine datasets; convert negative numbers and NAs to 0
  if( identical(accumulate_data, early_retire_data) ){
    combined <- bind_rows(accumulate_data, regular_retire_data)
  } else{
    combined <- bind_rows(accumulate_data, early_retire_data, regular_retire_data)
  }
  combined_data <- combined %>%
    mutate_all(~ ifelse(is.na(.x), 0, .x)) %>%
    mutate(net_worth = tax_total + nontax_total) %>%
    mutate(year = 1:nrow(.)) %>%
    dplyr::select(year, everything())

  # check on retirement money
  went_broke_check <- head(subset(combined_data, nontax_total == 0))
  went_broke_nontax <- nrow(went_broke_check) > 0
  went_broke_age <- max(went_broke_check$age, access_age)

  # process tax amount
  went_broke_tax <- ifelse(went_broke_tax, str_interp("Warning: ran out of money at ${access_age}; must access retirement accounts early<br/>"), "")
  went_broke_nontax <- ifelse(went_broke_nontax, str_interp("Warning: ran out of money at ${went_broke_age}; consider working longer or saving more<br/>"), "")

  # formatting
  d <- mutate_at(combined_data, vars(-age), ~ round(.x, 2))
  out <- list(
    data = d,
    nontax_access = access_age,
    changed_year_nontax = access_age < access_nontax_age,
    went_broke_tax = went_broke_tax,
    went_broke_nontax = went_broke_nontax
  )

  # return data
  return(out)
}
