# Compound Interest Calculations
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu


# CALCULATIONS UTILTY FUNCTIONS
# compound interest formula with annual contribution (made at start of compounding period)
# - if no interest rate: just add up cumulative contributions
# - if constant contributions: use compound interest formula
# - if vary contributions and interest rate: manually calculate
formula <- function(n, principle, contributions, interest_rate){
  if(!dplyr::between(interest_rate, 0, 1)) stop("Interest rate must be between 0 and 1")
  
  r <- 1 + interest_rate
  if(interest_rate == 0){
    val <- principle * r ^ (n) + cumsum(contributions)
  } else if(length(unique(contributions)) == 1){
    val <- principle * r ^ (n) + contributions * r * (1 - r ^ n) / (1 - r)
  } else{
    val <- c()
    tot <- principle
    for(ct in contributions){
      tot <- (tot + ct) * r
      val <- c(val, tot)
    }
  }
  val
}


# compute the total, principle, and interest; convert all neg values to 0
apply_calculations <- function(n, principle, contribution, savings_increase = 0, interest_rate, label){
  
  contributions <- c(contribution, formula(n, contribution, 0, savings_increase))[n]
  tot <- formula(n, principle, contributions, interest_rate)
  principle <- c(principle, tot)[n] 
  interest <- tot - principle - contributions
  
  out <- data.frame(principle = principle, contribution = contributions, interest = interest, total = tot) %>%
    dplyr::mutate_all(~ ifelse(.x < 0, 0, .x)) %>% 
    purrr::set_names(paste(label, colnames(.), sep = "_"))
  out
}


# accumulation years: start contributions on start_age year
accumulate <- function(
  start_age, retire_age,
  tax_starting_principle, tax_defer_starting_principle, tax_exempt_starting_principle, tax_exempt_starting_contributions,
  tax_yearly_add, tax_defer_yearly_add, tax_exempt_yearly_add,
  savings_increase, 
  growth_rate
){
  
  age <- start_age:(retire_age - 1)
  index <- 1:length(age)
  
  tax_accounts <- apply_calculations(
    n = index,
    principle = tax_starting_principle, contribution = tax_yearly_add,
    savings_increase = savings_increase, interest_rate = growth_rate, label = "tax"
  )
  tax_defer_accounts <- apply_calculations(
    n = index,
    principle = tax_defer_starting_principle, contribution = tax_defer_yearly_add,
    interest_rate = growth_rate, label = "tax_defer"
  )
  tax_exempt_accounts <- apply_calculations(
    n = index,
    principle = tax_exempt_starting_principle, contribution = tax_exempt_yearly_add,
    interest_rate = growth_rate, label = "tax_exempt"
  ) %>% 
    dplyr::mutate(tax_exempt_contribution_cumul = tax_exempt_starting_contributions + cumsum(tax_exempt_contribution)) %>% 
    dplyr::select(-tax_exempt_total, dplyr::everything())
  
  # balances up until retirement
  dplyr::bind_cols(age = age, tax_accounts, tax_defer_accounts, tax_exempt_accounts)
}

# withdraw helpers
adjust_cum_diff <- function(dat, bal_begin, extra = numeric(0)){
  diffs <- dat %>% 
    dplyr::rename(principle = dplyr::matches("principle"), total = dplyr::matches("total"), interest = dplyr::matches("interest")) %>% 
    dplyr::mutate(dff = -(principle + interest - total)) %>% 
    dplyr::pull(dff)
  bal_updated <- cumsum(c(bal_begin, diffs, extra))[-1]
  
  dplyr::mutate(dat, tax_exempt_contribution_cumul = bal_updated)
}
get_instructions <- function(dat, variable, leftover = NULL){
  list(
    index = if(nrow(dat) > 0) 1:nrow(dat) else numeric(0),
    less_partial_withdraw = if(!is.null(leftover)) leftover else tail(dat[,variable], 1)
  )
}
withdraw_partial_immediately <- function(instructions, dat, variable, label, yearly_spend, growth_rate){
  apply_calculations(
    n = 1,
    principle = dat[,variable], contribution = -(yearly_spend - instructions$less_partial_withdraw),
    interest_rate = growth_rate, label = label
  )
}
withdraw_partial_after_wait <- function(instructions, dat, variable, label, yearly_spend, growth_rate){
  accounts1 <- apply_calculations(
    n = instructions$index, 
    principle = dat[,variable] %>% tail(1), contribution = 0,
    interest_rate = growth_rate, label = label
  ) %>% head(-1) # last row is the withdrawal from previous
  accounts2 <- apply_calculations(
    n = 1,
    principle = accounts1[,variable] %>% tail(1), contribution = -(yearly_spend - instructions$less_partial_withdraw),
    interest_rate = growth_rate, label = label
  )
  dplyr::bind_rows(accounts1, accounts2)
}
get_index_left <- function(original, chill){
  diff <- setdiff(original, chill)
  index_left <- if(length(diff) > 0) 1:length(diff) else numeric(0)
  index_left
}

# withdraw from tax exempt accounts
withdraw_tax_exempt_contrib_less_bal <- function(index, bal_before_retire, yearly_spend, growth_rate){
  
  accounts <- apply_calculations(
    n = index,
    principle = bal_before_retire$tax_exempt_total, contribution = -yearly_spend,
    interest_rate = growth_rate, label = "tax_exempt"
  ) %>% 
    dplyr::filter(tax_exempt_principle > 0) %>% 
    adjust_cum_diff(bal_begin = bal_before_retire$tax_exempt_contribution_cumul) %>% 
    dplyr::select(-tax_exempt_total, dplyr::everything())
  
  # instructions for next 
  next_instructions <- get_instructions(dat = accounts, variable = "tax_exempt_principle")
  
  list(d = accounts, instructions = next_instructions)
}
withdraw_tax_exempt_contrib_more_bal <- function(index, bal_before_retire, yearly_spend, growth_rate){
  
  # contributions only
  accounts_contrib <- apply_calculations(
    n = index,
    principle = bal_before_retire$tax_exempt_contribution_cumul, contribution = -yearly_spend,
    interest_rate = 0, label = "tax_exempt_contrib"
  ) 
  accounts_contrib_withdraw <- accounts_contrib %>% 
    dplyr::filter(tax_exempt_contrib_principle > 0)
  
  # instructions for withdraw
  current_leftover <- tail(accounts_contrib_withdraw$tax_exempt_contrib_principle, 1) 
  current_index_withdraw <- 1:nrow(accounts_contrib_withdraw)
  
  # withdraw
  if( nrow(accounts_contrib_withdraw) == 0 ){ # sit tight
    accounts <- apply_calculations(
      n = index,
      principle = bal_before_retire$tax_exempt_total, contribution = 0,
      interest_rate = growth_rate, label = "tax_exempt"
    ) %>% head(0)
    
  } else if( nrow(accounts_contrib_withdraw) == 1 ){ # withdraw partial immediately
    accounts <- apply_calculations(
      n = current_index_withdraw,
      principle = bal_before_retire$tax_exempt_total, contribution = -current_leftover,
      interest_rate = growth_rate, label = "tax_exempt"
    )
    
  } else if( current_leftover >= yearly_spend ){ # withdraw full immediately
    accounts <- apply_calculations(
      n = current_index_withdraw, 
      principle = bal_before_retire$tax_exempt_total, contribution = -yearly_spend,
      interest_rate = growth_rate, label = "tax_exempt"
    )
    
  } else{ # withdraw full then partial at the end
    accounts1 <- apply_calculations(
      n = current_index_withdraw, 
      principle = bal_before_retire$tax_exempt_total, contribution = -yearly_spend,
      interest_rate = growth_rate, label = "tax_exempt"
    ) %>% head(-1) # remove tail
    accounts2 <- apply_calculations(
      n = 1, 
      principle = accounts1$tax_exempt_total %>% tail(1), contribution = -current_leftover,
      interest_rate = growth_rate, label = "tax_exempt"
    )
    accounts <- dplyr::bind_rows(accounts1, accounts2)
  }
  accounts <- accounts %>% 
    dplyr::mutate(tax_exempt_contribution_cumul = accounts_contrib_withdraw$tax_exempt_contrib_total) %>% 
    dplyr::select(-tax_exempt_total, dplyr::everything())
  
  # instructions for next
  next_instructions <- get_instructions(dat = accounts_contrib_withdraw, variable = "tax_exempt_contrib_principle", leftover = current_leftover)
  
  list(d = accounts,instructions = next_instructions)
}
withdraw_tax_exempt <- function(index, bal_before_retire, yearly_spend, growth_rate){
  min_is_contrib <- with(bal_before_retire, tax_exempt_contribution_cumul < tax_exempt_total)
  if(!min_is_contrib){
    withdraw_tax_exempt_contrib_less_bal(index, bal_before_retire, yearly_spend, growth_rate)
  } else{
    withdraw_tax_exempt_contrib_more_bal(index, bal_before_retire, yearly_spend, growth_rate)
  }
}

# withdraw from accounts
withdraw_generic <- function(
  index, instructions, 
  bal_before_retire, 
  yearly_spend, growth_rate,
  label
){
  variable_total <- paste0(label, "_total")
  variable_principle <- paste0(label, "_principle")
  
  # withdraw
  accounts <- if( length(instructions$index) == 0 ){
    apply_calculations(
      n = index,
      principle = bal_before_retire[,variable_total], contribution = -yearly_spend,
      interest_rate = growth_rate, label = label
    )
    
    # 
  } else if( (length(index) == length(instructions$index)) & instructions$less_partial_withdraw >= yearly_spend ){
    apply_calculations(
      n = index, 
      principle = bal_before_retire[,variable_total], contribution = 0, 
      interest_rate = growth_rate, label = label
    )
    
    # use info from previous
  } else{
    accounts1 <- if( length(instructions$index) == 1 ){ # withdraw partial immediately
      withdraw_partial_immediately(
        instructions, 
        bal_before_retire, variable = variable_total, label = label, 
        yearly_spend = yearly_spend, growth_rate = growth_rate
      )
    } else if( length(instructions$index) > 1 ){ # withdraw partial after wait
      withdraw_partial_after_wait(
        instructions, 
        bal_before_retire, variable = variable_total, label = label, 
        yearly_spend = yearly_spend, growth_rate = growth_rate
      )
    }
    accounts2 <- apply_calculations(
      n = get_index_left(original = index, chill = instructions$index),
      principle = accounts1[,variable_total] %>% tail(1), contribution = -yearly_spend,
      interest_rate = growth_rate, label = label
    )
    dplyr::bind_rows(accounts1, accounts2)
  }
  
  #  instructions for next
  skipped_prev_withdrawal_no_data <- bal_before_retire[,variable_total] == 0
  full_prev_withdrawal <- ifelse(length(index) == length(instructions$index), instructions$less_partial_withdraw >= yearly_spend, FALSE)
  if( skipped_prev_withdrawal_no_data | full_prev_withdrawal ){ # no calculations needed, sit tight
    next_instructions <- instructions
  } else{ # some calculations & pass on
    accounts <- accounts %>% 
      dplyr::rename(principle = dplyr::matches("principle")) %>% 
      dplyr::filter(principle > 0) %>% 
      purrr::set_names(plyr::mapvalues(colnames(.), "principle", variable_principle))
    next_instructions <- get_instructions(dat = accounts, variable = variable_principle)
  } 
  
  list(d = accounts,instructions = next_instructions)
}

# fill any empty rows
fill_data <- function(index, dat, bal_before_retire, label, yearly_spend, growth_rate){
  d_tail <- if( nrow(dat) > 0 ){
    new_index <- get_index_left(index, 1:nrow(dat))
    withdraw_generic(
      index = new_index, 
      instructions = list(index = new_index, less_partial_withdraw = yearly_spend),
      bal_before_retire = tail(dat, 1), 
      yearly_spend = yearly_spend, 
      growth_rate = growth_rate, 
      label = label
    )$d
    
  } else{
    bal_before_retire
    apply_calculations(
      n = index, 
      principle = bal_before_retire[,paste0(label, "_total")], contribution = 0, 
      interest_rate = growth_rate, label = label
    )
  }
  
  dplyr::bind_rows(dat, d_tail)
}

# need use of roth ladder & when
check_roth_ladder <- function(a, b, index, yearly_spend, age){
  # roth ladder check
  use_final_instructions <- if( length(a$index) == length(b$index) ){
    if(length(a$index) == 0){
      a
    } else if( a$less_partial_withdraw >= yearly_spend) a else b
  } else if( length(a$index) > length(b$index) ){
    a
  } else{
    b
  }
  
  if(length(use_final_instructions$index) > 0){
    full_time_span <- length(use_final_instructions$index) == length(index)
    extra_money_at_end <- use_final_instructions$less_partial_withdraw >= yearly_spend
    need_roth_ladder <- ifelse(full_time_span & extra_money_at_end, FALSE, TRUE)
    roth_age <- if(need_roth_ladder){
      tail(age[use_final_instructions$index], 1)
    } else NA
    
  } else{
    need_roth_ladder <- TRUE
    roth_age <- age[1]
  }
  
  list(need_roth_ladder = need_roth_ladder, age = roth_age)
}


# early retirement years: starts on retire_age; goes through iteration roth -> tax -> tira
# note: there is a bug if must pull from all 3 accounts in one year (ie roth contrib & tax deplete in same year); will withdraw more than yearly spend
early_retire <- function(
  in_dat,
  retire_age, access_nontax_age,
  yearly_spend, growth_rate
){
  
  age <- retire_age:(access_nontax_age - 1)
  index <- 1:length(age)
  bal_before_retire <- tail(in_dat, 1)
  
  # roth
  tax_exempt_calc <- withdraw_tax_exempt(
    index = index, 
    bal_before_retire = bal_before_retire, 
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate
  )
  tax_exempt_accounts <- fill_data(
    index = index, 
    dat = tax_exempt_calc$d, 
    bal_before_retire = bal_before_retire,
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate, 
    label = "tax_exempt"
  ) %>% 
    dplyr::mutate(tax_exempt_contribution_cumul = ifelse(is.na(tax_exempt_contribution_cumul), min(tax_exempt_contribution_cumul, na.rm = TRUE), tax_exempt_contribution_cumul))
  
  # tax
  tax_calc <- withdraw_generic(
    index = index, 
    instructions = tax_exempt_calc$instructions,
    bal_before_retire = bal_before_retire, 
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate,
    label = "tax"
  )
  tax_accounts <- fill_data(
    index = index, 
    dat = tax_calc$d, 
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate, 
    label = "tax"
  )
  
  # tira
  tax_defer_calc <- withdraw_generic(
    index = index, 
    instructions = tax_calc$instructions,
    bal_before_retire = bal_before_retire, 
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate,
    label = "tax_defer"
  )
  tax_defer_accounts <- fill_data(
    index = index, 
    dat = tax_defer_calc$d, 
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate, 
    label = "tax_defer"
  )

  # combine
  out <- dplyr::bind_cols(age = age, tax_accounts, tax_defer_accounts, tax_exempt_accounts)
  
  # check for use of roth ladder
  roth_ladder <- check_roth_ladder(
    a = tax_exempt_calc$instructions, 
    b = tax_calc$instructions, 
    index = index,
    yearly_spend = yearly_spend, 
    age = age
  )
  
  # check for need to tap roth gains prematurely
  roth_gains <- if( roth_ladder$need_roth_ladder & tail(out, 1)$tax_defer_total == 0 ){
    age_from_tax_defer <- out %>% 
      dplyr::filter(tax_defer_total == 0) %>% 
      dplyr::slice(1) %>% 
      dplyr::pull(age)
    
    list(
      need_roth_gains = TRUE,
      age = max(roth_ladder$age, age_from_tax_defer)
    )
  } else{
    list(
      need_roth_gains = FALSE,
      age = NA
    )
  }
  
  # update early retire data if the last row requires tapping roth, remove it (will get combined in next step)
  d_update <- if(roth_gains$need_roth_gains) dplyr::filter(out, age < roth_gains$age) else out
  list(d = d_update, roth_ladder = roth_ladder, roth_gains = roth_gains)
}


# regular retirement years: combine everything into tax exempt and withdraw
regular_retire <- function(
  in_dat, roth_gains,
  yearly_spend, growth_rate
){
  
  # go up to when need to extract roth gains
  bal_before_retire <- in_dat
  age <- (bal_before_retire$age + 1): 100
  index <- 1:length(age)
  
  # combine into 1 account and withdraw
  tax_exempt_accounts <- apply_calculations(
    n = index, 
    principle = with(bal_before_retire, tax_total + tax_defer_total + tax_exempt_total),
    contribution = -yearly_spend, 
    interest_rate = growth_rate, label = "tax_exempt"
  )
  tax_accounts <- apply_calculations( 
    n = index,
    principle = 0, contribution = 0,
    interest_rate = growth_rate, label = "tax"
  )
  tax_defer_accounts <- apply_calculations( 
    n = index,
    principle = 0, contribution = 0,
    interest_rate = growth_rate, label = "tax_defer"
  )
  
  dplyr::bind_cols(age = age, tax_accounts, tax_defer_accounts, tax_exempt_accounts)
}

# combine data sets across stages; convert stranges #s to 0
combine_retire_data <- function(accumulate_data, early_retire_data, regular_retire_data){
  combined <- bind_rows(accumulate_data, early_retire_data, regular_retire_data)
  combined_data <- combined %>%
    dplyr::mutate_at(vars(-age), ~ round(.x, 2)) %>% 
    dplyr::mutate_all(~ ifelse(is.na(.x) | is.infinite(.x) | .x < 0, 0, .x)) %>%
    dplyr::mutate(net_worth = tax_total + tax_defer_total + tax_exempt_total) %>%
    dplyr::mutate(year = 1:nrow(.)) %>%
    dplyr::select(year, everything())
  
  combined_data
}



# RETIREMENT DATA CALCULATIONS
retire <- function(
  start_age, retire_age, access_nontax_age = official_nontax_access,
  tax_starting_principle, tax_defer_starting_principle, tax_exempt_starting_principle, tax_exempt_starting_contributions,
  tax_yearly_add, tax_defer_yearly_add, tax_exempt_yearly_add, 
  savings_increase, growth_rate, yearly_spend
){
  
  # working years - build portfolio
  accumulate_data <- accumulate(
    start_age = start_age,
    retire_age = retire_age,
    tax_starting_principle = tax_starting_principle, 
    tax_defer_starting_principle = tax_defer_starting_principle, 
    tax_exempt_starting_principle = tax_exempt_starting_principle,
    tax_exempt_starting_contributions = tax_exempt_starting_contributions,
    tax_yearly_add = tax_yearly_add, 
    tax_defer_yearly_add = tax_defer_yearly_add, 
    tax_exempt_yearly_add = tax_exempt_yearly_add,
    savings_increase = savings_increase, 
    growth_rate = growth_rate 
  )
  
  # early retirement years - withdraw from roth, then tax
  early_retire_calc <- early_retire(
    in_dat = accumulate_data,
    retire_age = retire_age,
    access_nontax_age = access_nontax_age,
    yearly_spend = yearly_spend,
    growth_rate = growth_rate
  )
  early_retire_data <- early_retire_calc$d
  roth_ladder <- early_retire_calc$roth_ladder
  roth_gains <- early_retire_calc$roth_gains
  
  # regular retirement years - combine into 1 and withdraw
  next_dat <- if(nrow(early_retire_data) > 0) tail(early_retire_data, 1) else tail(accumulate_data, 1)
  regular_retire_data <- regular_retire(
    in_dat = next_dat,
    roth_gains = roth_gains,
    yearly_spend = yearly_spend, 
    growth_rate = growth_rate
  )
  
  # combine datasets
  combined_data <- combine_retire_data(accumulate_data, early_retire_data, regular_retire_data)
  
  # messages
  went_broke_age <- head(dplyr::filter(combined_data, net_worth == 0)$age, 1)
  went_broke <- list(
    went_broke = length(went_broke_age) > 0, 
    age = if(length(went_broke_age) == 0) NA else went_broke_age 
  )
  msg1 <- ifelse(roth_ladder$need_roth_ladder, glue::glue("Warning: require access to Roth ladder funds at {age}; setup at {age2}<br/>", age = roth_ladder$age, age2 = roth_ladder$age - 5), "")
  msg2 <- ifelse(roth_gains$need_roth_gains, glue::glue("Warning: ran out of money at {age}; must access retirement accounts prematurely, expect additional fees<br/>", age = roth_gains$age),"")
  msg3 <- ifelse(went_broke$went_broke, glue::glue("Warning: ran out of money at {went_broke_age}; consider working longer or saving more<br/>"), "")
  
  # output
  out <- list(
    data = combined_data,
    retire_age = retire_age,
    roth_access = roth_ladder$age,
    retire_access = min(roth_gains$age, access_nontax_age, na.rm = TRUE),
    broke = went_broke$age,
    roth_msg = msg1, 
    penalty_msg = msg2, 
    broke_msg = msg3
  )
  out
}
