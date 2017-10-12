# Utility functions
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

make_error_msg <- function(condition, msg) ifelse(condition, msg, "")

check_throw_error <- function(checks) unlist(lapply(checks, function(x) x != ""))

process_summary_data <- function(l, age, retire_early_age){

  d <- l$data
  d2 <- subset(d, age == (retire_early_age - 1))
  year_access_nontax <- l$nontax_access

  # process data for Roth Ladder
  roth_ladder <- ifelse(l$changed_year_nontax, paste("Age Start Roth Ladder:", max(age[year_access_nontax] - 5, retire_early_age + 1), "<br/>"), "")

  # obtain text
  HTML(paste(
    "Years Working:", retire_early_age - age[1], "<br/>",
    "Assets in Taxable Account at RE ($):", d2$tax_total, "<br/>",
    "Assets in Retirement Account at RE ($):", d2$nontax_total, "<br/>",
    "Assets Total at RE ($):", d2$net_worth, "<br/>",
    "<br/>",
    "Age Retire:", retire_early_age, "<br/>",
    roth_ladder,
    "Age Access Retirement Accounts:", age[year_access_nontax], "<br/>",
    l$went_broke_tax, l$went_broke_nontax,
    "<br/>",
    "Assets Total at 100 ($):", d$net_worth[length(age)], "<br/>"
  ))
}

make_interest_plot <- function(d){

  d %>%
    subset(str_detect(variable, "interest")) %>%
    mutate(variable = factor(ifelse(variable == "tax_interest", "taxable", "nontaxable"), levels = c("taxable", "nontaxable"))) %>%
    ggplot(aes(age, value)) +
    geom_line(size = 1.1) +
    geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "red") +
    facet_grid(~ variable) +
    labs(x = "Age", y = "Interest Earned Per Year ($)", title = "Interested Earned Per Year by Age")

}

make_total_plot <- function(d){

  d %>%
    subset(str_detect(variable, "total")) %>%
    mutate(variable = factor(ifelse(variable == "tax_total", "taxable", "nontaxable"), levels = c("taxable", "nontaxable"))) %>%
    ggplot(aes(age, value)) +
    geom_line(size = 1.1) +
    geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "red") +
    facet_grid(~ variable) +
    labs(x = "Age", y = "Total ($)", title = "Account Assets by Age")

}
