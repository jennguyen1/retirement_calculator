# Utility functions
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

make_error_msg <- function(condition, msg) ifelse(condition, msg, "")

check_throw_error <- function(checks) unlist(lapply(checks, function(x) x != ""))

process_summary_data <- function(l, age, retire_early_age){

  d <- l$data
  d2 <- subset(d, age == (retire_early_age - 1))
  age_access_nontax <- l$nontax_access

  # process data for Roth Ladder
  roth_ladder <- ifelse(l$changed_year_nontax, paste("Age Start Roth Ladder:", max(age_access_nontax - 5, retire_early_age + 1), "<br/>"), "")

  # obtain text
  HTML(paste(
    "Years Working:", retire_early_age - age[1], "<br/>",
    "Assets in Taxable Account at RE ($):", formatC(d2$tax_total, format="d", big.mark=","), "<br/>",
    "Assets in Retirement Account at RE ($):", formatC(d2$nontax_total, format="d", big.mark=","), "<br/>",
    "Assets Total at RE ($):", formatC(d2$net_worth, format="d", big.mark=","), "<br/>",
    "<br/>",
    "Age Retire:", retire_early_age, "<br/>",
    roth_ladder,
    "Age Access Retirement Accounts:", age_access_nontax, "<br/>",
    l$went_broke_tax, l$went_broke_nontax,
    "<br/>",
    "Assets Total at 100 ($):", formatC(d$net_worth[length(age)], format="d", big.mark=","), "<br/>"
  ))
}

make_interest_plot <- function(d, yearly_spend){

  plot_data <- d %>%
    subset(str_detect(variable, "interest")) %>%
    mutate(variable = factor(ifelse(variable == "tax_interest", "Taxable", "Retirement"), levels = c("Taxable", "Retirement"))) %>%
    mutate(value = value / 1000)
  ymax <- ceiling(max(subset(plot_data, age <= 80)$value))

  plot_data %>%
    ggplot(aes(age, value)) +
    geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "forestgreen") +
    geom_hline(yintercept = yearly_spend / 1000, size = 1.1, linetype = "dotted") +
    geom_line(size = 1.1) +
    annotate("text", x = 90, y = yearly_spend/ 1000 + 40, label = "Yearly\nSpend") +
    facet_grid(~ variable) +
    labs(x = "Age", y = "Interest Earned Per Year ($1000)", title = "Interested Earned Per Year by Age")+
    scale_y_continuous(limits = c(0, ymax))

}

make_total_plot <- function(d){

  plot_data <- d %>%
    subset(str_detect(variable, "total")) %>%
    mutate(variable = factor(ifelse(variable == "tax_total", "Taxable", "Retirement"), levels = c("Taxable", "Retirement"))) %>%
    mutate(value = value / 1000)
    ymax <- ceiling(max(subset(plot_data, age <= 80)$value))

  plot_data %>%
    ggplot(aes(age, value)) +
    geom_hline(yintercept = 0, size = 1.1, linetype = "dashed", color = "forestgreen") +
    geom_line(size = 1.1) +
    facet_grid(~ variable) +
    labs(x = "Age", y = "Total ($1000)", title = "Account Assets by Age") +
    scale_y_continuous(limits = c(0, ymax))

}
