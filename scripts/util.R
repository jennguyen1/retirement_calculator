# Utility functions
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

make_error_msg <- function(condition, msg) ifelse(condition, msg, "")

check_throw_error <- function(checks) unlist(lapply(checks, function(x) x != ""))

process_summary_data <- function(l, age, retire_early_age, official_nontax_access_age = 60){

  d <- l$data
  d2 <- subset(d, age == (retire_early_age - 1))
  age_access_nontax <- l$nontax_access

  # process data for Roth Ladder
  roth_ladder <- ifelse(
    l$changed_year_nontax,
    paste("Age Start Roth Ladder:", max(age_access_nontax - 5, retire_early_age), "<br/>"),
    ""
  )

  age_access_description_prefix <- "Age Access Retirement Accounts"
  age_access_description <- str_interp(ifelse(
    age_access_nontax < official_nontax_access_age,
    "${age_access_description_prefix} (via Roth Ladder): ${age_access_nontax}<br/>",
    "${age_access_description_prefix}: ${age_access_nontax}<br/>"
  ))

  # variables
  tax_total <- formatC(d2$tax_total, format="d", big.mark=",")
  nontax_total <- formatC(d2$nontax_total, format="d", big.mark=",")
  networth_total <- formatC(d2$net_worth, format="d", big.mark=",")
  bal_at_100 <- formatC(d$net_worth[length(age)], format="d", big.mark=",")

  # obtain text
  HTML(paste(
    "Years Working: ${{retire_early_age - age[1]}}<br/>" %>% str_interp(),
    "Assets in Taxable Account at RE ($): ${tax_total}<br/>" %>% str_interp(),
    "Assets in Retirement Account at RE ($): ${nontax_total}<br/>" %>% str_interp(),
    "Assets Total at RE ($): ${networth_total}<br/>" %>% str_interp(),
    "<br/>",
    "Age Retire: ${retire_early_age} <br/>" %>% str_interp(),
    roth_ladder,
    age_access_description,
    l$went_broke_tax, l$went_broke_nontax,
    "<br/>",
    "Assets Total at 100 ($): ${bal_at_100}<br/>" %>% str_interp()
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
