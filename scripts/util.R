# Utility functions
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# global official nontax access age
official_nontax_access <- 60

# error messages
make_error_msg <- function(condition, msg) ifelse(condition, msg, "")

# check whether to throw error
check_throw_error <- function(checks) unlist(lapply(checks, function(x) x != ""))


# summary of results
process_summary_data <- function(l, age, retire_early_age, official_nontax_access_age = official_nontax_access){

  d <- l$data
  d2 <- subset(d, age == (retire_early_age - 1))
  age_access_nontax <- l$roth_access

  # process data for Roth Ladder
  roth_ladder <- ifelse(
    age_access_nontax < official_nontax_access_age,
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
    "Years Working: ${{retire_early_age - age[1] - 1}}<br/>" %>% str_interp(),
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


# make interest plot
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

# make total balances plot
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


# format table for display
format_table_for_display <- function(d){

  dat <- d %>%
    mutate_at(vars(-year, -age), ~ round(.x)) %>%
    dplyr::select(age, tax_principle, tax_interest, tax_total, nontax_principle, nontax_interest, nontax_total, net_worth)
  colnames(dat) <- c("Age", "Tax Accounts Principle", "Tax Accounts Interest", "Tax Accounts Total", "Retirement Accounts Principle", "Retirement Accounts Interest", "Retirement Accounts Total", "Net Worth")

  return(dat)
}

# make table colors
make_tab_colors <- function(input, roth_access_age, retire_access_age){


  # index 1/2/3 may be equal if
  # index 2/3 may be equal if
  milestones <- c(
    input$retire_early-1,
    roth_access_age-1,
    retire_access_age-1,
    100
  ) %>% unique()

  work_color <- "white"
  early_retire_color <- c("rgba(66, 134, 244, 0.4)", "rgba(43, 198, 48, 0.4)")
  reg_retire_color <- "rgba(43, 198, 48, 0.8)"
  milestone_colors <- c(work_color, early_retire_color[1:(length(milestones)-2)], reg_retire_color, "white")

  return(list(milestones = milestones, colors = milestone_colors))
}

# format table header
format_header <- function(){

  sketch <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Age'),
        th(colspan = 3, 'Taxable Accounts'),
        th(colspan = 3, 'Retirement Accounts'),
        th(rowspan = 2, 'Net Worth')
      ),
      tr(
        lapply(rep(c("Principle", "Interest", "Total"), 2), th)
      )
    )
  ))

  return(sketch)
}

