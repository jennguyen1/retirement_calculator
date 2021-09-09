# Utility functions
# Date: Oct 2017
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# global official nontax access age
official_nontax_access <- 60


# fix integer overflow
fix_int_overflow <- function(val) ifelse(val == "NA", '> 2,147,483,647', val)


# for figs/tabs
color_key <- data.frame(
  label = c("work", "early retirement via tax-exempt contributions and/or taxable accounts", "early retirement via Roth ladder", "retirement via locked retirement accounts", "ran out of money"),
  color = c("white", "#4184F5", "#2BC630", "#2BC630", "#CF202A"), 
  alpha = c(1, .6, .4, .7, .7),
  rgb = c("rgba(255,255,255,1)", "rgba(65,132,245,0.6)", "rgba(43,198,48,0.4)", "rgba(43,198,48,0.7)", "rgba(207,32,42,0.7)")
)


# summary of results
process_summary_data <- function(l, age){
  
  d <- l$data
  d2 <- dplyr::filter(d, age == (l$retire_age - 1))
  age_access_nontax <- l$roth_access
  
  # variables
  tax_total <- d2$tax_total %>% formatC(format = "d", big.mark = ",") %>% fix_int_overflow()
  tax_defer_total <- d2$tax_defer_total %>% formatC(format = "d", big.mark = ",") %>% fix_int_overflow()
  tax_exempt_total <- d2$tax_exempt_total %>% formatC(format = "d", big.mark = ",") %>% fix_int_overflow()
  networth_total <- formatC(d2$net_worth, format="d", big.mark=",") %>% fix_int_overflow()
  bal_at_100 <- formatC(tail(d, 1)$net_worth, format="d", big.mark=",") %>% fix_int_overflow()
  
  # obtain text
  HTML(paste(
    "Years Working: {x}<br/>" %>% glue::glue(x = l$retire_age - age[1]), 
    "Assets in Taxable Account at RE ($): {tax_total}<br/>" %>% glue::glue(),
    "Assets in Tax-Deferred Account at RE ($): {tax_defer_total}<br/>" %>% glue::glue(),
    "Assets in Tax-Exempt Account at RE ($): {tax_exempt_total}<br/>" %>% glue::glue(),
    "Assets Total at RE ($): {networth_total}<br/>" %>% glue::glue(),
    "<br/>",
    l$roth_msg, 
    l$penalty_msg, 
    l$brke_msg,
    "<br/>",
    "Assets Total at 100 ($): {bal_at_100}<br/><br/>" %>% glue::glue() 
  ))
}

# add notes to data
annotate_data <- function(l){
  # multiple situations
  # - broke: NA vs value
  # - roth access NA vs value
  # - roth access = or != early retire age
  # - roth access = or != retire access
  l$data %>% 
    dplyr::mutate(
      note = case_when(
        age < l$retire_age ~ color_key$label[1],
        age >= l$broke ~ color_key$label[5],
        age >= l$retire_access & age <= 100 ~ color_key$label[4],
        age >= l$retire_age & age < dplyr::coalesce(l$roth_access, l$retire_access) ~ color_key$label[2],
        age >= l$roth_access & age < l$retire_access ~ color_key$label[3],
        TRUE ~ as.character(NA)
      )
    ) 
}

make_networth_plot <- function(d){
  plot_data <- d %>% 
    dplyr::filter(variable == "net_worth") %>% 
    dplyr::mutate(value = value / 1000)
  milestones <- plot_data %>% 
    dplyr::mutate(notes = factor(note, levels = color_key$label)) %>% 
    dplyr::group_by(notes) %>% 
    dplyr::summarise(start = min(age), end = min(max(age)+0.99, 100))
  use_color_key <- dplyr::filter(color_key, label %in% milestones$notes)
  
  potential_x <- rev(c(milestones$start, 100))
  x_axis <- potential_x[abs(diff(potential_x)) > 4]
  max_y <- min(ceiling(max(plot_data$value)), 10000)
  
  ggplot() +
    # shading segments
    geom_rect(data = milestones, aes(xmin = start, xmax = end, ymin = 0, fill = notes), ymin = 0, ymax = 12500, color = "transparent", alpha = use_color_key$alpha) + 
    geom_segment(data = milestones, aes(x = start, xend = start, y = 0, yend = max_y), size = 0.3) +
    geom_segment(data = milestones, aes(x = min(start), xend = 100, y = 0, yend = 0), size = 0.3) +
    geom_segment(data = milestones, aes(x = 100, xend = 100, y = 0, yend = max_y), size = 0.3) +
    # account value
    geom_line(data = plot_data, aes(age, value), size = 1.11) +
    # settings
    scale_x_continuous(breaks = x_axis) +
    scale_y_continuous(limits = c(0, max_y)) + 
    scale_fill_manual(values = use_color_key$color) + 
    labs(x = "Age", y = "Net Worth ($1,000)", title = "Portfolio Projections") +
    theme(
      panel.grid = element_blank(), 
      legend.position = "none",
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      aspect.ratio = 0.75
    )
}

# format table for display
format_table_for_display <- function(d){
  d %>%
    dplyr::mutate_at(vars(-year, -age), round) %>% 
    dplyr::select(age, dplyr::matches("total|cum"), net_worth) %>% 
    purrr::set_names(c(
      "Age", "Tax Accounts Total", 
      "Tax-Deferred Accounts Total", 
      "Tax-Exempt Cumulative Contributions", "Tax-Exempt Accounts Total", 
      "Net Worth"
    ))
}

# make table colors 
make_tab_colors <- function(l){
  # extract colors
  milestones <- annotate_data(l) %>% 
    dplyr::group_by(note) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(age, label = note) %>% 
    dplyr::arrange(age) %>% 
    dplyr::left_join(color_key, "label") %>% 
    dplyr::mutate(age = age - 1)

  styleInterval(c(milestones$age[-1], 100), c(milestones$rgb, "white"))
}

# format table header
format_header <- function(){ 

  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Age'),
        th(colspan = 1, 'Taxable Accounts'),
        th(colspan = 1, 'Tax-Deferred Accounts'),
        th(colspan = 2, 'Tax-Exempt Accounts'),
        th(rowspan = 2, 'Net Worth')
      ),
      tr(
        lapply(c("Total", "Total", "Cumulative Contribution", "Total"), th)
      )
    )
  ))
}
