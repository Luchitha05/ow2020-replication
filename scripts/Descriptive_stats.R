library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(knitr)
library(kableExtra)
library(tidyverse)
library(zoo)
# ------------------------------------------------------------
# A. High-frequency/event-date shock moments
# ------------------------------------------------------------
results <- "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/Replication/Results"

dir.create(results, recursive = TRUE, showWarnings = FALSE)

shocks_daily <- read_csv(
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/construct_shocks_daily.csv",
  show_col_types = FALSE
) %>%
  mutate(dated = as.Date(dated))

hf_stats <- shocks_daily %>%
  filter(
    dated >= as.Date("1990-01-01"),
    dated <= as.Date("2007-12-31")
  ) %>%
  mutate(GW_wide_100 = GW_wide * 100) %>%
  summarise(
    Mean = mean(GW_wide_100, na.rm = TRUE),
    Median = median(GW_wide_100, na.rm = TRUE),
    `S.D.` = sd(GW_wide_100, na.rm = TRUE),
    Min = min(GW_wide_100, na.rm = TRUE),
    Max = max(GW_wide_100, na.rm = TRUE),
    Observations = sum(!is.na(GW_wide_100))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Statistic",
    values_to = "High Frequency"
  )

print(hf_stats)

# B. Quarterly shock moments

shocks_quarterly <- read_csv(
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/construct_shocks_quarterly.csv",
  show_col_types = FALSE
)

shocks_quarterly <- shocks_quarterly %>%
  mutate(
    dateq_chr = as.character(dateq),
    dateq_yq = case_when(
      str_detect(dateq_chr, "Q") ~ as.yearqtr(dateq_chr, format = "%Y Q%q"),
      TRUE ~ as.yearqtr(as.numeric(dateq_chr))
    )
  )
coverage_note <- tibble(
  series = c("daily", "quarterly"),
  first_obs = c(
    as.character(min(shocks_daily$dated[!is.na(shocks_daily$GW_wide)], na.rm = TRUE)),
    as.character(min(shocks_quarterly$dateq_yq[!is.na(shocks_quarterly$GW_wide_w)], na.rm = TRUE))
  ),
  last_obs = c(
    as.character(max(shocks_daily$dated[!is.na(shocks_daily$GW_wide)], na.rm = TRUE)),
    as.character(max(shocks_quarterly$dateq_yq[!is.na(shocks_quarterly$GW_wide_w)], na.rm = TRUE))
  )
)
# Filter window follows OW (1990Q1-2007Q4); shock series coverage begins 1994,
print(coverage_note)


quarterly_stats <- shocks_quarterly %>%
  filter(
    dateq_yq >= as.yearqtr("1990 Q1"),
    dateq_yq <= as.yearqtr("2007 Q4")
  ) %>%
  mutate(
    GW_wide_100 = GW_wide * 100,
    GW_wide_w_100 = GW_wide_w * 100
  ) %>%
  summarise(
    Smoothed_Mean = mean(GW_wide_w_100, na.rm = TRUE),
    Smoothed_Median = median(GW_wide_w_100, na.rm = TRUE),
    Smoothed_SD = sd(GW_wide_w_100, na.rm = TRUE),
    Smoothed_Min = min(GW_wide_w_100, na.rm = TRUE),
    Smoothed_Max = max(GW_wide_w_100, na.rm = TRUE),
    Smoothed_Observations = sum(!is.na(GW_wide_w_100)),
    
    Sum_Mean = mean(GW_wide_100, na.rm = TRUE),
    Sum_Median = median(GW_wide_100, na.rm = TRUE),
    Sum_SD = sd(GW_wide_100, na.rm = TRUE),
    Sum_Min = min(GW_wide_100, na.rm = TRUE),
    Sum_Max = max(GW_wide_100, na.rm = TRUE),
    Sum_Observations = sum(!is.na(GW_wide_100))
  )

quarterly_table <- tibble(
  Statistic = c("Mean", "Median", "S.D.", "Min", "Max", "Observations"),
  Smoothed = c(
    quarterly_stats$Smoothed_Mean,
    quarterly_stats$Smoothed_Median,
    quarterly_stats$Smoothed_SD,
    quarterly_stats$Smoothed_Min,
    quarterly_stats$Smoothed_Max,
    quarterly_stats$Smoothed_Observations
  ),
  Sum = c(
    quarterly_stats$Sum_Mean,
    quarterly_stats$Sum_Median,
    quarterly_stats$Sum_SD,
    quarterly_stats$Sum_Min,
    quarterly_stats$Sum_Max,
    quarterly_stats$Sum_Observations
  )
)


table1 <- hf_stats %>%
  left_join(quarterly_table, by = "Statistic")

#saving table
write_csv(
  table1,
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/Replication/Results/tab_descriptives_monetary_shocks.csv"
)


# Format Table 1 for LaTeX
table1_latex <- table1 %>%
  mutate(
    `High Frequency` = ifelse(
      Statistic == "Observations",
      sprintf("%.0f", `High Frequency`),
      sprintf("%.3f", `High Frequency`)
    ),
    Smoothed = ifelse(
      Statistic == "Observations",
      sprintf("%.0f", Smoothed),
      sprintf("%.3f", Smoothed)
    ),
    Sum = ifelse(
      Statistic == "Observations",
      sprintf("%.0f", Sum),
      sprintf("%.3f", Sum)
    )
  ) %>%
  rename(
    `High frequency` = `High Frequency`
  )

table1_tex <- kable(
  table1_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r"),
  escape = FALSE,
  linesep = ""
)

writeLines(
  as.character(table1_tex),
  file.path(results, "tab_descriptives_monetary_shocks.tex")
)

# Table 2 Panel A: Summary Statistics of Firm-Level Variables

panel <- read_csv(
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/construct_panel_data_firm_trim.csv",
  show_col_types = FALSE
)

panel= comp_trim
#to make it similar to the sample
panel <- panel %>%
  filter(
    dateq >= as.yearqtr("1983 Q3"),
    dateq <= as.yearqtr("2014 Q4")
  )
# Check availability of original Table 2 variables
original_table2_vars <- c("gvkey", "dateq", "dlog_capital", "lev", "d2default", "aboveA_dummy")

available_table2_vars <- intersect(original_table2_vars, names(panel))
missing_table2_vars <- setdiff(original_table2_vars, names(panel))



# Only variables available in our restricted replication
firm_vars_available <- c("dlog_capital", "lev")
firm_vars_available <- intersect(firm_vars_available, names(panel))

table2_panel_a <- panel %>%
  select(all_of(firm_vars_available)) %>%
  summarise(
    across(
      everything(),
      list(
        Mean = ~mean(.x, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        `S.D.` = ~sd(.x, na.rm = TRUE),
        `95th Percentile` = ~quantile(.x, probs = 0.95, na.rm = TRUE, type = 2),
        Observations = ~sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "(.+)_(Mean|Median|S\\.D\\.|95th Percentile|Observations)",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value
  ) %>%
  mutate(
    Statistic = factor(
      Statistic,
      levels = c("Mean", "Median", "S.D.", "95th Percentile", "Observations")
    )
  ) %>%
  arrange(Statistic)

print(table2_panel_a)

table2_panel_a_latex <- table2_panel_a %>%
  mutate(
    Statistic = as.character(Statistic),
    
    Investment = ifelse(
      Statistic == "Observations",
      sprintf("%.0f", dlog_capital),
      sprintf("%.3f", dlog_capital)
    ),
    
    Leverage = ifelse(
      Statistic == "Observations",
      sprintf("%.0f", lev),
      sprintf("%.3f", lev)
    )
  ) %>%
  select(
    Statistic,
    Investment,
    Leverage
  )

table2_panel_a_tex <- kable(
  table2_panel_a_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r"),
  escape = FALSE,
  linesep = ""
)

writeLines(
  as.character(table2_panel_a_tex),
  file.path(
    results,
    "tab_descriptives_firmlevel_a_restricted.tex"
  )
)
write_csv(
  table2_panel_a,
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/Replication/Results/tab_descriptives_firmlevel_a_restricted.csv"
)


# Table 2 Panels B-C: 
# In this restricted replication, d2default and aboveA_dummy are not available, hence we cant do the correlation

