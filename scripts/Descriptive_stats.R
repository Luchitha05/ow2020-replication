# ============================================================
# Ottonello and Winberry (2020, Econometrica)
# "Financial Heterogeneity and the Investment Channel of Monetary Policy"
#
# descriptive_statistics.R -- R translation of
# descriptive_statistics.do. Produces Table 1 (moments of the
# monetary policy shock series) and Table 2 Panel A (moments of
# the firm-level variables). Each table is written to the results
# folder as a .csv and as a booktabs .tex fragment.
#
# Section headers follow the do-file. Table 2 Panels B and C are
# not produced; the reason is stated in the Table 2 section.
#
# Inputs
#   data_constructed/construct_shocks_daily.csv
#   data_constructed/construct_shocks_quarterly.csv
#   data_constructed/construct_panel_data_firm_trim.csv
#
# Outputs
#   Replication/Results/tab_descriptives_monetary_shocks.csv
#   Replication/Results/tab_descriptives_monetary_shocks.tex
#   Replication/Results/tab_descriptives_firmlevel_a_restricted.csv
#   Replication/Results/tab_descriptives_firmlevel_a_restricted.tex
# ============================================================

library(tidyverse)  # dplyr, tidyr, stringr, readr
library(zoo)        # as.yearqtr
library(knitr)      # kable


# ============================================================
# Paths
# ============================================================

dir_root        <- "."
dir_constructed <- file.path(dir_root, "data_constructed")
dir_results     <- file.path(dir_root, "results")

# Series built by earlier scripts
file_shocks_daily     <- file.path(dir_constructed, "construct_shocks_daily.csv")
file_shocks_quarterly <- file.path(dir_constructed, "construct_shocks_quarterly.csv")
file_panel_trim       <- file.path(dir_constructed, "construct_panel_data_firm_trim.csv")

# Outputs
file_tab1_csv <- file.path(dir_results, "tab_descriptives_monetary_shocks.csv")
file_tab1_tex <- file.path(dir_results, "tab_descriptives_monetary_shocks.tex")
file_tab2_csv <- file.path(dir_results, "tab_descriptives_firmlevel_a_restricted.csv")
file_tab2_tex <- file.path(dir_results, "tab_descriptives_firmlevel_a_restricted.tex")

dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# Table 1  Summary statistics of monetary policy shocks
# ============================================================
# Sample window 1 Jan 1990 to 31 Dec 2007. Shocks are reported in basis
# points, hence the factor of 100.

# ---- A. High frequency (event-date) shock moments ----

shocks_daily <- read_csv(file_shocks_daily, show_col_types = FALSE) %>%
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


# ---- B. Quarterly shock moments ----

shocks_quarterly <- read_csv(file_shocks_quarterly, show_col_types = FALSE)

# dateq arrives either as "1990 Q1" text or as the underlying numeric,
# depending on how the constructed file was written
shocks_quarterly <- shocks_quarterly %>%
  mutate(
    dateq_chr = as.character(dateq),
    dateq_yq = case_when(
      str_detect(dateq_chr, "Q") ~ as.yearqtr(dateq_chr, format = "%Y Q%q"),
      TRUE ~ as.yearqtr(as.numeric(dateq_chr))
    )
  )

# Filter window follows OW (1990Q1-2007Q4), but the GW series does not
# start until 1994Q2, so the reported moments are computed on the
# 1994Q2-2007Q4 subsample in practice
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

print(coverage_note)

# GW_wide_w is the within-quarter weighted (smoothed) shock; GW_wide is
# the simple within-quarter sum
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


# ---- C. Assemble and write Table 1 ----

# relationship = "one-to-one" errors out if a Statistic label is ever
# repeated on either side, which would silently duplicate rows
table1 <- hf_stats %>%
  left_join(quarterly_table, by = "Statistic", relationship = "one-to-one")

write_csv(table1, file_tab1_csv)

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
  # The .tex header differs in case from the .csv header; left as is so
  # the published fragment is unchanged
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

writeLines(as.character(table1_tex), file_tab1_tex)


# ============================================================
# Table 2  Summary statistics of firm-level variables
# ============================================================

# ---- Panel A ----

panel <- read_csv(file_panel_trim, show_col_types = FALSE)

# dateq loses its yearqtr class on the write_csv/read_csv round trip, so
# restore it before comparing against as.yearqtr values in the filter
panel <- panel %>%
  mutate(dateq = as.yearqtr(as.character(dateq), format = "%Y Q%q"))

stopifnot(!any(is.na(panel$dateq)))

# Match the estimation sample window
panel <- panel %>%
  filter(
    dateq >= as.yearqtr("1983 Q3"),
    dateq <= as.yearqtr("2014 Q4")
  )

# Restriction R1 (distance to default): the do-file also reports moments
# of d2default. The input .dta from which it is built is absent from the
# replication package, so the variable does not exist in this pipeline.
#
# Restriction R4 (credit ratings): the do-file also reports the above-A
# rating dummy aboveA_dummy. The S&P rating field is not in the current
# Compustat extract, so the dummy cannot be constructed.]

firm_vars_available <- c("dlog_capital", "lev")

table2_panel_a <- panel %>%
  select(all_of(firm_vars_available)) %>%
  summarise(
    across(
      everything(),
      list(
        Mean = ~mean(.x, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        `S.D.` = ~sd(.x, na.rm = TRUE),
        # type = 2 reproduces Stata's summarize, detail percentile rule
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

write_csv(table2_panel_a, file_tab2_csv)

# Column headings switch to the paper's labels only in the .tex fragment;
# the .csv keeps the variable names so it can be diffed against the panel
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

writeLines(as.character(table2_panel_a_tex), file_tab2_tex)


# ---- Panels B and C: not produced ----
# Both panels are correlation matrices over all four firm-level
# variables. Two of the four (d2default, aboveA_dummy) are unavailable
# under restrictions R1 and R4 above, so the correlations cannot be
# computed on a comparable basis and the panels are omitted entirely.