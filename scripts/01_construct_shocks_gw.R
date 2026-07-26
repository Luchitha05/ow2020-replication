# ============================================================
# Ottonello & Winberry (2020), "Financial Heterogeneity and the Investment
# Channel of Monetary Policy", Econometrica
#
# Builds the GW wide-window monetary policy surprise series: reads the
# event-level surprises, rescales them to decimals, and aggregates them to
# quarterly frequency both unweighted and with the within-quarter timing
# weights used in the paper.
#
# Inputs
#   data_raw/replication_dataset_gw.xlsx           sheet 2, event-level surprises
#
# Outputs
#   data_constructed/construct_shocks_daily.csv    event-level series
#   data_constructed/construct_shocks_quarterly.csv
#
# Section numbers follow the Stata do-file. This script defines no helper
# functions, so that section is absent.
# ============================================================

library(tidyverse)  # dplyr, readr
library(readxl)     # read_excel
library(lubridate)  # days
library(zoo)        # as.yearqtr


# ============================================================
# Paths
# ============================================================

dir_root <- "."
dir_raw         <- file.path(dir_root, "data_raw")
dir_constructed <- file.path(dir_root, "data_constructed")

# Raw input
file_gw_xlsx <- file.path(dir_raw, "replication_dataset_gw.xlsx")

# Outputs
file_shocks_daily     <- file.path(dir_constructed, "construct_shocks_daily.csv")
file_shocks_quarterly <- file.path(dir_constructed, "construct_shocks_quarterly.csv")


# Read the GW replication file
gw_raw <- read_excel(file_gw_xlsx, sheet = 2)

#Keep the FOMC date and the wide-window surprise
gw_daily <- gw_raw %>%
  rename(
    dated = DATE,
    GW_wide = wide_surprise
  ) %>%
  mutate(
    dated = as.Date(dated),
    # Source file reports surprises in percent; the do-file rescales to decimals so coefficients are read per percentage point
    GW_wide = GW_wide / 100
  ) %>%
  select(dated, GW_wide) %>%
  filter(!is.na(dated), !is.na(GW_wide)) %>%
  arrange(dated)

# Checking duplicates
gw_daily %>%
  count(dated) %>%
  filter(n > 1)

write_csv(gw_daily, file_shocks_daily)

# Create quarterly timing weights
gw_quarterly_input <- gw_daily %>%
  mutate(
    dateq = as.yearqtr(dated),
    quarter_start = as.Date(dateq),
    quarter_end = as.Date(dateq + 0.25) - days(1),
    days_in_qtr = as.numeric(quarter_end - quarter_start + 1),
    shock_qtr_day = as.numeric(dated - quarter_start + 1),
    
    weight_to_current_shock = (days_in_qtr - shock_qtr_day) / days_in_qtr,
    weight_to_next_shock = shock_qtr_day / days_in_qtr,
    
    GW_wide_to_current_shock = weight_to_current_shock * GW_wide,
    GW_wide_to_next_shock = weight_to_next_shock * GW_wide
  )

# Collapse event shocks to quarter-level shocks
gw_quarterly <- gw_quarterly_input %>%
  group_by(dateq) %>%
  summarise(
    GW_wide = sum(GW_wide, na.rm = TRUE),
    GW_wide_to_current_shock = sum(GW_wide_to_current_shock, na.rm = TRUE),
    GW_wide_to_next_shock = sum(GW_wide_to_next_shock, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dateq) %>%
  mutate(
    GW_wide_w = GW_wide_to_current_shock + lag(GW_wide_to_next_shock)
  ) %>%
  select(dateq, GW_wide, GW_wide_w)

write_csv(gw_quarterly, file_shocks_quarterly)