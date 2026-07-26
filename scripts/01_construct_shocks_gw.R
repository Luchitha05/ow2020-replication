library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

# ------------------------------------------------------------
# 1. Read the GW replication Excel sheet
# ------------------------------------------------------------

gw_raw <- read_excel("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/replication_dataset_gw.xlsx", sheet = 2)

# ------------------------------------------------------------
# 2. Keep only the FOMC date and wide monetary surprise
# ------------------------------------------------------------

gw_daily <- gw_raw %>%
  rename(
    dated = DATE,
    GW_wide = wide_surprise
  ) %>%
  mutate(
    dated = as.Date(dated),
    # Excel says shocks are in percent.
    # The original Stata code divides by 100 to convert percent to decimal.
    GW_wide = GW_wide / 100
  ) %>%
  select(dated, GW_wide) %>%
  filter(!is.na(dated), !is.na(GW_wide)) %>%
  arrange(dated)
#Checking duplicates
gw_daily %>%
  count(dated) %>%
  filter(n > 1)

write_csv(
  gw_daily,
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/construct_shocks_daily.csv"
)
# ------------------------------------------------------------
# 3. Create quarterly timing weights
# ------------------------------------------------------------

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
# ------------------------------------------------------------
# 4. Collapse event shocks to quarter-level shocks
# ------------------------------------------------------------

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

write_csv(gw_quarterly, "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/construct_shocks_quarterly.csv")