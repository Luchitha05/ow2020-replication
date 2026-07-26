# ============================================================
# Ottonello & Winberry (2020, Econometrica)
# "Financial Heterogeneity and the Investment Channel of Monetary Policy"

# Constructs the quarterly FRED macro panel: real GDP, unemployment
# rate, GDP deflator, CPI, VIX, and the federal funds rate, plus
# log-differenced GDP and CPI
#
# Inputs:
#   data_raw/GDPC1.csv
#   data_raw/UNRATE.csv
#   data_raw/IPDNBS.csv
#   data_raw/CPIAUCSL.csv
#   data_raw/VIXCLS.csv
#   data_raw/FEDFUNDS.csv
# Outputs:
#   data_constructed/fred_quarterly.csv
# ============================================================

library(tidyverse)  # dplyr, tidyr, stringr, readr
library(zoo)         # as.yearqtr

# ---- Paths ----
dir_root        <- "."
dir_raw         <- file.path(dir_root, "data_raw")
dir_constructed <- file.path(dir_root, "data_constructed")

# raw inputs
file_gdp <- file.path(dir_raw, "GDPC1.csv")
file_ur  <- file.path(dir_raw, "UNRATE.csv")
file_ipd <- file.path(dir_raw, "IPDNBS.csv")
file_cpi <- file.path(dir_raw, "CPIAUCSL.csv")
file_vix <- file.path(dir_raw, "VIXCLS.csv")
file_ffr <- file.path(dir_raw, "FEDFUNDS.csv")

# outputs
file_fred_quarterly <- file.path(dir_constructed, "fred_quarterly.csv")

# ============================================================
# I  Load raw FRED series
# ============================================================
gdp <- read_csv(file_gdp)
ur  <- read_csv(file_ur)
ipd <- read_csv(file_ipd)
cpi <- read_csv(file_cpi)
vix <- read_csv(file_vix)
ffr <- read_csv(file_ffr)
# ============================================================
# II  Merge series on observation date and label to quarter
# ============================================================
# Each series is one row per date, so every join is one-to-one
fred_quarterly <- gdp %>%
  full_join(ur,  by = "observation_date", relationship = "one-to-one") %>%
  full_join(ipd, by = "observation_date", relationship = "one-to-one") %>%
  full_join(cpi, by = "observation_date", relationship = "one-to-one") %>%
  full_join(vix, by = "observation_date", relationship = "one-to-one") %>%
  full_join(ffr, by = "observation_date", relationship = "one-to-one") %>%
  mutate(
    observation_date = as.Date(observation_date),
    dateq = as.yearqtr(observation_date)
  ) %>%
  rename(
    gdp = GDPC1,
    ur  = UNRATE,
    ipd = IPDNBS,
    cpi = CPIAUCSL,
    vix = VIXCLS,
    ffr = FEDFUNDS
  ) %>%
  # sort by quarter: the log-difference step below depends on this order
  arrange(dateq)

# ============================================================
# III  Compute log differences and select final columns
# ============================================================
fred_quarterly <- fred_quarterly %>%
  mutate(
    dlog_gdp = log(gdp) - lag(log(gdp)),
    dlog_cpi = log(cpi) - lag(log(cpi))
  ) %>%
  select(dateq, gdp, ur, ipd, cpi, vix, ffr, dlog_gdp, dlog_cpi)

write_csv(fred_quarterly, file_fred_quarterly)