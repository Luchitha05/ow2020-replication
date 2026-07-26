library(tidyverse)
library(zoo)

gdp <- read_csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/GDPC1.csv")
ur  <- read_csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/UNRATE.csv")
ipd <- read_csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/IPDNBS.csv")
cpi <- read_csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/CPIAUCSL.csv")
vix <- read_csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/VIXCLS.csv")
ffr <- read_csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/FEDFUNDS.csv")

fred_quarterly <- gdp %>%
  full_join(ur,  by = "observation_date") %>%
  full_join(ipd, by = "observation_date") %>%
  full_join(cpi, by = "observation_date") %>%
  full_join(vix, by = "observation_date") %>%
  full_join(ffr, by = "observation_date") %>%
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
  arrange(dateq)

fred_quarterly <- fred_quarterly %>%
  mutate(
    dlog_gdp = log(gdp) - lag(log(gdp)),
    dlog_cpi = log(cpi) - lag(log(cpi))
  ) %>%
  select(
    dateq,
    gdp,
    ur,
    ipd,
    cpi,
    vix,
    ffr,
    dlog_gdp,
    dlog_cpi
  )

fred_quarterly %>%
  summarise(
    first_q = min(dateq, na.rm = TRUE),
    last_q = max(dateq, na.rm = TRUE),
    missing_gdp = sum(is.na(gdp)),
    missing_ur = sum(is.na(ur)),
    missing_ipd = sum(is.na(ipd)),
    missing_cpi = sum(is.na(cpi)),
    missing_vix = sum(is.na(vix)),
    missing_ffr = sum(is.na(ffr))
  )
write_csv(fred_quarterly, "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/fred_quarterly.csv")
