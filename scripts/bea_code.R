# ============================================================
# Ottonello & Winberry (2020, Econometrica)
# "Financial Heterogeneity and the Investment Channel of Monetary Policy"
#
# Constructs industry-by-quarter depreciation rates from BEA fixed
# asset tables (net stock and depreciation), by 2-digit NAICS code
#
# Inputs:
#   data_raw/BEA_Netstock.xlsx (sheet "Datasets")
#   data_raw/BEA_Dep.xlsx      (sheet "Datasets")
# Outputs:
#   data_constructed/bea_fixed_assets_depreciation_data.csv
# ============================================================

library(tidyverse)  # dplyr, tidyr, stringr
library(readxl)      # read_excel
library(zoo)         # as.yearqtr

# ---- Paths ----
dir_root        <- "."
dir_raw         <- file.path(dir_root, "data_raw")
dir_constructed <- file.path(dir_root, "data_constructed")

# raw inputs
file_bea_stock <- file.path(dir_raw, "BEA_Netstock.xlsx")
file_bea_dep   <- file.path(dir_raw, "BEA_Dep.xlsx")

# outputs
file_bea_quarterly <- file.path(dir_constructed, "bea_fixed_assets_depreciation_data.csv")


# ============================================================
# I  Load raw BEA tables
# ============================================================
bea_stock <- read_excel(file_bea_stock, sheet = "Datasets")
bea_dep   <- read_excel(file_bea_dep, sheet = "Datasets")


# First column holds the BEA series code but is unnamed on import
stock_raw <- bea_stock %>% rename(series_code = 1)
dep_raw   <- bea_dep %>% rename(series_code = 1)

# ============================================================
# II  Keep total equipment, structures, and intellectual property
# ============================================================
stock_totals <- stock_raw %>%
  filter(str_detect(series_code, "EQ00|ST00|IP00"))

dep_totals <- dep_raw %>%
  filter(str_detect(series_code, "EQ00|ST00|IP00"))

# ============================================================
# III  Reshape to long (one row per series-year)
# ============================================================
stock_long <- stock_totals %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "stock"
  ) %>%
  mutate(year = as.integer(year))

dep_long <- dep_totals %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "depreciation"
  ) %>%
  mutate(year = as.integer(year))

# ============================================================
# IV  Extract BEA industry code and asset type from series code
# ============================================================
stock_long <- stock_long %>%
  mutate(
    bea_code = str_match(series_code, "K1N(.+?)1(EQ00|ST00|IP00)\\.A")[, 2],
    asset_type = str_match(series_code, "K1N(.+?)1(EQ00|ST00|IP00)\\.A")[, 3]
  )

dep_long <- dep_long %>%
  mutate(
    bea_code = str_match(series_code, "M1N(.+?)1(EQ00|ST00|IP00)\\.A")[, 2],
    asset_type = str_match(series_code, "M1N(.+?)1(EQ00|ST00|IP00)\\.A")[, 3]
  )
# ============================================================
# V  Merge stock and depreciation series
# ============================================================
bea_long <- stock_long %>%
  select(bea_code, asset_type, year, stock) %>%
  left_join(
    dep_long %>% select(bea_code, asset_type, year, depreciation),
    by = c("bea_code", "asset_type", "year")
  )

# mapping BEA labelings to 2-digit NAICS
bea_long <- bea_long %>%
  mutate(
    naics_id = as.numeric(str_sub(bea_code, 1, 2))
  )

# ============================================================
# VI  Aggregate to NAICS-year and compute annual depreciation rate
# ============================================================
bea_annual <- bea_long %>%
  group_by(naics_id, year) %>%
  summarise(
    stock_total = sum(stock, na.rm = TRUE),
    depreciation_total = sum(depreciation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delta_ind_ann = depreciation_total / stock_total
  )
# ============================================================
# VIII  Convert annual to quarterly depreciation rate
# ============================================================
# Stata: gen delta_ind = 1 - (1-delta_ind_ann)^(1/4)

bea_quarterly <- bea_annual %>%
  crossing(quarter = 1:4) %>%
  mutate(
    delta_ind = 1 - (1 - delta_ind_ann)^(1/4),
    dateq = as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  select(naics_id, dateq, delta_ind)

write_csv(bea_quarterly, file_bea_quarterly)
