library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(haven)
library(readr)

bea_stock <- read_excel(
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/BEA_Netstock.xlsx",
  sheet = "Datasets"
)

bea_dep <- read_excel(
  "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_raw/BEA_Dep.xlsx",
  sheet = "Datasets"
)
#Renaming
stock_raw<- bea_stock %>% rename(series_code = 1)
dep_raw  <- bea_dep  %>% rename(series_code = 1)

#Getting only TOTAL EQUIPMENT, TOTAL STRUCTURES, TOTAL INTELLECTUAL
stock_totals <- stock_raw %>%
  filter(str_detect(series_code, "EQ00|ST00|IP00"))

dep_totals <- dep_raw %>%
  filter(str_detect(series_code, "EQ00|ST00|IP00"))

#Now we pivot
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

#Next we need to extract the code from the names
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
#Merging these two
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

#Now we aggrigate the groups and calculate depreciation
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
#Using gen delta_ind = 1 - (1-delta_ind_ann)^(1/4) to convert annual to quarterly

bea_quarterly <- bea_annual %>%
  crossing(quarter = 1:4) %>%
  mutate(
    delta_ind = 1 - (1 - delta_ind_ann)^(1/4),
    dateq = as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  select(naics_id, dateq, delta_ind)

write_csv(bea_quarterly, "~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/bea_fixed_assets_depreciation_data.csv")
