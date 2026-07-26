# ============================================================
# Ottonello & Winberry (2020, Econometrica)
# "Financial Heterogeneity and the Investment Channel of Monetary Policy"
#
# Panel construction: R translation of the authors' Stata pipeline,
# sections I-III. Section numbers follow the authors' Stata do-file.
# Gaps in the numbering correspond to steps handled in separate scripts or outside the scope of this replication.
# 
# Inputs:  data_raw/Quarterly_data.dta, data_raw/DS_yearinc.csv, plus the
#          shock, BEA and FRED series built by earlier scripts into
#          data_constructed/
# Outputs: construct_panel_data_firm_compustat.csv
#          construct_panel_data_firm_trim.csv
#          construct_panel_data_aggregate.csv
# ============================================================

library(tidyverse)  # dplyr, tidyr, stringr, readr
library(zoo)        # as.yearqtr
library(haven)      # read_dta
library(slider)     # slide_dbl


# ============================================================
# Paths
# ============================================================

# Set dir_root to the top level of the replication package. This is the
# only line that needs to change when moving between machines.
dir_root <- "."

dir_raw         <- file.path(dir_root, "data_raw")
dir_constructed <- file.path(dir_root, "data_constructed")

# Raw inputs
file_compustat_raw <- file.path(dir_raw, "Quarterly_data.dta")
file_ds_yearinc    <- file.path(dir_raw, "DS_yearinc.csv")

# Series built by earlier scripts
file_shocks <- file.path(dir_constructed, "construct_shocks_quarterly.csv")
file_bea    <- file.path(dir_constructed, "bea_fixed_assets_depreciation_data.csv")
file_fred   <- file.path(dir_constructed, "fred_quarterly.csv")

# Intermediate checkpoint, written then read back
file_compustat_clean <- file.path(
  dir_constructed, "compustat_quarterly_variables_clean.csv"
)

# Outputs
file_panel_compustat <- file.path(
  dir_constructed, "construct_panel_data_firm_compustat.csv"
)
file_panel_aggregate <- file.path(
  dir_constructed, "construct_panel_data_aggregate.csv"
)
file_panel_trim <- file.path(
  dir_constructed, "construct_panel_data_firm_trim.csv"
)


# ============================================================
# Helper functions
# ============================================================

# Stata-style lag: returns the previous value only when the previous row is
# exactly one quarter back, otherwise NA. Used for the Ltrim flags.
lag_quarter <- function(x, dateq) {
  previous_x <- lag(x)
  previous_date <- lag(dateq)
  
  consecutive <- as.numeric(dateq - previous_date) == 0.25
  
  if_else(
    !is.na(consecutive) & consecutive,
    previous_x,
    NA
  )
}

# Winsorise at the 0.5th and 99.5th percentiles.
# type = 2 reproduces Stata's default quantile definition.
winsor_005 <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }
  bounds <- quantile(x, probs = c(0.005, 0.995), na.rm = TRUE, type = 2)
  pmin(pmax(x, bounds[[1]]), bounds[[2]])
}
# Standardise to mean zero, unit variance. Returns all NA if the variable is
# constant or entirely missing.
standardize <- function(x) {
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  
  if (is.na(x_sd) || x_sd == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - x_mean) / x_sd
}

# Capital series for one firm-spell: anchor at gross PP&E in the first
# Anchor capital at gross PP&E, then accumulate net investment forward
build_capital <- function(ppegtq, netinv, netinv_seq, first_ppegtq) {
  n <- length(ppegtq)
  capital <- rep(NA_real_, n)
  
  if (n == 0 || all(is.na(first_ppegtq))) {
    return(capital)
  }
  
  start_seq <- first(na.omit(first_ppegtq))
  start_row <- which(netinv_seq == start_seq & !is.na(ppegtq))[1]
  
  if (is.na(start_row)) {
    return(capital)
  }
  
  # Anchor capital using gross PP&E
  capital[start_row] <- ppegtq[start_row]
  
  # Accumulate net investment forward
  if (start_row < n) {
    for (i in seq.int(start_row + 1, n)) {
      if (
        !is.na(capital[i - 1]) &&
        !is.na(netinv[i]) &&
        !is.na(netinv_seq[i]) &&
        netinv_seq[i] > start_seq
      ) {
        capital[i] <- capital[i - 1] + netinv[i]
      }
    }
  }
  
  capital
}


# ============================================================
# I.1  Clean the raw Compustat quarterly extract
# ============================================================


# Load the raw quarterly Compustat extract
comp <- read_dta(file_compustat_raw)

# Make sure key ID variables are numeric where needed
comp <- comp %>%
  mutate(
    gvkey = as.numeric(gvkey),
    sic = as.numeric(sic),
    naics = as.numeric(naics)
  )

# Drop observations with missing calendar quarter
comp <- comp %>%
  filter(!is.na(datacqtr))

# Raw extract should be unique by firm-date
stopifnot(!any(duplicated(comp[c("gvkey", "datadate")])))

# Build the quarterly date index from datacqtr
comp <- comp %>%
  mutate(
    year = as.numeric(str_sub(datacqtr, 1, 4)),
    quarter = as.numeric(str_extract(datacqtr, "(?<=Q)[1-4]")),
    dateq = as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  select(-datacqtr)
# Resolve duplicate firm-quarters: prefer the fully updated record (updq == 3), then the latest datadate
comp <- comp %>%
  group_by(gvkey, dateq) %>%
  arrange(desc(updq == 3), desc(datadate), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Stata-style assert: halt if any duplicates survive
stopifnot(comp %>% count(gvkey, dateq) %>% filter(n > 1) %>% nrow() == 0)

comp <- comp %>% select(-datadate, -fyearq)

# Drop financials (SIC 6000-6799) and utilities (SIC 4900-4999)
comp <- comp %>%
  filter(is.na(sic) | !(sic >= 6000 & sic <= 6799)) %>%
  filter(is.na(sic) | !(sic >= 4900 & sic <= 4999))

# Keep U.S.-incorporated firms
comp <- comp %>%
  filter(fic == "USA")

# Merge DS Worldscope incorporation dates
ds_yearinc <- read_csv(file_ds_yearinc)

comp <- comp %>%
  left_join(ds_yearinc, by = "cusip")

# Fill missing quarterly gaps within each firm
comp <- comp %>%
  group_by(gvkey) %>%
  complete(
    dateq = seq(min(dateq), max(dateq), by = 0.25)
  ) %>%
  arrange(gvkey, dateq) %>%
  fill(conm, .direction = "downup") %>%
  ungroup()

# Keep only variables needed for later construction
vars_to_keep <- c(
  "gvkey", "dateq", "fqtr", "year", "quarter", "conm", "sic", "naics",
  "actq", "aqcy", "atq", "cheq", "dlcq", "dlttq", "dvpq",
  "lctq", "ltq", "oiadpq", "ppegtq", "ppentq", "saleq", "xintq",
  "inc_dateq"
)

comp_clean <- comp %>%
  select(any_of(vars_to_keep))

# Save cleaned Compustat quarterly file
write_csv(comp_clean, file_compustat_clean)

# ============================================================
# I.3  Merge monetary shocks, BEA depreciation and FRED aggregates
# ============================================================

comp_clean <- read_csv(file_compustat_clean)
shocks <- read_csv(file_shocks)

comp_clean <- comp_clean %>%
  mutate(dateq = as.yearqtr(dateq))

shocks <- shocks %>%
  mutate(dateq = as.yearqtr(dateq))

comp_merged <- comp_clean %>%
  left_join(shocks, by = "dateq")

bea_quarterly <- read_csv(file_bea)
# Map two-digit NAICS to the BEA sector depreciation rate
comp_merged <- comp_merged %>%
  mutate(
    naics_id = as.numeric(str_sub(as.character(naics), 1, 2))
  )
bea_quarterly <- bea_quarterly %>%
  mutate(
    dateq = as.yearqtr(dateq)
  )

comp_merged <- comp_merged %>%
  left_join(
    bea_quarterly,
    by = c("naics_id", "dateq")
  )

# ============================================================
# II.2  Capital and investment variables
# ============================================================

comp_panel <- comp_merged %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    # Start with the original PPENTQ
    ppentq2 = ppentq,
    
    # Fill only an isolated missing quarter where both adjacent quarters exist
    ppentq2 = if_else(
      is.na(ppentq2) &
        !is.na(lag(ppentq2)) &
        !is.na(lead(ppentq2)),
      (lag(ppentq2) + lead(ppentq2)) / 2,
      ppentq2
    ),
    
    # Net investment = change in interpolated net PP&E
    netinv = ppentq2 - lag(ppentq2)
  ) %>%
  ungroup()

# Number continuous spells of non-missing net investment within each firm
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    nomiss_netinv = !is.na(netinv),
    
    # A new spell begins when netinv becomes available
    spell_start = nomiss_netinv &
      !lag(nomiss_netinv, default = FALSE),
    
    # Number the spells within each firm
    spell_counter = cumsum(spell_start),
    
    # Rows with missing netinv are not initially part of a spell
    netinv_spell = if_else(
      nomiss_netinv,
      spell_counter,
      NA_integer_
    )
  ) %>%
  group_by(gvkey, netinv_spell) %>%
  mutate(
    # Position within each usable spell: 1, 2, 3, ...
    netinv_seq = if_else(
      !is.na(netinv_spell),
      row_number(),
      NA_integer_
    ),
    
    # Length of the usable net-investment spell
    netinv_spell_length = if_else(
      !is.na(netinv_spell),
      n(),
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  select(-spell_start, -spell_counter)

# Attach the quarter before each spell (seq 0) as the gross PP&E anchor
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    anchor_before_spell =
      is.na(netinv_spell) &
      !is.na(ppegtq) &
      lead(netinv_seq) == 1 &
      !is.na(lead(netinv)),
    
    netinv_spell = if_else(
      anchor_before_spell,
      lead(netinv_spell),
      netinv_spell
    ),
    
    netinv_seq = if_else(
      anchor_before_spell,
      0L,
      netinv_seq
    )
  ) %>%
  ungroup()

# Earliest position in the spell with non-missing gross PP&E
comp_panel <- comp_panel %>%
  group_by(gvkey, netinv_spell) %>%
  mutate(
    first_ppegtq = if (
      all(is.na(netinv_spell)) ||
      all(is.na(ppegtq))
    ) {
      NA_integer_
    } else {
      min(netinv_seq[!is.na(ppegtq)], na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Apply per firm-spell and drop single-observation capital series
spell_rows <- comp_panel %>%
  filter(!is.na(netinv_spell)) %>%
  group_by(gvkey, netinv_spell) %>%
  mutate(
    capital = build_capital(
      ppegtq = ppegtq,
      netinv = netinv,
      netinv_seq = netinv_seq,
      first_ppegtq = first_ppegtq
    ),
    
    ncapital = sum(!is.na(capital)),
    
    # Remove capital sequences containing only one observation
    capital = if_else(
      ncapital == 1,
      NA_real_,
      capital
    )
  ) %>%
  ungroup() %>%
  select(gvkey, dateq, capital)

# Merge the constructed capital back:
comp_panel <- comp_panel %>%
  left_join(
    spell_rows,
    by = c("gvkey", "dateq")
  )
# Nominal investment: change in capital plus industry depreciation
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    nom_inv =
      capital -
      lag(capital) +
      delta_ind * lag(capital)
  ) %>%
  ungroup()

# ============================================================
# II.3  Shock and firm-level variables
# ============================================================

# Merge quarterly FRED aggregates
fred_quarterly 	<- read_csv(file_fred)

fred_quarterly <- fred_quarterly %>%
  mutate(
    dateq = as.yearqtr(dateq)
  )

comp_panel <- comp_panel %>%
  left_join(fred_quarterly, by = "dateq")

# Deflate nominal series by the investment price deflator
comp_panel <- comp_panel %>%
  mutate(
    real_capital = 100 * capital / ipd,
    real_inv = 100 * nom_inv / ipd,
    real_sales = 100 * saleq / ipd,
    real_total_assets = 100 * atq / ipd
  ) 

# Balance-sheet identities used by the flow variables below
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    total_assets      = atq,
    total_liabilities = ltq,
    debt              = lag(dlcq) + lag(dlttq),
    equity            = total_assets - total_liabilities
  ) %>%
  ungroup()

# Getting the remaining variables

comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    # Interest expenses
    int_l = if_else(
      lag(total_liabilities) != 0 &
        !is.na(lag(total_liabilities)),
      xintq / lag(total_liabilities),
      NA_real_
    ),
    
    delta_int_l = (
      lead(xintq, 12) - xintq
    ) / lag(total_liabilities),
    
    # Capital and investment
    log_capital = if_else(
      real_capital > 0,
      log(real_capital),
      NA_real_
    ),
    
    dlog_capital = log_capital - lag(log_capital),
    Ldl_capital = lag(dlog_capital),
    
    inv_rate = if_else(
        lag(real_capital) > 0,
        real_inv / lag(real_capital),
        NA_real_
    ),
    # Changes in debt and equity
    d_debt_rate = (debt - lag(debt)) / lag(total_assets),
    d_equity_rate = (equity - lag(equity)) / lag(total_assets),
    
    # Cash flow
    cash_ebit = if_else(
      capital > 0,
      lag(oiadpq) / capital,
      NA_real_
    )
  ) %>%
  ungroup()

# Stata: gen rsales_growth = (real_sales - L4.real_sales)/L4.real_sales

comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    rsales_growth = if_else(
      lag(real_sales, 4) != 0 &
        !is.na(lag(real_sales, 4)),
      (real_sales - lag(real_sales, 4)) /
        lag(real_sales, 4),
      NA_real_
    ),
    
    rsales_sd_5yr = slide_dbl(
      rsales_growth,
      ~ {
        values <- .x[!is.na(.x)]
        if (length(values) >= 2) sd(values) else NA_real_
      },
      .before = 19,
      .complete = FALSE
    ),
    
    rsales_sd_10yr = slide_dbl(
      rsales_growth,
      ~ {
        values <- .x[!is.na(.x)]
        if (length(values) >= 2) sd(values) else NA_real_
      },
      .before = 39,
      .complete = FALSE
    )
  ) %>%
  ungroup()

comp_panel <- comp_panel %>%
  select(-rsales_growth)

# Major SIC industry groups
comp_panel <- comp_panel %>%
  mutate(
    maj_sic = if_else(
      !is.na(sic),
      floor(sic / 100),
      NA_real_
    ),
    
    ag = if_else(!is.na(maj_sic), maj_sic < 10, NA),
    mining = if_else(
      !is.na(maj_sic),
      between(maj_sic, 10, 14),
      NA
    ),
    constr = if_else(
      !is.na(maj_sic),
      between(maj_sic, 15, 17),
      NA
    ),
    manuf = if_else(
      !is.na(maj_sic),
      between(maj_sic, 20, 39),
      NA
    ),
    trans = if_else(
      !is.na(maj_sic),
      between(maj_sic, 40, 49),
      NA
    ),
    wholesale = if_else(
      !is.na(maj_sic),
      between(maj_sic, 50, 51),
      NA
    ),
    retail = if_else(
      !is.na(maj_sic),
      between(maj_sic, 52, 59),
      NA
    ),
    fire = if_else(
      !is.na(maj_sic),
      between(maj_sic, 60, 67),
      NA
    ),
    services = if_else(
      !is.na(maj_sic),
      between(maj_sic, 70, 89),
      NA
    ),
    pub = if_else(
      !is.na(maj_sic),
      between(maj_sic, 91, 97),
      NA
    ),
    
    maj_ind = case_when(
      sic %in% c(9995, 9997) ~ NA_real_,
      ag == TRUE ~ 1,
      mining == TRUE ~ 2,
      constr == TRUE ~ 3,
      manuf == TRUE ~ 4,
      trans == TRUE ~ 5,
      wholesale == TRUE ~ 6,
      retail == TRUE ~ 7,
      services == TRUE ~ 8,
      TRUE ~ NA_real_
    )
  )

# One incorporation date per gvkey: gvkeys map to several cusips, so keep the earliest

inc_bridge <- comp %>%
  distinct(gvkey, cusip) %>%
  left_join(
    ds_yearinc %>%
      transmute(
        cusip = as.character(cusip),
        inc_date = as.Date(inc_date),
        inc_dateq_exact = as.yearqtr(inc_date)
      ),
    by = "cusip"
  ) %>%
  arrange(gvkey, inc_dateq_exact) %>%
  group_by(gvkey) %>%
  slice(1) %>%
  ungroup() %>%
  select(gvkey, inc_date, inc_dateq_exact)

stopifnot(nrow(inc_bridge) == n_distinct(inc_bridge$gvkey))


comp_panel <- comp_panel %>%
  select(-any_of(c("inc_date", "inc_dateq_exact"))) %>%
  left_join(inc_bridge, by = "gvkey", relationship = "many-to-one")

# Constructing firm age
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    min_dateq = min(dateq, na.rm = TRUE),
    age_sample = as.numeric(dateq - min_dateq),
    age_ds = as.numeric(dateq - inc_dateq_exact),
    age_inc = pmax(age_sample, age_ds, na.rm = TRUE),
    age_inc = if_else(
      is.na(age_sample) & is.na(age_ds),
      NA_real_,
      age_inc
    )
  ) %>%
  ungroup() %>%
  select(-min_dateq)

# Age dummies

comp_panel <- comp_panel %>%
  mutate(
    age_inc_young = case_when(
      is.na(age_inc) ~ NA_real_,
      age_inc < 15 ~ 1,
      TRUE ~ 0
    ),
    age_inc_middle = case_when(
      is.na(age_inc) ~ NA_real_,
      age_inc >= 15 & age_inc <= 50 ~ 1,
      TRUE ~ 0
    ),
    age_inc_old = case_when(
      is.na(age_inc) ~ NA_real_,
      age_inc > 50 ~ 1,
      TRUE ~ 0
    )
  )
# Dividend-paying dummy creation
comp_panel <- comp_panel %>%
  mutate(
    divmiss_dvpq = case_when(
      is.na(dvpq) ~ NA_real_,
      dvpq > 0 ~ 1,
      TRUE ~ 0
    )
  )

# Converting acquisitions from year-to-date to quarterly
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    Laqcy = lag(aqcy),
    Laqcy = if_else(!is.na(fqtr) & fqtr == 1, 0, Laqcy),
    aqc = aqcy - Laqcy
  ) %>%
  ungroup() %>%
  select(-Laqcy)

# Sample and timing dummies
comp_panel <- comp_panel %>%
  mutate(
    great_recession = as.numeric(
      dateq > as.yearqtr("2007 Q4")
    ),
    pre_1994 = as.numeric(
      dateq < as.yearqtr("1994 Q1")
    ),
    fiscal_dummy = case_when(
      is.na(fqtr) ~ NA_real_,
      fqtr == 4 ~ 1,
      TRUE ~ 0
    )
  )
# Creating Gertler–Gilchrist size classification
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    avg_GGsales = slide_dbl(
      real_sales,
      ~ {
        x <- .x[!is.na(.x)]
        if (length(x) == 0) NA_real_ else mean(x)
      },
      .before = 39,
      .complete = FALSE
    )
  ) %>%
  ungroup()

comp_panel <- comp_panel %>%
  group_by(dateq) %>%
  arrange(avg_GGsales, gvkey, .by_group = TRUE) %>%
  mutate(
    rank_GGsales = if_else(
      !is.na(avg_GGsales) &
        dateq >= as.yearqtr("1972 Q1") &
        dateq <= as.yearqtr("2007 Q4"),
      row_number(),
      NA_integer_
    ),
    
    count_GGsales = n(),
    
    pct_GGsales = if_else(
      !is.na(rank_GGsales) & count_GGsales > 0,
      100 * rank_GGsales / count_GGsales,
      NA_real_
    ),
    
    gg_size2_sales_10yr_pct30 = case_when(
      is.na(pct_GGsales) ~ NA_real_,
      pct_GGsales < 30 ~ 0,
      pct_GGsales >= 30 ~ 1
    )
  ) %>%
  ungroup()
comp_panel <- comp_panel %>%
  select(
    -avg_GGsales,
    -rank_GGsales,
    -count_GGsales,
    -pct_GGsales
  )

# Shock variables and their lags
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    # Main shock variables
    wide = -GW_wide_w,
    wide_sum = -GW_wide,
    
    # Quarterly change in the effective federal funds rate
    dmffr = -(ffr - lag(ffr)) / 100,
    
    # Positive and negative wide shocks
    pos_wide = wide * (wide > 0),
    neg_wide = wide * (wide < 0),
    
    # Current and lagged wide shocks
    L0wide = wide,
    L1wide = lag(wide, 1),
    L2wide = lag(wide, 2),
    L3wide = lag(wide, 3),
    L4wide = lag(wide, 4),
    
    # Consistent naming used later
    L0wide_sum = wide_sum,
    L0pos_wide = pos_wide,
    L0neg_wide = neg_wide
  ) %>%
  ungroup()

# Firm-level covariates

comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    # Sales growth from t-2 to t-1
    rsales_g = if_else(
      lag(real_sales, 1) > 0 &
        lag(real_sales, 2) > 0,
      log(lag(real_sales, 1)) -
        log(lag(real_sales, 2)),
      NA_real_
    ),
    
    # Forward four-quarter sales growth
    rsales_Fg4 = if_else(
      real_sales > 0 &
        lead(real_sales, 4) > 0,
      log(lead(real_sales, 4)) -
        log(real_sales),
      NA_real_
    ),
    
    # Liquidity
    liquidity = if_else(
      lag(atq) != 0,
      (lag(actq) - lag(lctq)) / lag(atq),
      NA_real_
    ),
    
    # Cash holdings relative to assets
    liq_ch = if_else(
      lag(atq) != 0,
      lag(cheq) / lag(atq),
      NA_real_
    ),
    
    # Leverage
    lev = if_else(
      lag(atq) != 0,
      (lag(dlcq) + lag(dlttq)) / lag(atq),
      NA_real_
    ),
    
    # Average leverage over current and previous 3 values
    levavg = (
      lev +
        lag(lev, 1) +
        lag(lev, 2) +
        lag(lev, 3)
    ) / 4,
    
    # Net leverage
    levnet = if_else(
      lag(atq) != 0,
      (
        lag(lctq) +
          lag(dlttq) -
          lag(actq)
      ) / lag(atq),
      NA_real_
    ),
    
    # Lagged leverage measures
    levL1 = lag(lev, 1),
    levL2 = lag(lev, 2),
    levL4 = lag(lev, 4),
    
    # Firm size
    size = if_else(
      lag(real_total_assets) > 0,
      log(lag(real_total_assets)),
      NA_real_
    ),
    
    # Balance-sheet shares
    sh_current_a = if_else(
      lag(atq) != 0,
      lag(actq) / lag(atq),
      NA_real_
    ),
    
    sh_l = if_else(
      lag(atq) != 0,
      lag(ltq) / lag(atq),
      NA_real_
    ),
    
    sh_ol = if_else(
      lag(atq) != 0,
      (
        lag(ltq) -
          lag(dlcq) -
          lag(dlttq)
      ) / lag(atq),
      NA_real_
    ),
    
    shstdt = if_else(
      lag(atq) != 0,
      lag(dlcq) / lag(atq),
      NA_real_
    ),
    
    shltdt = if_else(
      lag(atq) != 0,
      lag(dlttq) / lag(atq),
      NA_real_
    )
  ) %>%
  ungroup()

# Cumulative dynamic response variables
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey)

for (i in 0:20) {
  comp_panel <- comp_panel %>%
    mutate(
      !!paste0("cumF", i, "dlog_capital") :=
        lead(log_capital, i) - lag(log_capital, 1),
      
      !!paste0("cumF", i, "d_exfin") :=
        (lead(debt, i) - lag(debt, 1)) / lag(total_assets, 1) +
        (lead(equity, i) - lag(equity, 1)) / lag(total_assets, 1)
    )
}

for (i in 1:20) {
  j <- i + 1
  
  comp_panel <- comp_panel %>%
    mutate(
      !!paste0("cumL", i, "dlog_capital") :=
        lag(log_capital, 1) - lag(log_capital, j)
    )
}

comp_panel <- comp_panel %>%
  ungroup()

# Lagged aggregate controls.
comp_panel <- comp_panel %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    Ldlog_gdp = lag(dlog_gdp),
    Ldlog_cpi = lag(dlog_cpi),
    Lvix = lag(vix),
    Lur = lag(ur)
  ) %>%
  ungroup()
# Checkpoint: firm-quarter panel before trimming
write_csv(comp_panel, file_panel_compustat)

# ============================================================
# III Trim Final Sample and Generate Sample-dependent Standardized Variables
# ============================================================


# ============================================================
# III.1  Trim the estimation sample
# ============================================================

comp_trim <- comp_panel

# Drop non-positive capital or total assets
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_capital =
      coalesce(capital <= 0, FALSE) |
      coalesce(atq <= 0, FALSE),
    
    Ltrim_capital =
      lag_quarter(trim_capital, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_capital)
# Drop quarters with acquisitions above 5% of assets
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    acquisition_ratio = abs(aqc) / atq,
    
    trim_acquisitions =
      is.na(acquisition_ratio) |
      acquisition_ratio > 0.05,
    
    Ltrim_acquisitions =
      lag_quarter(trim_acquisitions, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_acquisitions) %>%
  select(-acquisition_ratio)

# Drop the top and bottom 0.5% of investment rates (quantile type = 2 matches Stata's default)
bottom_inv <- quantile(
  comp_trim$inv_rate,
  probs = 0.005,
  na.rm = TRUE,
  type = 2
)

top_inv <- quantile(
  comp_trim$inv_rate,
  probs = 0.995,
  na.rm = TRUE,
  type = 2
)

comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_invrate =
      is.na(inv_rate) |
      inv_rate < bottom_inv |
      inv_rate > top_inv,
    
    Ltrim_invrate =
      lag_quarter(trim_invrate, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_invrate)

# Require a capital-investment spell of at least 40 quarters
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_spell = coalesce(
      netinv_spell_length < 40,
      FALSE
    ),
    
    Ltrim_spell =
      lag_quarter(trim_spell, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_spell)

lev_p99 <- quantile(
  comp_trim$lev,
  0.99,
  na.rm = TRUE,
  type = 2
)

liq_ch_p99 <- quantile(
  comp_trim$liq_ch,
  0.99,
  na.rm = TRUE,
  type = 2
)

comp_trim <- comp_trim %>%
  mutate(
    lev_top1 = case_when(
      is.na(lev) ~ NA_real_,
      lev >= lev_p99 ~ 1,
      TRUE ~ 0
    ),
    
    liq_top1 = case_when(
      is.na(liq_ch) ~ NA_real_,
      liq_ch >= liq_ch_p99 ~ 1,
      TRUE ~ 0
    )
  )
# Remove extreme liquidity

comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_liq =
      is.na(liquidity) |
      liquidity < -10 |
      liquidity > 10,
    
    Ltrim_liq =
      lag_quarter(trim_liq, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_liq)

# Remove leverage outside 0–10
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_lev =
      is.na(lev) |
      lev < 0 |
      lev > 10,
    
    Ltrim_lev =
      lag_quarter(trim_lev, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_lev)

# Remove extreme sales growth
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_rsales =
      is.na(rsales_g) |
      rsales_g < -1 |
      rsales_g > 1,
    
    Ltrim_rsales =
      lag_quarter(trim_rsales, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_rsales)

# Remove negative sales
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_saleq  = coalesce(saleq < 0, FALSE),
    Ltrim_saleq = lag_quarter(trim_saleq, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_saleq)

# Remove negative cash-to-assets ratios
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    trim_liqch = coalesce(liq_ch < 0, FALSE),
    
    Ltrim_liqch =
      lag_quarter(trim_liqch, dateq)
  ) %>%
  ungroup() %>%
  filter(!trim_liqch)


# ============================================================
# III.2  Sample-dependent standardised variables
# ==========================================================

# Standardizing the basic variables
basic_std_vars <- c(
  "rsales_Fg4",
  "rsales_g",
  "rsales_sd_5yr",
  "rsales_sd_10yr",
  "sh_current_a",
  "size"
)

comp_trim <- comp_trim %>%
  mutate(
    across(
      all_of(basic_std_vars),
      standardize,
      .names = "{.col}_std"
    )
  )

comp_trim <- comp_trim %>%
  mutate(
    L0rsales_g_std_wide =
      rsales_g_std * L0wide,
    
    L0rsales_Fg4_std_wide =
      rsales_Fg4_std * L0wide,
    
    L0size_std_wide =
      size_std * L0wide
  )

nodem_vars <- c("lev", "levavg")

for (var in nodem_vars) {
  wins_name <- paste0(var, "_wins_nodem")
  std_name <- paste0(var, "_wins_nodem_std")
  interaction_name <- paste0(var, "_wins_nodem_std_wide")
  
  comp_trim[[wins_name]] <-
    winsor_005(comp_trim[[var]])
  
  comp_trim[[std_name]] <-
    standardize(comp_trim[[wins_name]])
  
  comp_trim[[interaction_name]] <-
    comp_trim[[std_name]] * comp_trim$wide
}

# Winsorise at 0.5%/99.5%, then demean within firm, then standardise

demean_vars <- c("lev","liq_ch","levavg","levnet","shstdt","shltdt","sh_ol",
  "sh_l","cash_ebit","levL1","levL2","levL4"
)
for (var in demean_vars) {
  wins_name <- paste0(var, "_wins")
  
  comp_trim[[wins_name]] <-
    winsor_005(comp_trim[[var]])
}
comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    across(
      all_of(paste0(demean_vars, "_wins")),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{sub('_wins$', '', .col)}_wins_dem"
    )
  ) %>%
  ungroup()
dem_names <- paste0(demean_vars, "_wins_dem")
# NaN arises where a firm has no non-missing values; recode to NA

comp_trim <- comp_trim %>%
  mutate(
    across(
      all_of(dem_names),
      ~ if_else(is.nan(.x), NA_real_, .x)
    )
  )
for (var in demean_vars) {
  dem_name <- paste0(var, "_wins_dem")
  std_name <- paste0(var, "_wins_dem_std")
  
  comp_trim[[std_name]] <-
    standardize(comp_trim[[dem_name]])
}
for (var in demean_vars) {
  dem_name <- paste0(var, "_wins_dem")
  std_name <- paste0(var, "_wins_dem_std")
  
  comp_trim[[paste0(var, "_wins_dem_std_wide")]] <-
    comp_trim[[std_name]] * comp_trim$wide
  
  comp_trim[[paste0(var, "_wins_dem_wide")]] <-
    comp_trim[[dem_name]] * comp_trim$wide
}
comp_trim <- comp_trim %>%
  select(-all_of(paste0(demean_vars, "_wins")))
	# Leverage only: distance-to-default is out of scope
comp_trim <- comp_trim %>%
  mutate(
    lev_wins_dem_std_pwide =
      lev_wins_dem_std * pos_wide,
    
    lev_wins_dem_std_nwide =
      lev_wins_dem_std * neg_wide,
    
    lev_wins_dem_std_wides =
      lev_wins_dem_std * wide_sum,
    
    lev_wins_dem_std_dmffr =
      lev_wins_dem_std * dmffr
  )
# Match Stata naming: liq_ch_wins_* -> liq_wins_*
comp_trim <- comp_trim %>%
  rename_with(
    ~ sub("^liq_ch_wins_", "liq_wins_", .x),
    starts_with("liq_ch_wins_")
  )

comp_trim <- comp_trim %>%
  mutate(
    int_l_wins = winsor_005(int_l),
    delta_int_l_wins = winsor_005(delta_int_l)
  )

comp_trim <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey)

for (h in 0:20) {
  comp_trim <- comp_trim %>%
    mutate(
      !!paste0("F", h, "int_l_wins") :=
        lead(int_l_wins, h),
      
      !!paste0("F", h, "delta_int_l_wins") :=
        lead(delta_int_l_wins, h)
    )
}
comp_trim <- comp_trim %>%
  ungroup()

for (h in 0:20) {
  source_name <- paste0("cumF", h, "d_exfin")
  output_name <- paste0(source_name, "_wins")
  
  comp_trim[[output_name]] <-
    winsor_005(comp_trim[[source_name]])
}
comp_trim <- comp_trim %>%
  mutate(
    L0divmiss_dvpq_wide =
      divmiss_dvpq * wide
  )
comp_trim <- comp_trim %>%
  mutate(
    lev_wins_nodem_std_gdp =
      lev_wins_nodem_std * Ldlog_gdp,
    
    levavg_wins_nodem_std_gdp =
      levavg_wins_nodem_std * Ldlog_gdp
  )
comp_trim <- comp_trim %>%
  mutate(
    lev_wins_dem_std_gdp =
      lev_wins_dem_std * Ldlog_gdp,
    
    liq_wins_dem_std_gdp =
      liq_wins_dem_std * Ldlog_gdp,
    
    levavg_wins_dem_std_gdp =
      levavg_wins_dem_std * Ldlog_gdp
  )
comp_trim <- comp_trim %>%
  mutate(
    lev_wins_dem_std_cpi =
      lev_wins_dem_std * Ldlog_cpi,
    
    lev_wins_dem_std_ur =
      lev_wins_dem_std * Lur,
    
    levL1_wins_dem_std_gdp =
      levL1_wins_dem_std * Ldlog_gdp,
    
    levL2_wins_dem_std_gdp =
      levL2_wins_dem_std * Ldlog_gdp,
    
    levL4_wins_dem_std_gdp =
      levL4_wins_dem_std * Ldlog_gdp
  )
# Other leveragecomponent interactions with GDP
other_lev_vars <- c("levnet","shstdt","shltdt","sh_ol","sh_l")
for (var in other_lev_vars) {
  std_name <- paste0(var, "_wins_dem_std")
  output_name <- paste0(var, "_wins_dem_std_gdp")
  
  comp_trim[[output_name]] <-
    comp_trim[[std_name]] * comp_trim$Ldlog_gdp
}
heterogeneity_vars <- c("gg_size2_sales_10yr_pct30","age_inc_middle",
  "age_inc_old","rsales_sd_5yr_std","rsales_sd_10yr_std")

for (var in heterogeneity_vars) {
  comp_trim[[paste0(var, "_wide")]] <-
    comp_trim[[var]] * comp_trim$wide
  
  comp_trim[[paste0(var, "_gdp")]] <-
    comp_trim[[var]] * comp_trim$Ldlog_gdp
}
# Keep a row only if every Ltrim flag is a non-missing FALSE. Applied after winsorising and standardising so that those moments are computed on the pre-Ltrim sample, matching the Stata ordering
comp_trim <- comp_trim %>%
  filter(!if_any(starts_with("Ltrim"), ~ is.na(.x) | .x))

# ============================================================
# III.3  Labelling and saving
# ============================================================

# Restriction R4: credit ratings. Columns are retained as NA so downstream table code runs unchanged.
comp_trim <- comp_trim %>%
  mutate(
    rating_enc = NA_real_,
    aboveA_dummy = NA_real_,
    aboveA_dummy_wide = NA_real_,
    aboveA_dummy_gdp = NA_real_
  )

# Aggregate average investment growth, seasonally adjusted
aggregate_panel <- comp_trim %>%
  arrange(gvkey, dateq) %>%
  group_by(gvkey) %>%
  mutate(
    cap_ipd = 100 * capital / ipd,
    inv_ipd = if_else(
      cap_ipd > 0 & lag(cap_ipd) > 0,
      100 * (log(cap_ipd) - log(lag(cap_ipd))),
      NA_real_
    )
  ) %>%
  ungroup()
aggregate_panel <- aggregate_panel %>%
  group_by(dateq) %>%
  summarise(
    count_firms_invavg = n(),
    inv_avg_ipd_nsa    = mean(inv_ipd, na.rm = TRUE),
    .groups = "drop"
  )

aggregate_panel <- aggregate_panel %>%
  mutate(
    quarter = as.numeric(format(dateq, "%q"))
  )

seasonal_model <- lm(
  inv_avg_ipd_nsa ~ factor(quarter),
  data = aggregate_panel,
  na.action = na.exclude
)

aggregate_panel <- aggregate_panel %>%
  mutate(
    inv_avg_ipd_sa1 = residuals(seasonal_model)
  )

aggregate_panel <- aggregate_panel %>%
  select(
    dateq,
    count_firms_invavg,
    inv_avg_ipd_nsa,
    inv_avg_ipd_sa1
  )
# ---- Macro lags ----

# Calendar lags taken from the full FRED series, then nulled Stata-style:
# L(k) is missing if the firm has no observation k quarters back.

fred_lags <- fred_quarterly %>%       
  arrange(dateq) %>%
  mutate(
    L1_dlog_gdp = lag(dlog_gdp, 1), L2_dlog_gdp = lag(dlog_gdp, 2),
    L3_dlog_gdp = lag(dlog_gdp, 3), L4_dlog_gdp = lag(dlog_gdp, 4),
    L1_dlog_cpi = lag(dlog_cpi, 1), L2_dlog_cpi = lag(dlog_cpi, 2),
    L3_dlog_cpi = lag(dlog_cpi, 3), L4_dlog_cpi = lag(dlog_cpi, 4),
    L1_ur = lag(ur, 1), L2_ur = lag(ur, 2),
    L3_ur = lag(ur, 3), L4_ur = lag(ur, 4)
  ) %>%
  select(dateq, starts_with("L1_"), starts_with("L2_"),
         starts_with("L3_"), starts_with("L4_"))

comp_trim <- comp_trim %>%
  left_join(fred_lags, by = "dateq", relationship = "many-to-one") %>%
  group_by(gvkey) %>%
  mutate(
    has_L1 = (dateq - 0.25) %in% dateq,
    has_L2 = (dateq - 0.50) %in% dateq,
    has_L3 = (dateq - 0.75) %in% dateq,
    has_L4 = (dateq - 1.00) %in% dateq,
    across(starts_with("L1_"), ~ if_else(has_L1, .x, NA_real_)),
    across(starts_with("L2_"), ~ if_else(has_L2, .x, NA_real_)),
    across(starts_with("L3_"), ~ if_else(has_L3, .x, NA_real_)),
    across(starts_with("L4_"), ~ if_else(has_L4, .x, NA_real_))
  ) %>%
  ungroup() %>%
  select(-has_L1, -has_L2, -has_L3, -has_L4)

# Save
write_csv(aggregate_panel, file_panel_aggregate)
# Saving panel data
write_csv(comp_trim, file_panel_trim)


