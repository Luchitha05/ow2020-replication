# ============================================================================
# Ottonello, P. and T. Winberry (2020), "Financial Heterogeneity and the
# Investment Channel of Monetary Policy", Econometrica 88(6), 2473-2502.
#
# Impact-effect firm-level regressions. R translation of
# regressions_firmlevel_impacteffect.do.
#
# Inputs
#   data_constructed/construct_panel_data_firm_trim.csv
#     Trimmed firm-quarter panel written by construct_panel_data.R
#
# Outputs
#   None written to disk. Tables print to the console.
#
# Table numbering follows the published paper, as in the do-file. Gaps are of
# two kinds. Tables 4-6, 8 and 16-17 are not produced by this do-file in the
# original package either. Tables 12, 13 and 19 are within its scope but are
# dropped here under the restrictions below.
#
# Scope restrictions (professor-defined; full statement in the audit document)
#   R1  Distance to default. <reason from audit document>. All d2d columns
#       dropped: tables 3, 9, 10, 11, 14, 15, 18, 20, 21, 22, 23.
#   R2  Greenbook forecasts and forecast revisions. <reason>. Tables 12 and 13
#       dropped in full; table 14 loses columns 4-6.
#   R3  Gurkaynak-Sack-Swanson target and path shocks. <reason>. Table 19
#       dropped in full.
#   R4  Credit ratings. <reason>, so aboveA_dummy cannot be built. Table 9
#       loses columns 3 and 5.
#
# A restricted column is dropped rather than reported whenever it collapses
# into a column already reported in the same table. Columns that remain
# distinct specifications after restriction are retained.
#
# Model objects are named m<published column>_t<published table>.
# ============================================================================

library(dplyr)
library(fixest)

# ---- Estimation options ----
# reghdfe-compatible defaults. Must run before any feols() call.
setFixest_ssc(ssc(fixef.K = "nested"))          # reghdfe-style dof correction 
setFixest_estimation(fixef.rm = "singleton")    # reghdfe drops singletons

# ---- Paths ----
# Object names match construct_panel_data.R so the two scripts stay consistent.
dir_root        <- "."
dir_constructed <- file.path(dir_root, "data_constructed")
file_panel      <- file.path(dir_constructed, "construct_panel_data_firm_trim.csv")

# ============================================================
# 1  Load data
# ============================================================

comp_trim <- read.csv(file_panel)

stopifnot(!any(duplicated(comp_trim[c("gvkey", "dateq")])))
# The control vector 
controls_firm <- c("rsales_g_std", "size_std", "sh_current_a_std")

# ============================================================
# Table 3  Heterogeneous Responses of Investment to Monetary Policy
# ============================================================
# Columns 1,2  replicated
# Column 3     omitted (R1): d2d-only specification, nothing remains
# Column 4     omitted (R1): collapses into column 2 once d2d is removed
# Column 5     restricted (R1): d2d terms dropped, so the point estimate is not directly comparable to the published column


m1_t3 <- feols(
  dlog_capital ~ lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
) 

m2_t3 <- feols(
  dlog_capital ~ lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m5_t3 <- feols(
  dlog_capital ~ wide + lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std +
    L1_dlog_gdp + L2_dlog_gdp + L3_dlog_gdp + L4_dlog_gdp +
    L1_dlog_cpi + L2_dlog_cpi + L3_dlog_cpi + L4_dlog_cpi +
    L1_ur + L2_ur + L3_ur + L4_ur |
    gvkey + maj_ind^quarter + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
# specification replicated; point estimate not directly comparable due to scoped exclusion of distance-to-default.
etable(
  m1_t3, m2_t3, m5_t3,
  keep_raw = c("lev_wins_dem_std_wide", "wide"),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    wide = "FFR shock"
  )
)


# ============================================================
# Table 7  Empirical results, model and data
# ============================================================
# Columns 1,3     replicated
# Column 2     omitted (R5): structural model column
# Column 4     omitted (R5): structural model column

m1_t7 <- feols(
  dlog_capital ~ 
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m2_t7 <- feols(
  dlog_capital ~ 
    lev_wins_dem_std_gdp +
    lev_wins_dem_wide +
    lev_wins_dem +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t7, m2_t7,
  keep_raw = c("lev_wins_dem_std_wide", "lev_wins_dem_wide"),
  dict = c(
    lev_wins_dem_std_wide = "Standardized leverage × FFR shock",
    lev_wins_dem_wide = "Raw demeaned leverage × FFR shock"
  )
)

# ============================================================
# Table 9  Heterogeneous responses, not demeaning financial positions
# ============================================================
# Columns 1,2  replicated
# Column 3     omitted (R4): credit rating only, nothing remains
# Column 4     omitted (R1): d2d-only specification, nothing remains
# Column 5     omitted (R4): collapses into column 2 once the rating dummy is removed
# Column 6     omitted (R1): collapses into column 2 once d2d is removed
# Column 7     restricted (R1): d2d terms dropped, so the point estimate is not directly comparable to the published column

m1_t9 <- feols(
  dlog_capital ~
    lev_wins_nodem_std_gdp +
    lev_wins_nodem_std_wide +
    lev_wins_nodem_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m2_t9 <- feols(
  dlog_capital ~
    lev_wins_nodem_std_gdp +
    lev_wins_nodem_std_wide +
    lev_wins_nodem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m7_t9 <- feols(
  dlog_capital ~
    wide +
    lev_wins_nodem_std_gdp +
    lev_wins_nodem_std_wide +
    lev_wins_nodem_std +
    rsales_g_std + size_std + sh_current_a_std +
    L1_dlog_gdp + L2_dlog_gdp + L3_dlog_gdp + L4_dlog_gdp +
    L1_dlog_cpi + L2_dlog_cpi + L3_dlog_cpi + L4_dlog_cpi +
    L1_ur + L2_ur + L3_ur + L4_ur |
    gvkey + maj_ind^quarter + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t9, m2_t9, m7_t9,
  keep_raw = c("lev_wins_nodem_std_wide", "wide"),
  dict = c(
    lev_wins_nodem_std_wide = "Non-demeaned leverage × FFR shock",
    wide = "FFR shock"
  )
)
# ============================================================
# Table 10  Main results, not controlling for differences in cyclical sensitivities
# ============================================================
# Column 1     replicated
# Column 2     omitted (R1): d2d-only specification, nothing remains
# Column 3     omitted (R1): collapses into column 1 once d2d is removed
# Column 4     restricted (R1): d2d terms dropped, so the point estimate is not directly comparable to the published column

m1_t10 <- feols(
  dlog_capital ~
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m4_t10 <- feols(
  dlog_capital ~
    wide +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std +
    L1_dlog_gdp + L2_dlog_gdp + L3_dlog_gdp + L4_dlog_gdp +
    L1_dlog_cpi + L2_dlog_cpi + L3_dlog_cpi + L4_dlog_cpi +
    L1_ur + L2_ur + L3_ur + L4_ur |
    gvkey + maj_ind^quarter + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t10, m4_t10,
  keep_raw = c("lev_wins_dem_std_wide", "wide"),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    wide = "FFR shock"
  )
)

# ============================================================
# Table 11  Expansionary vs. Contractionary Shocks
# ============================================================
# Columns 1,2  replicated
# Columns 3,4  omitted (R1): d2d-only specifications

m1_t11 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m2_t11 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_pwide +
    lev_wins_dem_std_nwide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
etable(
  m1_t11, m2_t11,
  keep_raw = c(
    "lev_wins_dem_std_wide",
    "lev_wins_dem_std_pwide",
    "lev_wins_dem_std_nwide"
  ),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    lev_wins_dem_std_pwide = "Leverage × positive FFR shock",
    lev_wins_dem_std_nwide = "Leverage × negative FFR shock"
  )
)

# ============================================================
# Table 12  Controlling for Greenbook Forecast Revisions
# ============================================================
# Not estimated. Every column interacts financial position with Greenbook
# forecast revisions (R2), which is the point of the table.
# Columns 1, 3, 5  omitted (R2): removing the forecast revision interactions collapses each into table 3 column 2
# Columns 2, 4, 6  omitted (R1, R2): d2d specifications, nothing remains


# ============================================================
# Table 13  Controlling for Greenbook Forecasts
# ============================================================
# Not estimated. As table 12, with forecast levels in place of revisions.
# Columns 1, 3, 5  omitted (R2): removing the forecast interactions collapses each into table 3 column 2
# Columns 2, 4, 6  omitted (R1, R2): d2d specifications, nothing remains


# ============================================================
# Table 14  Post-1994 Estimates
# ============================================================
# Column 1     replicated
# Column 2     omitted (R1): d2d-only specification, nothing remains
# Column 3     omitted (R1): collapses into column 1 once d2d is removed
# Column 4     omitted (R2): collapses into column 1 once the Greenbook revision interactions are removed
# Column 5     omitted (R1, R2): nothing remains
# Column 6     omitted (R1, R2): collapses into column 1

# Column 1: post-1994 leverage baseline
m1_t14 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession, year >= 1994)
)


etable(
  m1_t14, 
  keep_raw = c("lev_wins_dem_std_wide", "wide"),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    wide = "FFR shock"
  )
)

# ============================================================
# Table 15  Lagged Investment
# ============================================================
# Column 1     replicated
# Column 2     omitted (R1): d2d-only specification, nothing remains
# Column 3     omitted (R1): collapses into column 1 once d2d is removed
# Column 4     restricted (R1): d2d terms dropped, so the point estimate is not directly comparable to the published column

m1_t15 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    Ldl_capital +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m4_t15 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    Ldl_capital +
    rsales_g_std + size_std + sh_current_a_std +
    L1_dlog_gdp + L2_dlog_gdp + L3_dlog_gdp + L4_dlog_gdp +
    L1_dlog_cpi + L2_dlog_cpi + L3_dlog_cpi + L4_dlog_cpi +
    L1_ur + L2_ur + L3_ur + L4_ur |
    gvkey + maj_ind^quarter + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t15, m4_t15,
  keep_raw = c("lev_wins_dem_std_wide", "Ldl_capital", "wide"),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    Ldl_capital = "Lagged investment growth",
    wide = "FFR shock"
  )
)

# ============================================================
# Table 18  Controlling for differences in cyclical sensitivities
# ============================================================
# Columns 1, 3, 5  replicated
# Columns 2, 4, 6  omitted (R1): d2d-only specifications, nothing remains

m1_t18 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m3_t18 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_cpi +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m5_t18 <- feols(
  dlog_capital ~
    lev_wins_dem_std_ur +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t18, m3_t18, m5_t18,
  keep_raw = c(
    "lev_wins_dem_std_wide",
    "lev_wins_dem_std_gdp",
    "lev_wins_dem_std_cpi",
    "lev_wins_dem_std_ur"
  ),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    lev_wins_dem_std_gdp = "Leverage × GDP growth",
    lev_wins_dem_std_cpi = "Leverage × CPI growth",
    lev_wins_dem_std_ur = "Leverage × unemployment"
  )
)

# ============================================================
# Table 19  Target vs. Path Decomposition
# ============================================================
# Not estimated.
# Column 1     omitted: identical specification to table 3 column 2
# Column 2     omitted (R3): target and path shocks unavailable
# Column 3     omitted (R1): d2d-only specification, nothing remains
# Column 4     omitted (R1, R3): nothing remains

# ============================================================
# Table 20  Alternative Time Aggregation
# ============================================================
# Column 1     replicated
# Column 2     omitted (R1): d2d-only specification, nothing remains
# Column 3     omitted (R1): collapses into column 1 once d2d is removed
# Column 4     restricted (R1): d2d terms dropped, so the point estimate is not
#              directly comparable to the published column

m1_t20 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wides +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m4_t20 <- feols(
  dlog_capital ~
    wide_sum +
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wides +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std +
    L1_dlog_gdp + L2_dlog_gdp + L3_dlog_gdp + L4_dlog_gdp +
    L1_dlog_cpi + L2_dlog_cpi + L3_dlog_cpi + L4_dlog_cpi +
    L1_ur + L2_ur + L3_ur + L4_ur |
    gvkey + maj_ind^quarter + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t20, m4_t20,
  keep_raw = c("lev_wins_dem_std_wides", "wide_sum"),
  dict = c(
    lev_wins_dem_std_wides = "Leverage × FFR shock sum",
    wide_sum = "FFR shock sum"
  )
)

# ============================================================
# Table 21  Interaction with Other Firm-Level Covariates
# ============================================================
# Columns 1, 3, 5, 7  replicated
# Columns 2, 4, 6, 8  omitted (R1): d2d-only specifications, nothing remains

m1_t21 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    L0rsales_g_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
#Column 2 (d2d) omitted
m3_t21 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    L0rsales_Fg4_std_wide +
    lev_wins_dem_std +
    rsales_Fg4_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

#Column 4 (d2d) omitted
m5_t21 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    L0size_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

#Column 6 (d2d) omitted
m7_t21 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    liq_wins_dem_std_wide +
    lev_wins_dem_std +
    liq_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
#Column 8 (d2d) omitted
etable(
  m1_t21, m3_t21, m5_t21, m7_t21,
  keep_raw = c(
    "lev_wins_dem_std_wide",
    "L0rsales_g_std_wide",
    "L0rsales_Fg4_std_wide",
    "L0size_std_wide",
    "liq_wins_dem_std_wide"
  ),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    L0rsales_g_std_wide = "Sales growth × FFR shock",
    L0rsales_Fg4_std_wide = "Future sales growth × FFR shock",
    L0size_std_wide = "Size × FFR shock",
    liq_wins_dem_std_wide = "Liquidity × FFR shock"
  )
)

# ============================================================
# Table 22  Interaction with Other Measures of Financial Positions
# ============================================================
# Columns 1, 3, 5, 7  replicated
# Columns 2, 4, 6, 8  omitted (R1): d2d-only specifications, nothing remains

m1_t22 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    L0size_std_wide +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m3_t22 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    cash_ebit_wins_dem_std_wide +
    cash_ebit_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m5_t22 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    L0divmiss_dvpq_wide +
    divmiss_dvpq +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m7_t22 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    liq_wins_dem_std_wide +
    liq_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
etable(
  m1_t22, m3_t22, m5_t22, m7_t22,
  keep_raw = c(
    "lev_wins_dem_std_wide",
    "L0size_std_wide",
    "cash_ebit_wins_dem_std_wide",
    "L0divmiss_dvpq_wide",
    "liq_wins_dem_std_wide"
  ),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    L0size_std_wide = "Size × FFR shock",
    cash_ebit_wins_dem_std_wide = "Cash flow × FFR shock",
    L0divmiss_dvpq_wide = "Dividend payer × FFR shock",
    liq_wins_dem_std_wide = "Liquidity × FFR shock"
  )
)
# ============================================================
# Table 23  Instrumenting Financial Position with Past Financial Position
# ============================================================
# Columns 1,2,3  replicated
# Columns 4,5,6  omitted (R1): d2d instrumented specifications, nothing remains

m1_t23 <- feols(
  dlog_capital ~
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy |
    lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std ~
    levL1_wins_dem_std_gdp + levL1_wins_dem_std_wide + levL1_wins_dem_std,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m2_t23 <- feols(
  dlog_capital ~
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy |
    lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std ~
    levL2_wins_dem_std_gdp + levL2_wins_dem_std_wide + levL2_wins_dem_std,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
m3_t23 <- feols(
  dlog_capital ~
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy |
    lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std ~
    levL4_wins_dem_std_gdp + levL4_wins_dem_std_wide + levL4_wins_dem_std,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)
etable(
  m1_t23, m2_t23, m3_t23,
  keep_raw = "fit_lev_wins_dem_std_wide",
  dict = c(
    fit_lev_wins_dem_std_wide = "Leverage × FFR shock"
  )
)

# ============================================================
# Table 24  Decomposition of Leverage
# ============================================================
# Columns 1-7  replicated, no columns omitted

m1_t24 <- feols(
  dlog_capital ~
    lev_wins_dem_std_gdp +
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m2_t24 <- feols(
  dlog_capital ~
    levnet_wins_dem_std_gdp +
    levnet_wins_dem_std_wide +
    levnet_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m3_t24 <- feols(
  dlog_capital ~
    shstdt_wins_dem_std_gdp +
    shstdt_wins_dem_std_wide +
    shstdt_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m4_t24 <- feols(
  dlog_capital ~
    shltdt_wins_dem_std_gdp +
    shltdt_wins_dem_std_wide +
    shltdt_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m5_t24 <- feols(
  dlog_capital ~
    shstdt_wins_dem_std_gdp +
    shltdt_wins_dem_std_gdp +
    shstdt_wins_dem_std_wide +
    shstdt_wins_dem_std +
    shltdt_wins_dem_std_wide +
    shltdt_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m6_t24 <- feols(
  dlog_capital ~
    sh_ol_wins_dem_std_gdp +
    sh_ol_wins_dem_std_wide +
    sh_ol_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

m7_t24 <- feols(
  dlog_capital ~
    sh_l_wins_dem_std_gdp +
    sh_l_wins_dem_std_wide +
    sh_l_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

etable(
  m1_t24, m2_t24, m3_t24, m4_t24, m5_t24, m6_t24, m7_t24,
  keep_raw = c(
    "lev_wins_dem_std_wide",
    "levnet_wins_dem_std_wide",
    "shstdt_wins_dem_std_wide",
    "shltdt_wins_dem_std_wide",
    "sh_ol_wins_dem_std_wide",
    "sh_l_wins_dem_std_wide"
  ),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    levnet_wins_dem_std_wide = "Net leverage × FFR shock",
    shstdt_wins_dem_std_wide = "Short-term debt × FFR shock",
    shltdt_wins_dem_std_wide = "Long-term debt × FFR shock",
    sh_ol_wins_dem_std_wide = "Other liabilities × FFR shock",
    sh_l_wins_dem_std_wide = "Liabilities × FFR shock"
  )
)

