library(dplyr)
library(fixest)
library(modelsummary)


setFixest_ssc(ssc(fixef.K = "nested"))          # reghdfe-style dof correction 
setFixest_estimation(fixef.rm = "singleton")    # reghdfe drops singletons

comp_trim= read.csv("~/Desktop/RA 2026/15949_Data_and_Programs_1/Data_replication_package_ecma/data_constructed/construct_panel_data_firm_trim.csv")
names(comp_trim)
# The control vector 
controls_firm <- c("rsales_g_std", "size_std", "sh_current_a_std")

#Restricted tables
#Distance to default: 3,9,10, 11,14,15,18, 20,21, 22,23
#credit ratings: 9
# Greenbook forecasts: had to omit 12,13, affected 14
#GSS path shocks: omitted 19


# ------------------------------------------------------------
# Restricted Table 3: leverage-only baseline

m1 <- feols(
  dlog_capital ~ lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
) #Identical

m2 <- feols(
  dlog_capital ~ lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)#identical

#dropped columns 3,4 since no d2d 
m5 <- feols(
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
  m1, m2, m5,
  keep_raw = c("lev_wins_dem_std_wide", "wide"),
  dict = c(
    lev_wins_dem_std_wide = "Leverage × FFR shock",
    wide = "FFR shock"
  )
)


# ------------------------------------------------------------
# Table 7:  standardized vs unstandardized leverage

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
#COMMENT
#the negative leverage-shock interaction is present both when leverage is standardized and when it is left in raw demeaned units. The standardized specification is easier to interpret because the coefficient corresponds to a one sd increase in demeaned leverage.

# ------------------------------------------------------------
# Table 9: Heterogeneous responses

# Column 1
m1_t9 <- feols(
  dlog_capital ~
    lev_wins_nodem_std_gdp +
    lev_wins_nodem_std_wide +
    lev_wins_nodem_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

#Column 2
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
#Columns 3,5 removed as rating data unavailable
# Columns4,6 removed as d2d unavailable

#Column 7: Match on lev side, d2d scoped out
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
# The leverage-shock interaction stays negative and statistically significant, hence the restricted leverage result is robust to using non-demeaned leverage. This specification is less clean for within-firm interpretation because it combines within-firm changes with cross-firm leverage differences.

# ------------------------------------------------------------
# Table 10R: No cyclical sensitivity controls

# Restricted replication: leverage-only

# Column 1: leverage interaction, with firm controls, no leverage x GDP control
m1_t10 <- feols(
  dlog_capital ~
    lev_wins_dem_std_wide +
    lev_wins_dem_std +
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

# columns 2,3 (d2d) scoped out
# Column 4: aggregate controls version, no leverage x GDP control (Without d2d)
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
# dropping the leverage × GDP control weakens the estimated leverage-shock interaction, although the coefficient remains negative.

# ------------------------------------------------------------
# Table 11R: Expansionary vs contractionary shocks

# Column 1: baseline pooled shock
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

# Column 2: split positive and negative shocks
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
# Column 3,4 (d2d) scoped
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
#Less leveraged firms reduce investment more after unexpected monetary loosening, the tightening effect is weekly significant
#but the evidence is weak because the expansionary coefficient is only significant at the 10% level.



#Cant do Table 12, 13 since those require Greenbook forecasts 

# ------------------------------------------------------------
# Table 14R: Post-1994 estimates

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
#the baseline estimation sample appears to be effectively post-1994 after dropping rows with missing regression variables. Therefore, applying the post-1994 restriction does not change the Table 14R estimates.

# ------------------------------------------------------------
# Table 15R: Lagged Investment]

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

#2nd and 3rd column removed

#Included on leverage side d2d terms dropped; coefficient not directly comparable to paper
m4_t15 <- feols(
  dlog_capital ~
    wide +
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
# Even after controlling for past investment growth, more leveraged firm-quarters respond more to monetary policy shocks.

# ------------------------------------------------------------
# Table 18R: Extra cyclical sensitivity controls

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

# Column 2 requires d2d

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

# Column 4 required d2d

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

#Column 6 excluded as no d2d

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

#Leverage × FFR shock stays negative, hence effect is ot simply driven by leverage-specific sensitivity to GDP or CPI, however when controlling for leverage-specific unemployment sensitivity the result weakens

#Table 19 skipped entirely as no GSS target/path

# ------------------------------------------------------------
# Table 20: Alternative Time Aggregation

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

# Column 2,3 emmited as no d2d

#Column 4: Included on lev side, d2d dropped
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
#Result still statistically significant, hence the restricted leverage result is robust to the time-aggregation method used for the monetary policy shock.

# ------------------------------------------------------------
# Table 21R: Interaction with other firm-level covariates

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
# The negative leverage coefficient remains after these additional firm-level interactions are included.

# ------------------------------------------------------------
# Table 22R: Other financial-position measures

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

#Column 2 (d2d) omitted
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

#Column 4 (d2d) omitted
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

#Column 6 (d2d) omitted
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

#Column 6 (d2d) omitted
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

#Survives robustness check(again): firm size, cash flow, dividend-paying status, or liquidity

# ------------------------------------------------------------
# Table 23R: IV using lagged leverage

# 1-quarter lag instrument
m1_t23 <- feols(
  dlog_capital ~
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy |
    lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std ~
    levL1_wins_dem_std_gdp + levL1_wins_dem_std_wide + levL1_wins_dem_std,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

# 2-quarter lag instrument
m2_t23 <- feols(
  dlog_capital ~
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy |
    lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std ~
    levL2_wins_dem_std_gdp + levL2_wins_dem_std_wide + levL2_wins_dem_std,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

# 4-quarter lag instrument
m3_t23 <- feols(
  dlog_capital ~
    rsales_g_std + size_std + sh_current_a_std |
    gvkey + maj_ind^dateq + fiscal_dummy |
    lev_wins_dem_std_gdp + lev_wins_dem_std_wide + lev_wins_dem_std ~
    levL4_wins_dem_std_gdp + levL4_wins_dem_std_wide + levL4_wins_dem_std,
  cluster = ~ dateq + gvkey,
  data = comp_trim %>% filter(!great_recession)
)

#columns 4,5,6 excluded(d2d)
etable(
  m1_t23, m2_t23, m3_t23,
  keep_raw = "fit_lev_wins_dem_std_wide",
  dict = c(
    fit_lev_wins_dem_std_wide = "Leverage × FFR shock"
  )
)
# The IV estimates remain negative across all lag choices, with the four-quarter lag specification weakly significant.
# ------------------------------------------------------------
# Table 24R: Decomposition of Leverage: All columns match

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

#The monetary-policy sensitivity is closely related to debt net of cash/liquid assets, not just gross leverage.
# The leverage effect is mainly coming from short-term debt exposure, not long-term debt as it is insignificant. If monetary policy tightens, firms with more short-term debt may face quicker increases in borrowing costs, also insignificant for total liabilities

