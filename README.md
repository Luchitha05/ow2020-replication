# Financial Heterogeneity and the Investment Channel of Monetary Policy — R Replication
 
An R replication of the empirical section of Ottonello, P. and T. Winberry (2020), ["Financial Heterogeneity and the Investment Channel of Monetary Policy,"](https://doi.org/10.3982/ECTA13603) *Econometrica* 88(6), 2473–2502. The authors' original pipeline was written in Stata; this repository translates it to R, documents where results match or diverge from the published paper, and extends the sample through 2026.
 
Undergraduate research project conducted under Prof. Alaïs Martin-Baillon.
 
## Status
 
- Panel construction: complete and debugged against the published tables
- Baseline coefficient replication: matches OW's post-1994 appendix benchmark (Table XIV) to within a small margin
- Tables 3, 7, 9–11, 14–15, 18–24: audited column-by-column
- Figure 1(a): replicated, shape/timing match published dynamics
- Audit write-up: in progress
- Sample extension through 2026: not yet started
## Scope
 
A few components of the original paper are out of scope for this replication, due to data availability constraints (e.g. distance-to-default measures, Greenbook forecasts, structural shock decompositions, credit ratings) or a narrowed panel period. These restrictions are documented in the audit write-up and noted inline in the relevant scripts.
 
## Repository structure
 
```
data_raw/           Raw input data (not tracked — see Data below)
data_constructed/   Intermediate datasets built by the construction scripts
results/            Output tables and figures
```
 
### Scripts, in run order
 
| Script | Purpose | Output |
|---|---|---|
| `01_construct_shocks_gw.R` | Builds the GW wide-window monetary policy surprise series (event-level and quarterly) | `construct_shocks_daily.csv`, `construct_shocks_quarterly.csv` |
| `bea_code.R` | Constructs industry-by-quarter depreciation rates from BEA fixed asset tables | `bea_fixed_assets_depreciation_data.csv` |
| `FRED_data_construction.R` | Builds the quarterly FRED macro panel (GDP, unemployment, deflator, CPI, VIX, fed funds rate) | `fred_quarterly.csv` |
| `panel_construction.R` | Main firm-quarter panel construction (R translation of the authors' Stata pipeline, sections I–III) | `construct_panel_data_firm_compustat.csv`, `construct_panel_data_firm_trim.csv`, `construct_panel_data_aggregate.csv` |
| `Descriptive_stats.R` | Descriptive statistics tables | Tables 1–2 |
| `regressions_firmlevel_impacteffect.R` | Impact-effect firm-level regressions | Remaining tables (printed to console) |
| `regressions_firmlevel_dynamiceffect.R` | Local projections of the leverage-shock interaction coefficient, horizons h = 0–20; also overlays the replicated path on the authors' published Figure 1(a) coefficients for comparison | `figure1a_leverage_paper_style.png`, `figure1a_overlay.png` |
 
## Raw data
 
To reproduce the pipeline, you will need:
 
- **Compustat quarterly data** (`Quarterly_data.dta`) — via WRDS
- **Datastream year-incorporated data** (`DS_yearinc.csv`) — via WRDS
- **BEA Fixed Asset Tables** — non-residential fixed assets, net stock and depreciation, from the [BEA Fixed Assets interactive tables](https://apps.bea.gov/national/FA2004/Details/Index.htm) (`BEA_Netstock.xlsx`, `BEA_Dep.xlsx`, sheet "Datasets")
- **FRED series** — `GDPC1`, `UNRATE`, `IPDNBS`, `CPIAUCSL`, `VIXCLS`, `FEDFUNDS` (quarterly, US)
- **Monetary policy shock series** (`replication_dataset_gw.xlsx`) — event-level surprises used to construct the GW wide-window shock
- **Authors' published Figure 1(a) coefficients** (`dynamics_lev_baseline.csv`) — from Ottonello & Winberry's own replication package, used to overlay the replicated dynamic effects against the published path
Place these files in `data_raw/` using the filenames above before running the scripts.
 
Access to Compustat and Datastream requires a WRDS subscription (e.g. through an academic institution).
 
## Running the pipeline
 
Run the scripts in the order listed in the table above. Each script's header documents its specific inputs and outputs. `Descriptive_stats.R` and `regressions_firmlevel_impacteffect.R`/`regressions_firmlevel_dynamiceffect.R` can be run independently of each other once `panel_construction.R` has completed.
 
## Notes on methodology
 
- Section numbers in script headers follow the authors' original Stata do-files, to make cross-referencing easier. Gaps in numbering correspond to steps handled elsewhere or outside this replication's scope.
- Because Stata could not be run locally for this project, all comparisons are between R output and the published paper's tables, not R output against the authors' original Stata output directly.
## Reference
 
Ottonello, P. and T. Winberry (2020). "Financial Heterogeneity and the Investment Channel of Monetary Policy." *Econometrica*, 88(6), 2473–2502.
