# ============================================================
# Ottonello & Winberry (2020, Econometrica)
# "Financial Heterogeneity and the Investment Channel of Monetary Policy"
#
# Figure 1(a): local projections of the leverage-shock interaction
# coefficient at horizons h = 0 to 20, plotted in the paper's style and
# overlaid on the authors' published path. Section numbers follow the
# estimation pipeline. Gaps in the numbering correspond to steps handled
# in separate scripts or outside the scope of this replication.
#
# Inputs:  construct_panel_data_firm_trim.csv, built by
#          construct_panel_data.R into data_constructed/
#          results/dynamics_lev_baseline.csv, shipped with the replication
#          package, holding the authors' own Figure 1(a) coefficients
# Outputs: figure1a_leverage_paper_style.png
#          figure1a_overlay.png
# ============================================================
library(tidyverse)  # dplyr, tidyr, purrr, ggplot2, readr
library(fixest)     # feols, nobs
# ============================================================
# Paths
# ============================================================
dir_root <- "."
dir_constructed <- file.path(dir_root, "data_constructed")
dir_results  <- file.path(dir_root, "results")
# Panel built by an earlier script
file_panel_trim <- file.path(dir_constructed, "construct_panel_data_firm_trim.csv")
# Published path, shipped with the replication package
file_ow_lev     <- file.path(dir_results, "dynamics_lev_baseline.csv")
# Outputs
file_fig1a_png   <- file.path(dir_results, "figure1a_leverage_paper_style.png")
file_overlay_png <- file.path(dir_results, "figure1a_overlay.png")
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1  Load the constructed firm panel
# ============================================================

comp_trim <- read_csv(file_panel_trim, show_col_types = FALSE)
stopifnot(!any(duplicated(comp_trim[c("gvkey", "dateq")])))

# ============================================================
# 2  Firm-level controls
# ============================================================
controls_firm <- c("rsales_g_std","size_std","sh_current_a_std")

# ============================================================
# 3  Figure 1(a) regressions: leverage
# ============================================================
# One regression per horizon h = 0, ..., 20, with the cumulative investment
# response cumF{h}dlog_capital on the left-hand side.

fig1_lev_models <- list()

for (lead in 0:20) {
  outcome_var <- paste0("cumF", lead, "dlog_capital")
  regression_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ lev_wins_dem_std_gdp + ",
      "lev_wins_dem_std_wide + ",
      "lev_wins_dem_std + ",
      paste(controls_firm, collapse = " + "),
      " | gvkey + maj_ind^dateq + fiscal_dummy"
    )
  )
  fig1_lev_models[[paste0("h", lead)]] <- feols(
    regression_formula,
    data = comp_trim %>% filter(!great_recession),
    cluster = ~ dateq + gvkey
  )
}

# ============================================================
# 4  Coefficient and standard error at each horizon
# ============================================================

fig1_lev_results <- map_dfr(
  0:20,
  function(lead) {
    
    model <- fig1_lev_models[[paste0("h", lead)]]
    
    tidy_model <- broom::tidy(model)
    
    tidy_model %>%
      filter(term == "lev_wins_dem_std_wide") %>%
      transmute(
        horizon = lead,
        beta_h = estimate,
        se = std.error,
        ci_low_90 = beta_h - 1.645 * se,
        ci_high_90 = beta_h + 1.645 * se,
        ci_low_95 = beta_h - 1.96 * se,
        ci_high_95 = beta_h + 1.96 * se,
        nobs = nobs(model)
      )
  }
)

print(fig1_lev_results)

write_csv(fig1_lev_results, file.path(dir_results, "dynamics_lev_replication.csv"))


# ============================================================
# 5  Plot the replicated path
# ============================================================

fig1_lev_paper <- fig1_lev_results %>%
  filter(horizon <= 12) %>%
  mutate(
    ci_low = beta_h - 1.645 * se,
    ci_high = beta_h + 1.645 * se
  )

fig1_lev_plot_paper <- ggplot(fig1_lev_paper, aes(x = horizon)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
  geom_line(aes(y = beta_h), linewidth = 0.9, color = "navy") +
  geom_line(aes(y = ci_low), linewidth = 0.7, linetype = "dashed", color = "navy") +
  geom_line(aes(y = ci_high), linewidth = 0.7, linetype = "dashed", color = "navy") +
  scale_x_continuous(
    breaks = 0:12,
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    breaks = seq(-8, 6, by = 2),
    limits = c(-8, 6)
  ) +
  labs(
    title = "(a) Leverage",
    x = "Quarters since shock",
    y = "Interaction coefficient"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank()
  )
print(fig1_lev_plot_paper)
ggsave(
  filename = file_fig1a_png,
  plot = fig1_lev_plot_paper,
  width = 5.5,
  height = 4.2,
  dpi = 300
)
# ============================================================
# 6 The authors' published path, and comparison
# ============================================================
# The shipped csv has no header row: line 1 holds the 21 coefficients and
# line 2 the 21 standard errors, each with a leading label field. read_csv
# would take line 1 as column names, so the file is read with readLines and
# split by hand

raw_lines <- readLines(file_ow_lev)
header_vals <- strsplit(raw_lines[1], ",")[[1]]
data_vals   <- strsplit(raw_lines[2], ",")[[1]]

# Strip surrounding quotes, then convert
header_num <- as.numeric(gsub('"', '', header_vals[-1]))
data_num   <- as.numeric(gsub('"', '', data_vals[-1]))

# Guards on the shape and on the two published impact values, so a change
# in the shipped file is caught before it reaches the overlay
stopifnot(length(header_num) == 21, length(data_num) == 21)
stopifnot(round(header_num[1], 2) == -0.57)
stopifnot(round(data_num[1], 2) == 0.27)

ow_lev_path <- tibble(
  horizon = 0:20,
  ow_beta = header_num,
  ow_se   = data_num
) %>%
  mutate(
    ow_ci_low_90  = ow_beta - 1.645 * ow_se,
    ow_ci_high_90 = ow_beta + 1.645 * ow_se
  )

print(ow_lev_path, n = 21)

# Merge with the replicated path
fig1_compare <- fig1_lev_results %>%
  filter(horizon <= 12) %>%
  left_join(ow_lev_path %>% filter(horizon <= 12), by = "horizon") %>%
  mutate(
    repl_ci_low_90  = beta_h - 1.645 * se,
    repl_ci_high_90 = beta_h + 1.645 * se
  )

print(fig1_compare, n = 13)

# Shape comparison, normalised by impact effect
fig1_compare %>%
  mutate(repl_norm = beta_h / first(beta_h),
         ow_norm   = ow_beta / first(ow_beta)) %>%
  select(horizon, beta_h, repl_norm, ow_beta, ow_norm) %>%
  print(n = 13)

# ============================================================
# 7  Overlay plot
# ============================================================
fig1_overlay <- ggplot(fig1_compare, aes(x = horizon)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
  geom_ribbon(aes(ymin = repl_ci_low_90, ymax = repl_ci_high_90),
              fill = "navy", alpha = 0.15) +
  geom_line(aes(y = beta_h, color = "Replication"), linewidth = 0.9) +
  geom_ribbon(aes(ymin = ow_ci_low_90, ymax = ow_ci_high_90),
              fill = "firebrick", alpha = 0.10) +
  geom_line(aes(y = ow_beta, color = "Ottonello-Winberry (2020)"),
            linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("Replication" = "navy",
                                "Ottonello-Winberry (2020)" = "firebrick")) +
  scale_x_continuous(breaks = 0:12, limits = c(0, 12)) +
  labs(title = "(a) Leverage: Replication vs. Original",
       x = "Quarters since shock", y = "Interaction coefficient", color = NULL) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        panel.grid.major = element_line(linewidth = 0.3),
        panel.grid.minor = element_blank())

print(fig1_overlay)

ggsave(
  filename = file_overlay_png,
  plot = fig1_overlay, width = 6.5, height = 4.5, dpi = 300
)


fig1_table <- fig1_compare %>%
  transmute(
    horizon,
    repl_beta = round(beta_h, 2), repl_se = round(se, 2),
    ow_beta   = round(ow_beta, 2), ow_se   = round(ow_se, 2),
    diff      = round(beta_h - ow_beta, 2)
  )

print(fig1_table, n = 13)