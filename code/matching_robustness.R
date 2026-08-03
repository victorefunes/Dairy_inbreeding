# =====================================================================
# Alternative matching methods (referee robustness).
#
# Your baseline (generate_db_rr.r): MatchIt nearest-neighbor on a logit
# propensity score, with replacement. This script re-runs the founder-
# level match under alternative estimators and checks (a) covariate
# balance and (b) whether the downstream DiD ATT is stable.
#
# Run AFTER generate_db_rr.r up to the point where `data_1` and
# `match_data_1` exist (it reuses your exact covariate formula).
# =====================================================================

setwd("C:/Users/victo/Documents/Dairy_inbreeding/code")
source("generate_db_match.R") 

# Reuse the EXACT propensity formula from your baseline object -----------
ps_formula <- match_data_1$formula          # superstar ~ pta_milk + ... + semex
ratio      <- 50                            # your baseline ratio

# =====================================================================
# TIER 1 -- pair matching: returns a match.matrix, so your existing
# rel_df / get_matches / descendant-building code runs UNCHANGED.
# Swap the object you feed downstream (match_data_1) for any of these.
# =====================================================================

# (a) Mahalanobis-distance NN (no propensity model at all)
m_maha <- matchit(ps_formula, data = data_1, method = "nearest",
                  distance = "mahalanobis", replace = TRUE,
                  ratio = ratio, discard = "none")

# (b) PS-NN with a caliper (drops poorly-matched superstars; tightens balance)
m_cal  <- matchit(ps_formula, data = data_1, method = "nearest",
                  distance = "glm", link = "logit", replace = TRUE,
                  ratio = ratio, caliper = 0.2, discard = "none")   # 0.2 SD of the PS

# (c) 1:1 WITHOUT replacement (most conservative; each control used once)
m_1to1 <- matchit(ps_formula, data = data_1, method = "nearest",
                  distance = "glm", link = "logit", replace = FALSE,
                  ratio = 1, discard = "none")

# (d) Optimal pair matching (minimizes total distance; needs optmatch)
#     install.packages("optmatch")
m_opt  <- matchit(ps_formula, data = data_1, method = "optimal",
                  distance = "glm", link = "logit", ratio = ratio)

# (e) Genetic matching (search over covariate weights; needs Matching, rgenoud)
#     install.packages(c("Matching","rgenoud"))
# m_gen <- matchit(ps_formula, data = data_1, method = "genetic",
#                  distance = "glm", link = "logit", replace = TRUE,
#                  ratio = ratio, pop.size = 200)   # slow with ~70 covariates

# --> For each Tier-1 object, run YOUR existing block verbatim, e.g.:
#       match_data_1 <- m_maha
#       <your match.matrix -> rel_df -> descendants -> feols Models 1-4>
#     then store the treat:post (or event-study) coefficients to compare.

# =====================================================================
# TIER 2 -- weighting / subclassification: NO match.matrix. Instead each
# founder gets a weight; carry that weight to its descendants and run the
# DiD weighted, rather than subsetting matched pairs.
# =====================================================================

# (f) Full matching (uses all controls, optimal subclasses; needs optmatch)
m_full <- matchit(ps_formula, data = data_1, method = "full",
                  distance = "glm", link = "logit", estimand = "ATT")

# (g) IPW and entropy balancing via WeightIt (doubly-robust-ready weights)
#     install.packages(c("WeightIt","ebal"))
library(WeightIt)
w_ipw  <- weightit(ps_formula, data = data_1, method = "glm",  estimand = "ATT")
w_ebal <- weightit(ps_formula, data = data_1, method = "ebal", estimand = "ATT")

# Extract founder-level weights to propagate to descendants ------------
founder_w <- data_1 |>
  mutate(w_full = m_full$weights,
         w_ipw  = w_ipw$weights,
         w_ebal = w_ebal$weights) |>
  select(reg_id, superstar, w_full, w_ipw, w_ebal)

# --> Join founder_w onto your descendant panel by founder (sire_id/reg_id),
#     multiply by your generational weight 2^(-gen_num) if you want both,
#     then in the DiD use: feols(inbreeding ~ ... , weights = ~ w_ebal, ...)
#     The treatment/control split is superstar==1/0; no pair-subsetting.

# =====================================================================
# BALANCE COMPARISON across all methods (this is what the referee wants
# to see first: does balance hold under each estimator?)
# =====================================================================
w_list <- list(
  Nearest_baseline = get.w(match_data_1),
  Mahalanobis      = get.w(m_maha),
  Caliper          = get.w(m_cal),
  NN_1to1          = get.w(m_1to1),
  Optimal          = get.w(m_opt),
  Full             = get.w(m_full),
  IPW              = get.w(w_ipw),
  Entropy          = get.w(w_ebal)
)

bal.tab(ps_formula, data = data_1,
        weights      = w_list,
        stats        = "mean.diffs",
        s.d.denom    = "treated",      # ATT -> standardize by treated SD
        thresholds   = c(m = 0.1),
        abs          = TRUE)

# love plot for the two or three you report
lp <- love.plot(ps_formula, data = data_1,
          weights = w_list[c("Nearest_baseline","Entropy","Mahalanobis")],
          stats = "mean.diffs", s.d.denom = "treated",
          thresholds = c(m = .1), abs = TRUE, drop.distance = TRUE)
ggsave(lp, filename = "C:/Users/victo/Box/Information_and_inbreeding/figures/balance_plot.png")
