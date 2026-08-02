# =====================================================================
# Recompute of inbreeding-cost NPV addressing R2's identification point.
#
# R2's objection: the ATT (beta_hat_t) is the DIFFERENTIAL inbreeding of
# popular-line descendants relative to matched controls. The original
# Appendix A code multiplies this differential by the ENTIRE national
# Holstein herd, which extrapolates a line-specific effect to animals it
# was never estimated on.
#
# This script produces two design-consistent figures instead:
#   (A) Popularity-premium cost  = differential ATT applied ONLY to the
#       treated (popular-line) population. This is what the DiD supports
#       directly. Clean, conservative, no extrapolation.
#   (B) Population-scaled cost    = differential ATT applied to the share
#       of the national herd descended from popular lines. Keeps an
#       industry-scale number but REQUIRES an explicit assumption
#       (pop_share) that you must defend. pop_share = 1 reproduces the
#       original (erroneous) whole-herd scaling for comparison.
#
# Run AFTER sourcing DiD_models_new.R (needs: fit4, data_full, disc, r).
# =====================================================================

library(tidyverse)

pwd <- getwd()
setwd(paste0(pwd, "/Documents/Dairy_inbreeding/code"))

source("DiD_models_new.R")  # loads fit4, data_full, disc, r

r <- 0.045

stopifnot(exists("fit4"), exists("data_full"), exists("disc"))

cost_per_pp <- 40.11   # CDCB: USD per 1 percentage-point inbreeding, per animal
cost_years  <- 2012:2019

# ---------------------------------------------------------------------
# 1. Extract Model (4) event-study coefficients beta_hat_t (matches text)
# ---------------------------------------------------------------------
cf <- coef(fit4)
bt <- cf[str_detect(names(cf), "^yob::[0-9]{4}:treat$")]

beta_df <- tibble(
  yob  = as.integer(str_replace(names(bt), "yob::([0-9]{4}):treat", "\\1")),
  beta = as.numeric(bt)               # in percentage points
) |>
  filter(yob %in% cost_years) |>
  arrange(yob)

# ---------------------------------------------------------------------
# 2. Treated-animal counts per year, on fit4's estimation sample
#    (replicate the exact filter chain so counts match the regression)
# ---------------------------------------------------------------------
pta_vars <- c("pta_milk","pta_fat_lb","pta_protein_lb","pta_scs","pta_pl",
              "pta_dpr","pta_hcr","pta_ccr","pta_liv","pta_type",
              "pta_gest_length","pta_heifer_liv","pta_efcalving","pta_mastitis",
              "pta_metritis","pta_disp_abomasum","pta_ketosis","pta_r_placenta",
              "pta_milk_fever","pta_stature","pta_strength","pta_dairy_form")

n_treated <- data_full |>
  filter(if_all(all_of(pta_vars), ~ !is.na(.)),
         inbreeding >= 0, yob > 2004, yob < 2020,
         treat == 1) |>
  count(yob, name = "n_treat_sample")

# ---------------------------------------------------------------------
# 3. National Holstein herd (millions of head), 2006-2019, from your code
# ---------------------------------------------------------------------
herd_df <- tibble(
  yob = 2006:2019,
  num_cattle_mil = c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233,
                     9.25, 9.3, 9.4, 9.4, 9.3, 9.35)
)

# ---------------------------------------------------------------------
# 4. Assemble and compute both framings
# ---------------------------------------------------------------------
pop_share <- 1.0   # <-- SET THIS for framing (B). 1.0 = original whole-herd
                   #     assumption. Replace with your defensible estimate of
                   #     the fraction of the national herd descended from
                   #     popular lines (or run a range as a sensitivity).

tab <- beta_df |>
  left_join(n_treated, by = "yob") |>
  left_join(herd_df,   by = "yob") |>
  mutate(
    cost_per_animal = cost_per_pp * beta,                       # USD/animal

    # (A) design-consistent: only the treated animals we observe
    cost_A = cost_per_animal * n_treat_sample,                  # USD

    # (B) population-scaled by assumed popular-line share of the herd
    cost_B = cost_per_animal * pop_share * num_cattle_mil * 1e6 # USD
  )

# discount factors: disc[1] applies to 2012, ..., disc[8] to 2019
disc_vec <- disc[seq_len(nrow(tab))]

npv_A <- sum(tab$cost_A * disc_vec, na.rm = TRUE)
npv_B <- sum(tab$cost_B * disc_vec, na.rm = TRUE)

# ---------------------------------------------------------------------
# 5. Report
# ---------------------------------------------------------------------
cat("\n--- Per-year detail (Model 4 coefficients) ---\n")
print(tab |>
        transmute(yob, beta,
                  cost_per_animal = round(cost_per_animal, 2),
                  n_treat_sample,
                  cost_A_musd = round(cost_A / 1e6, 2),
                  cost_B_musd = round(cost_B / 1e6, 1)))

cat("\n(A) Popularity-premium NPV (treated animals only), 2012 base:\n")
cat("    $", format(round(npv_A), big.mark = ","), "  (~$",
    round(npv_A / 1e6, 1), " million)\n", sep = "")

cat("\n(B) Population-scaled NPV, pop_share = ", pop_share,
    ", 2012 base:\n", sep = "")
cat("    $", format(round(npv_B), big.mark = ","), "  (~$",
    round(npv_B / 1e9, 2), " billion)\n", sep = "")

cat("\nNote: (B) with pop_share = 1 approximates the original Appendix A\n",
    "figure and is NOT defensible as written -- it treats the whole herd\n",
    "as popular-line descendants. Lower pop_share proportionally.\n", sep = "")

# ---------------------------------------------------------------------
# 6. (Optional) high-discount-rate variant, r = 0.23  (Wuepper et al.)
# ---------------------------------------------------------------------
r_hi <- 0.23
disc_hi <- (1 / (1 + r_hi))^(seq_len(nrow(tab)))
cat("\n(A) at r = 0.23:  $", format(round(sum(tab$cost_A * disc_hi)), big.mark = ","), "\n", sep = "")
cat("(B) at r = 0.23:  $", format(round(sum(tab$cost_B * disc_hi)), big.mark = ","), "\n", sep = "")
