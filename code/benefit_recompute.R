# =====================================================================
# Benefit-side NPV recompute, PARALLEL to cost_recompute_R2.R.
#
# Purpose: put benefit and cost on the IDENTICAL base so they are
# comparable and so pop_share cancels in the net sign / benefit-cost
# ratio (see the invariance note at the bottom).
#
# This computes the benefit as a treatment-control DIFFERENTIAL
#   Delta_NM_t = mean(NM | treat=1) - mean(NM | treat=0)
# which matches the paper's Appendix B text. NOTE: your current script
# instead averages NM_cor over ALL animals (a level, not a differential)
# and multiplies by the whole herd -- that is inconsistent with the
# Appendix B formula and breaks the invariance argument. Use this.
#
# Run AFTER cost_recompute_R2.R (needs: tab, data_full, disc).
#   tab must contain: yob, n_treat_sample, num_cattle_mil
# =====================================================================

library(dplyr)

stopifnot(exists("tab"), exists("data_full"), exists("disc"))

# Which Net Merit measure defines the benefit differential.
#   "NM"       = reported Net Merit (already USDA inbreeding-adjusted).
#   "NM_unadj" = unadjusted; use this if you want the cost term
#                (40.11 * beta) to be the ONLY inbreeding adjustment,
#                avoiding double-counting the inbreeding penalty.
# The choice changes the LEVEL but not the invariance property below.
nm_var <- "NM_unadj"

cost_years <- tab$yob   # 2012:2019

pta_vars <- c("pta_milk","pta_fat_lb","pta_protein_lb","pta_scs","pta_pl",
              "pta_dpr","pta_hcr","pta_ccr","pta_liv","pta_type",
              "pta_gest_length","pta_heifer_liv","pta_efcalving","pta_mastitis",
              "pta_metritis","pta_disp_abomasum","pta_ketosis","pta_r_placenta",
              "pta_milk_fever","pta_stature","pta_strength","pta_dairy_form")

# ---- per-year treatment/control means of NM on the estimation sample --
nm_by_group <- data_full |>
  filter(if_all(all_of(pta_vars), ~ !is.na(.)),
         inbreeding >= 0, yob %in% cost_years) |>
  group_by(yob, treat) |>
  summarise(nm = mean(.data[[nm_var]], na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = treat, values_from = nm,
                     names_prefix = "treat_") |>
  mutate(delta_nm = treat_1 - treat_0)     # benefit per treated animal

# ---- assemble on the SAME base as the cost recompute -----------------
ben <- tab |>
  select(yob, n_treat_sample, num_cattle_mil) |>
  left_join(nm_by_group |> select(yob, delta_nm), by = "yob") |>
  mutate(
    ben_A = delta_nm * n_treat_sample,                    # treated-only, USD
    ben_B = delta_nm * num_cattle_mil * 1e6               # per unit pop_share
  )

pop_share <- 1.0   # keep identical to the value used in cost_recompute_R2.R

disc_lo <- disc[seq_len(nrow(ben))]
r_hi    <- 0.23
disc_hi <- (1 / (1 + r_hi))^(seq_len(nrow(ben)))

npvB_ben <- function(d, share = pop_share) sum(ben$ben_B * share * d, na.rm = TRUE)
npvA_ben <- function(d)                    sum(ben$ben_A * d,         na.rm = TRUE)

# ---- pull the matching COST NPVs from tab (framing B, per unit share)-
#      (cost_per_animal * num_cattle_mil * 1e6, discounted)
cost_B_unit <- sum(tab$cost_per_animal * tab$num_cattle_mil * 1e6 * disc_lo, na.rm = TRUE)
cost_A_only <- sum(tab$cost_per_animal * tab$n_treat_sample * disc_lo,        na.rm = TRUE)

cat("\n--- Benefit differential by year (nm_var =", nm_var, ") ---\n")
print(ben |> transmute(yob, delta_nm = round(delta_nm, 1),
                       ben_A_musd = round(ben_A/1e6, 2),
                       ben_B_bshare = round(ben_B/1e9, 2)))

cat("\n(A) treated-only, r = 4.5%:\n")
cat(sprintf("    benefit = $%.2f M   cost = $%.2f M   net = $%.2f M   B/C = %.2f\n",
            npvA_ben(disc_lo)/1e6, cost_A_only/1e6,
            (npvA_ben(disc_lo)-cost_A_only)/1e6,
            npvA_ben(disc_lo)/cost_A_only))

cat("\n(B) population-scaled, r = 4.5%, pop_share =", pop_share, ":\n")
cat(sprintf("    benefit = $%.2f B   cost = $%.2f B   net = $%.2f B   B/C = %.2f\n",
            npvB_ben(disc_lo)/1e9, cost_B_unit*pop_share/1e9,
            (npvB_ben(disc_lo)-cost_B_unit*pop_share)/1e9,
            npvB_ben(disc_lo)/(cost_B_unit*pop_share)))

# ---- the invariance demonstration ------------------------------------
cat("\n--- B/C ratio is invariant to pop_share (framing B) ---\n")
for (s in c(0.3, 0.5, 0.7, 1.0)) {
  bc <- npvB_ben(disc_lo, s) / (cost_B_unit * s)
  net <- (npvB_ben(disc_lo, s) - cost_B_unit * s) / 1e9
  cat(sprintf("   pop_share = %.1f :  B/C = %.3f   net = $%.2f B\n", s, bc, net))
}
cat("   (B/C identical across rows; only the absolute net magnitude scales.)\n")
