# =====================================================================
# Cluster bootstrap for the popularity-premium benefit-cost ratio.
#
# Resamples LINES (sire_id, the clustering unit) with replacement and
# recomputes, per replicate:
#   cost_NPV    = sum_t disc_t * 40.11 * (inb_treat - inb_ctrl)_t * N_t
#   benefit_NPV = sum_t disc_t *        (NM_treat  - NM_ctrl)_t  * N_t
#   B/C, net
# on framing B (N_t = national herd), which is the economically correct
# base and makes B/C invariant to pop_share. Reports point estimate
# (full sample) + 95% percentile CI.
#
# NOTE: uses the RAW per-year treatment-control differential in inbreeding
# (not the Model-4 covariate-adjusted beta_hat) so cost and benefit are
# constructed identically. The point B/C will therefore differ slightly
# from the Model-4-based headline; report this bootstrap as the CI around
# the differential comparison.
#
# Run AFTER cost_recompute_R2.R (needs: tab, data_full, disc).
# =====================================================================

library(dplyr)

stopifnot(exists("tab"), exists("data_full"), exists("disc"))

set.seed(20240602)
B          <- 2000
nm_var     <- "NM_unadj"          # match the corrected benefit run
pop_share  <- 0.86                # sourced Holstein share (net magnitude only)
cost_pp    <- 40.11
cost_years <- tab$yob             # 2012:2019

pta_vars <- c("pta_milk","pta_fat_lb","pta_protein_lb","pta_scs","pta_pl",
              "pta_dpr","pta_hcr","pta_ccr","pta_liv","pta_type",
              "pta_gest_length","pta_heifer_liv","pta_efcalving","pta_mastitis",
              "pta_metritis","pta_disp_abomasum","pta_ketosis","pta_r_placenta",
              "pta_milk_fever","pta_stature","pta_strength","pta_dairy_form")

# ---- estimation frame + per-year national herd weights ---------------
df <- data_full |>
  filter(if_all(all_of(pta_vars), ~ !is.na(.)),
         inbreeding >= 0, yob %in% cost_years) |>
  select(sire_id, yob, treat, inbreeding, nm = all_of(nm_var))

herd <- tab |> select(yob, num_cattle_mil)
disc_lo <- disc[seq_len(length(cost_years))]
r_hi <- 0.23
disc_hi <- (1 / (1 + r_hi))^(seq_len(length(cost_years)))

# map each line to its row indices once (for fast cluster resampling)
idx_by_line <- split(seq_len(nrow(df)), df$sire_id)
lines <- names(idx_by_line)

# ---- one NPV evaluation on a given set of rows -----------------------
npv_set <- function(rows, disc_vec) {
  d <- df[rows, ]
  g <- d |>
    group_by(yob, treat) |>
    summarise(inb = mean(inbreeding, na.rm = TRUE), nm = mean(nm, na.rm = TRUE), .groups = "drop")
  # pivot to wide by treat
  w <- g |>
    tidyr::pivot_wider(names_from = treat, values_from = c(inb, nm),
                       names_sep = "_") |>
    right_join(herd, by = "yob") |>
    arrange(yob) |>
    mutate(inb_d = inb_1 - inb_0,
           nm_d  = nm_1  - nm_0,
           cost  = cost_pp * inb_d * num_cattle_mil * 1e6,
           ben   =           nm_d  * num_cattle_mil * 1e6)
  cN <- sum(w$cost * disc_vec, na.rm = TRUE)
  bN <- sum(w$ben  * disc_vec, na.rm = TRUE)
  c(cost = cN, benefit = bN)
}

# ---- point estimate (full sample) ------------------------------------
pt_lo <- npv_set(seq_len(nrow(df)), disc_lo)
pt_hi <- npv_set(seq_len(nrow(df)), disc_hi)
pt <- function(v, share) c(bc = v["benefit"]/v["cost"],
                           net = (v["benefit"] - v["cost"]) * share)

# ---- bootstrap -------------------------------------------------------
boot <- function(disc_vec) {
  out <- matrix(NA_real_, B, 2, dimnames = list(NULL, c("cost","benefit")))
  for (b in seq_len(B)) {
    draw <- sample(lines, length(lines), replace = TRUE)
    rows <- unlist(idx_by_line[draw], use.names = FALSE)
    out[b, ] <- npv_set(rows, disc_vec)
  }
  out
}
cat("Bootstrapping (", B, "reps)...\n")
Bs_lo <- boot(disc_lo)
Bs_hi <- boot(disc_hi)

summ <- function(Bs, ptv, share, label) {
  bc  <- Bs[,"benefit"] / Bs[,"cost"]
  net <- (Bs[,"benefit"] - Bs[,"cost"]) * share
  qbc  <- quantile(bc,  c(.025,.975), na.rm = TRUE)
  qnet <- quantile(net, c(.025,.975), na.rm = TRUE)
  cat(sprintf("\n[%s]  (pop_share = %.2f)\n", label, share))
  cat(sprintf("  cost NPV    = $%.2f B\n", ptv["cost"]*share/1e9))
  cat(sprintf("  benefit NPV = $%.2f B\n", ptv["benefit"]*share/1e9))
  cat(sprintf("  B/C  = %.3f   95%% CI [%.3f, %.3f]\n",
              ptv["benefit"]/ptv["cost"], qbc[1], qbc[2]))
  cat(sprintf("  net  = $%.2f B  95%% CI [$%.2f B, $%.2f B]\n",
              (ptv["benefit"]-ptv["cost"])*share/1e9, qnet[1]/1e9, qnet[2]/1e9))
  cat(sprintf("  Share of reps with B/C > 1: %.1f%%\n", 100*mean(bc > 1, na.rm=TRUE)))
}

summ(Bs_lo, pt_lo, pop_share, "r = 4.5%")
summ(Bs_hi, pt_hi, pop_share, "r = 23%")

cat("\nInterpretation: if the B/C CI contains 1 (equivalently the net CI\n",
    "contains 0), report the popularity premium as not distinguishable from\n",
    "break-even -- still consistent with the coordination-failure story.\n", sep="")
