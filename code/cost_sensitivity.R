# =====================================================================
# pop_share sensitivity + LaTeX table for Appendix A.
# Run AFTER cost_recompute_R2.R (needs: tab, disc).
#   tab must contain: cost_per_animal, n_treat_sample, num_cattle_mil
# =====================================================================

library(tidyverse)

stopifnot(exists("tab"), exists("disc"))

cost_years <- tab$yob
disc_lo <- disc[seq_len(nrow(tab))]                 # r = 4.5%
r_hi    <- 0.23
disc_hi <- (1 / (1 + r_hi))^(seq_len(nrow(tab)))    # r = 23%

# ---- Framing (A): treated animals only (no extrapolation) -----------
npvA <- function(d) sum(tab$cost_per_animal * tab$n_treat_sample * d, na.rm = TRUE)
A_lo <- npvA(disc_lo)
A_hi <- npvA(disc_hi)

# ---- Framing (B): scaled by assumed popular-line share of herd ------
shares <- c(0.3, 0.4, 0.5, 0.6, 0.7)
npvB <- function(share, d)
  sum(tab$cost_per_animal * share * tab$num_cattle_mil * 1e6 * d, na.rm = TRUE)

sens <- tibble(
  pop_share = shares,
  npv_4.5   = vapply(shares, npvB, numeric(1), d = disc_lo),
  npv_23    = vapply(shares, npvB, numeric(1), d = disc_hi)
)

cat("\n=== Framing (A): popularity premium, treated animals only ===\n")
cat(sprintf("  r = 4.5%%:  $%.1f million\n", A_lo / 1e6))
cat(sprintf("  r = 23%% :  $%.1f million\n", A_hi / 1e6))

cat("\n=== Framing (B): population-scaled, by assumed popular-line share ===\n")
print(sens |> mutate(npv_4.5 = sprintf("$%.2f B", npv_4.5 / 1e9),
                     npv_23  = sprintf("$%.2f B", npv_23  / 1e9)))

# ---------------------------------------------------------------------
# LaTeX table (booktabs) for Appendix A
# ---------------------------------------------------------------------
fmt_b <- function(x) sprintf("%.2f", x / 1e9)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Net present value of the popularity-specific inbreeding cost, 2012--2019 (2012 base year)}",
  "\\label{tab:cost_npv}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Discount rate} \\\\",
  "\\cmidrule(lr){2-3}",
  "Scaling of treated population & $r = 4.5\\%$ & $r = 23\\%$ \\\\",
  "\\midrule",
  sprintf("Treated animals only (no extrapolation) & \\$%.1f M & \\$%.1f M \\\\",
          A_lo / 1e6, A_hi / 1e6),
  "\\addlinespace",
  "\\multicolumn{3}{l}{\\textit{Share of national herd descended from popular lines:}} \\\\",
  sprintf("\\quad 30\\%% & \\$%s B & \\$%s B \\\\", fmt_b(sens$npv_4.5[1]), fmt_b(sens$npv_23[1])),
  sprintf("\\quad 40\\%% & \\$%s B & \\$%s B \\\\", fmt_b(sens$npv_4.5[2]), fmt_b(sens$npv_23[2])),
  sprintf("\\quad 50\\%% & \\$%s B & \\$%s B \\\\", fmt_b(sens$npv_4.5[3]), fmt_b(sens$npv_23[3])),
  sprintf("\\quad 60\\%% & \\$%s B & \\$%s B \\\\", fmt_b(sens$npv_4.5[4]), fmt_b(sens$npv_23[4])),
  sprintf("\\quad 70\\%% & \\$%s B & \\$%s B \\\\", fmt_b(sens$npv_4.5[5]), fmt_b(sens$npv_23[5])),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.85\\textwidth}",
  "\\footnotesize \\vspace{4pt} Notes: Per-animal cost is $40.11 \\times \\hat{\\beta}_t$",
  "(Model 4). ``Treated animals only'' applies the differential to the popular-line",
  "descendants in our sample and involves no extrapolation. The population-scaled rows",
  "apply the same differential to the stated fraction of the national Holstein herd and",
  "should be read as illustrative under an assumed share.",
  "\\end{minipage}",
  "\\end{table}"
)

out_tex <- file.path("../tables/cost_npv_table.tex")
writeLines(tex, out_tex)
cat("\nLaTeX table written to:", normalizePath(out_tex), "\n")
