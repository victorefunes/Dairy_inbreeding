# =====================================================================
# Integrating alternative matching estimators (caliper, IPW) into the
# DiD pipeline. Spans TWO files because generate_db_rr.R's final rm()
# destroys the panel-build inputs.
#
#   PART A -> paste into generate_db_rr.R (build the alt panels BEFORE rm)
#   PART B -> the one-line edit to generate_db_rr.R's rm() call
#   PART C -> paste into DiD_models_new.R (run models + comparison)
# =====================================================================
library(WeightIt)
library(tidyverse)

setwd("C:/Users/victo/Documents/Dairy_inbreeding/code")
source("generate_db_match.R")  # builds data_full
# #####################################################################
# PART A  --  generate_db_rr.R
# Place AFTER match_data_1 is created and BEFORE the final rm(...).
# #####################################################################

# --- alternative founder-level estimators ----------------------------
ps_formula <- match_data_1$formula

m_cal <- matchit(ps_formula, data = data_1, method = "nearest",
                 distance = "glm", link = "logit", replace = TRUE,
                 ratio = ratio, caliper = 0.2, discard = "none")

w_ipw <- weightit(ps_formula, data = data_1, method = "glm", estimand = "ATT")

# --- refactor YOUR existing panel build (lines ~476-648) into a function
#     Identical logic to your baseline; only the match object is a parameter.
build_data_full <- function(m_obj) {

  m_obj$match.matrix |>
    data.frame() |>
    rownames_to_column(var = "superstar") |>
    pivot_longer(-superstar, names_to = "match", values_to = "alt_sire") |>
    filter(!is.na(alt_sire)) |>
    separate(match, into = c("X", "match_num"), sep = 1) |>
    select(-X) -> rel_df

  match.data(m_obj, distance = "pscore") |>
    select(reg_id, pscore) |>
    rownames_to_column(var = "num_id") -> idx_pscore
  rel_df <- left_join(rel_df, idx_pscore, by = join_by("alt_sire" == "num_id"))

  data_1 |>
    rownames_to_column(var = "index") |>
    filter(superstar == 1) |>
    select(reg_id, index) -> idx_super
  rel_df <- left_join(rel_df, idx_super, by = join_by("superstar" == "index")) |>
    rename(reg_id = reg_id.y, reg_id_alt = reg_id.x) |>
    select(-c(superstar, alt_sire))

  naab_line |>
    filter(sire_id %in% rel_df$reg_id_alt) |>
    mutate(weight = 2^(-gen_num)) |>
    select(reg_id, sire_id, weight, gen_num) -> control
  control <- left_join(control, naab_old, by = "reg_id")
  control <- left_join(control, cdn[, c("reg_id", "inbreeding")], by = "reg_id")
  control <- left_join(control, rel_df[, c("reg_id_alt", "pscore")],
                       by = join_by("sire_id" == "reg_id_alt"))
  control <- distinct(control, reg_id, .keep_all = TRUE)

  naab_line |>
    filter(sire_id %in% rel_df$reg_id) |>
    mutate(weight = 2^(-gen_num)) |>
    select(reg_id, sire_id, weight, gen_num) -> treat
  treat <- left_join(treat, naab_old, by = "reg_id")
  treat <- left_join(treat, cdn, by = "reg_id")

  df <- rbind(treat   |> mutate(treat = 1),
              control |> select(-pscore) |> mutate(treat = 0))

  df <- left_join(df, inb_alt, by = "reg_id") |>
    mutate(inbreeding = ifelse(is.na(inbreeding.x), inbreeding.y, inbreeding.x)) |>
    select(-c(inbreeding.x, inbreeding.y))

  df <- left_join(df, companies,
                  by = join_by(controller_number == primary_stud_code)) |>
    mutate(parent_company = replace(parent_company, is.na(parent_company), "Other"))

  naab_aiss |>
    group_by(reg_id) |> arrange(period) |> slice_head(n = 1) |> ungroup() |>
    select(reg_id, NM_unadj) -> NM_df
  df <- left_join(df, NM_df, by = "reg_id")
  df
}

# --- IPW panel: controls are ALL alternative founders, weighted -------
#     (no match.matrix; carry founder IPW weight to descendants)
build_data_full_ipw <- function(w_obj) {
  fw <- data_1 |>
    mutate(fw = w_obj$weights) |>
    select(reg_id, superstar, fw)

  ctrl_founders <- fw |> filter(superstar == 0) |> pull(reg_id)
  trt_founders  <- fw |> filter(superstar == 1) |> pull(reg_id)

  build_side <- function(founders, tval) {
    naab_line |>
      filter(sire_id %in% founders) |>
      mutate(weight = 2^(-gen_num)) |>
      select(reg_id, sire_id, weight, gen_num) |>
      left_join(naab_old, by = "reg_id") |>
      left_join(cdn[, c("reg_id", "inbreeding")], by = "reg_id") |>
      left_join(fw, by = join_by("sire_id" == "reg_id")) |>
      distinct(reg_id, .keep_all = TRUE) |>
      mutate(treat = tval)
  }
  df <- rbind(build_side(trt_founders, 1), build_side(ctrl_founders, 0))

  df <- left_join(df, inb_alt, by = "reg_id") |>
    mutate(inbreeding = ifelse(is.na(inbreeding.x), inbreeding.y, inbreeding.x)) |>
    select(-c(inbreeding.x, inbreeding.y))
  df <- left_join(df, companies,
                  by = join_by(controller_number == primary_stud_code)) |>
    mutate(parent_company = replace(parent_company, is.na(parent_company), "Other"))
  # combined weight = founder IPW * generational; treated founders fw = 1
  df |> mutate(w_ipw = fw * weight)
}

# --- build the three panels (baseline stays as your existing data_full)
data_full_cal <- build_data_full(m_cal)
data_full_ipw <- build_data_full_ipw(w_ipw)


# #####################################################################
# PART B  --  generate_db_rr.R : edit the final rm()
# KEEP the new panels alive. Remove data_full_cal / data_full_ipw from
# whatever you rm, i.e. DO NOT delete: data_full, data_full_cal,
# data_full_ipw. (Your current rm() line does not list them, so just
# make sure the two new objects are not added to it.)
# #####################################################################


# #####################################################################
# PART C  --  DiD_models_new.R  (paste after fit4 / your Table 2 block)
# #####################################################################

library(fixest); library(dplyr); library(tibble); library(ggplot2)

pta_flt <- c("pta_milk","pta_fat_lb","pta_protein_lb","pta_scs","pta_pl","pta_dpr",
             "pta_hcr","pta_ccr","pta_liv","pta_type","pta_gest_length",
             "pta_heifer_liv","pta_efcalving","pta_mastitis","pta_metritis",
             "pta_disp_abomasum","pta_ketosis","pta_r_placenta","pta_milk_fever",
             "pta_stature","pta_strength","pta_dairy_form")

prep <- function(df) df |>
  filter(if_all(all_of(pta_flt), ~ !is.na(.)), inbreeding >= 0, yob > 2004, yob < 2020) |>
  mutate(post = factor(ifelse(yob > 2009, 1, 0)))

# 2x2 ATT (Model 1); w = optional weight column name (for IPW)
att_2x2 <- function(df, w = NULL) {
  d <- prep(df)
  args <- list(fml = inbreeding ~ i(post, treat, ref = 0) | treat + post,
               data = d, cluster = ~ sire_id)
  if (!is.null(w)) args$weights <- reformulate(w)   # ~w_ipw ; omitted otherwise
  f <- do.call(feols, args)
  cf <- coef(f)["post::1:treat"]; ci <- confint(f)["post::1:treat", ]
  tibble(att = cf, lo = ci[[1]], hi = ci[[2]])
}

# event study (Model 2)
att_event <- function(df, w = NULL) {
  d <- prep(df)
  args <- list(fml = inbreeding ~ i(yob, treat, ref = 2009) | treat + yob,
               data = d, cluster = ~ sire_id)
  if (!is.null(w)) args$weights <- reformulate(w)   # ~w_ipw ; omitted otherwise
  f <- do.call(feols, args)             
  cf <- coef(f); ci <- confint(f)
  k <- grepl("^yob::[0-9]{4}:treat$", names(cf))
  tibble(yob = as.integer(sub("yob::([0-9]{4}):treat", "\\1", names(cf)[k])),
         est = cf[k], lo = ci[k, 1], hi = ci[k, 2])
}

# --- ATT comparison table --------------------------------------------
att_tab <- bind_rows(
  att_2x2(data_full)                   |> mutate(method = "Baseline (NN)"),
  att_2x2(data_full_cal)               |> mutate(method = "Caliper"),
  att_2x2(data_full_ipw, w = "w_ipw")  |> mutate(method = "IPW")
)
print(att_tab)

# --- event-study overlay figure --------------------------------------
ev_tab <- bind_rows(
  att_event(data_full)                  |> mutate(method = "Baseline (NN)"),
  att_event(data_full_cal)              |> mutate(method = "Caliper"),
  att_event(data_full_ipw, w = "w_ipw") |> mutate(method = "IPW")
)

print(ev_tab)

ev_plot <- ggplot(ev_tab, aes(yob, est, color = method, group = method)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 2009, linetype = 3) +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.6)) +
  labs(x = "Year of birth", y = "Inbreeding rate coefficient (95% CI)",
       color = "Matching") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(family = "Palatino"))
ggsave(ev_plot, filename = "C:/Users/victo/Box/Information_and_inbreeding/figures/ev_plot.png",
       width = 7.5, height = 5, units = "in", dpi = 300)