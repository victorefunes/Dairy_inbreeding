library(tidyverse)
library(broom)
library(caret)
library(forecast)

yob_start <- 1997
yob_end <- 2005
yob_prg <- 2010
ratio <- 50
perc <- "p95"

pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]
setwd(paste0(folder, "/Box/Dairy_inbreeding/code"))

source("generate_db.R")
library(fixest)

data_full |>
  mutate(dob = ymd(dob),
         quarter = case_when(
           month(dob) %in% c(1,2,3) ~ 1,
           month(dob) %in% c(4,5,6) ~ 2,
           month(dob) %in% c(7,8,9) ~ 3,
           month(dob) %in% c(10,11,12) ~ 4),
         qob = paste0(yob, "-", quarter)) ->
  data_full

data_full |> 
  group_by(treat, yob) |>
  summarise(N = n()) |>
  pivot_wider(id_cols = yob, names_from = "treat", values_from = "N") |>
  data.frame()

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+pta_pl+pta_dpr+pta_hcr+pta_ccr+
          pta_liv+pta_type+pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form+
          pta_milk:post+pta_fat_lb:post+
          pta_protein_lb:post+pta_scs:post+
          pta_pl:post+pta_dpr:post+pta_hcr:post+
          pta_ccr:post+pta_liv:post+pta_type:post+
          pta_gest_length:post+pta_heifer_liv:post+pta_efcalving:post+
          pta_mastitis:post+pta_metritis:post+pta_disp_abomasum:post+
          pta_ketosis:post+pta_r_placenta:post+pta_milk_fever:post+
          pta_stature:post+pta_strength:post+
          pta_dairy_form:post|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit4

fit4 |>
  tidy() |>
  filter(grepl("yob::", term)) |> 
  separate(term, into = c("temp", "year_alt"), sep = "::") |>
  select(-temp) |>
  separate(year_alt, into = c("yob", "temp"), sep = ":") |>
  select(-temp) |>
  mutate(cost_animal = 40.11*estimate,
         num_cattle = c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                         9.25, 9.3, 9.4, 9.4, 9.3, 9.35),
         total_cost = cost_animal*num_cattle) ->
  cost_estimation

# Base year: 2012
# Interest rate: 4.5%
cost_estimation |>
  mutate(yob = as.numeric(yob)) |>
  filter(yob > 2011) |>
  mutate(factor = (1+0.045)^(-(yob-2010)),
         cost = total_cost*factor) |>
  summarise(cost = sum(cost)) ->
  cost_1

## 23% interest rate
cost_estimation |>
  mutate(yob = as.numeric(yob)) |>
  mutate(factor = (1+0.23)^(-(yob-2010)),
         cost = total_cost*factor) |>
  summarise(cost = sum(cost)) ->
  cost_2

data_full |> 
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob, treat) |> 
  summarise(NM = mean(NM, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(id_cols = "yob", names_from = "treat", values_from = "NM",
              names_glue = "{.value}_{treat}") |>
  mutate(NM_diff = NM_1-NM_0,
         num_cattle = c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                           9.25, 9.3, 9.4, 9.4, 9.3, 9.35),
            total_NM = NM_diff*num_cattle) |>
  ungroup() ->
  nm_estimation

nm_estimation |>
  mutate(yob = as.numeric(yob)) |>
  filter(yob > 2011) |>
  mutate(factor = (1+0.045)^(-(yob-2010)),
         benefit = total_NM*factor) |>
  summarise(benefit = sum(benefit)) ->
  benefit_1

nm_estimation |>
  mutate(yob = as.numeric(yob)) |>
  filter(yob > 2011) |>
  mutate(factor = (1+0.23)^(-(yob-2010)),
         benefit = total_NM*factor) |>
  summarise(benefit = sum(benefit)) ->
  benefit_2





## Total cost estimation 
# average inbreeding rates by group (treatment vs. control)
# cost per animal = 40.11 * (F_treat - F_control)
data_full |> 
  mutate(treat = factor(treat, labels = c("control", "treatment"))) |>
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob, treat) |> 
  summarise(inbreeding = mean(inbreeding, na.rm = TRUE)) |> 
  pivot_wider(id_cols = "yob", names_from = "treat", values_from = "inbreeding") |>
  mutate(cost_animal = 40.11*(treatment-control)) ->
  cost

cost$num_cattle <- c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                     9.25, 9.3, 9.4, 9.4, 9.3, 9.35)

cost |>
  mutate(total_cost = cost_animal*num_cattle) ->
  cost

sum(cost[cost$yob > 2011, "total_cost"])

## 5% interest rate

# Alternative calculation: difference between observed inbreeding and 6.25
data_full |> 
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob) |> 
  summarise(inbreeding_mean = mean(inbreeding, na.rm = TRUE)) |>
  mutate(cost_animal_alt = 40.11*(inbreeding_mean - 6.25)) ->
  cost_alt

cost <- left_join(cost, cost_alt, by = "yob")

cost |>
  mutate(total_cost_alt = cost_animal_alt*num_cattle) ->
  cost

# Base year: 2012
# Interest rate: 4.5%
cost |>
  ungroup() |>
  filter(yob > 2011) |>
  mutate(factor = (1+0.045)^(-(yob-2010)),
         cost_1 = total_cost*factor,
         cost_2 = total_cost_alt*factor) |>
  summarise(cost_1 = sum(cost_1),
            cost_2 = sum(cost_2))

## 23% interest rate
cost |>
  ungroup() |>
  filter(yob > 2011) |>
  mutate(factor = (1+0.23)^(-(yob-2010)),
         cost_1 = total_cost*factor,
         cost_2 = total_cost_alt*factor) |>
  summarise(cost_1 = sum(cost_1),
            cost_2 = sum(cost_2))

write_csv(cost, file = "../data/cost.csv")

#sum(cost_alt[cost_alt$yob > 2011, "total_cost_alt"])

## Train Net merit ARIMA model with full sample data
query <- "SELECT
  country,
  id_number,
  name,
  primary_stud_code,
  sire_id,
  mgs_id,
  period,
  status,
  naab_code,
  pta_milk_unadj as pta_milk,
  pta_fat_lb_unadj as pta_fat_lb,
  pta_protein_lb_unadj as pta_protein_lb,
  pta_scs_unadj as pta_scs,
  pta_pl_unadj as pta_pl,
  pta_dpr_unadj as pta_dpr,
  pta_hcr_unadj as pta_hcr,
  pta_ccr_unadj as pta_ccr,
  pta_liv_unadj as pta_liv,
  pta_ssb_unadj as pta_ssb,
  pta_dsb_unadj as pta_dsb,
  pta_type_unadj as pta_type,
  pta_stature_unadj as pta_stature,
  pta_strength_unadj as pta_strength,
  pta_body_depth_unadj as pta_body_depth,
  pta_dairy_form_unadj as pta_dairy_form,
  pta_rump_angle_unadj as pta_rump_angle,
  pta_thurl_width_unadj as pta_thurl_width,
  pta_rear_legs_side_unadj as pta_rear_legs_side,
  pta_rear_legs_rear_unadj as pta_rear_legs_rear,
  pta_foot_angle_unadj as pta_foot_angle,
  pta_foot_leg_score_unadj as pta_foot_leg_score,
  pta_fore_udder_unadj as pta_fore_udder,
  pta_rear_udder_height_unadj as pta_rear_udder_height,
  pta_rear_udder_width_unadj as pta_rear_udder_width,
  pta_udder_cleft_unadj as pta_udder_cleft,
  pta_udder_depth_unadj as pta_udder_depth,
  pta_teat_front_place_unadj as pta_teat_front_place,
  pta_teat_rear_place_unadj as pta_teat_rear_place,
  pta_teat_length_unadj as pta_teat_length,
  pta_mobility,
  pta_gest_length_unadj as pta_gest_length,
  pta_ketosis_unadj as pta_ketosis,
  pta_metritis_unadj as pta_metritis,
  pta_mastitis_unadj as pta_mastitis,
  pta_milk_fever_unadj as pta_milk_fever,
  FM_unadj as FM,
  CM_unadj as CM,
  GM_unadj as GM,
  NM_unadj as NM,
  TPI,
  udder_composite, 
  body_weight_composite,
  feet_legs_composite,
  aaa_rating,
  daughts_in_milk_pta,
  dob,
  semen_price
FROM
  CDCB_data.NAAB_AISS
WHERE 
  breed='HO'
"

naab <- bq_project_query(projectid, query)
naab <- bq_table_download(naab)

# Data Preparation
naab <- naab %>%
  separate(period, into = c("year", "per_num"), sep = "-", remove = FALSE)

## Some bulls have missing status
naab %>% 
  filter(is.na(status) == TRUE) %>%
  mutate(ndaugh = ifelse(daughts_in_milk_pta > 0, 1, 0)) %>%
  group_by(country, ndaugh) %>%
  tab(status)

naab <- naab |>
  mutate(status_new = case_when(
    status == "A" ~ "active",
    status == "F" ~ "foreign",
    status == "G" ~ "genomic"), 
    status_new = replace(status_new, is.na(status_new) & 
                           !(country %in% c("840", "USA")), "foreign"),
    status_new = replace(status_new, is.na(status_new)& 
                           (country %in% c("840", "USA")), "active"))

naab |>
  separate(period, into = c("year", "per_num"), sep = "-", remove = FALSE) |>
  mutate(dob = ymd(dob),
         yob = year(dob)) ->
  naab

naab |>
  filter(yob > 1984) |>
  group_by(yob) |>
  summarise(NM = mean(NM, na.rm = TRUE)) ->
  NM

NM_alt <- window(NM_ts, start = 1985, end = 2010)
auto.arima(NM_alt)

NM_alt_arima <- Arima(NM_alt, order = c(1, 1, 0), include.drift = TRUE)
NM_for <- forecast(NM_alt_arima, h = 10)

# Sample NM
data_full |>
  filter(yob > 2001 & yob < 2011) |> 
  group_by(yob) |> 
  summarise(NM_unadj = mean(NM_unadj, na.rm = TRUE)) ->
  NM_sample

NM_sample <- ts(NM_sample$NM_unadj, start = 2002, frequency = 1)
auto.arima(NM_sample)

NM_arima <- Arima(NM_sample, order = c(0, 1, 0), include.drift = TRUE)
NM_sample_for <- forecast(NM_arima, h = 10)

NM_for |> 
  data.frame() |>
  select(c(Point.Forecast, Lo.95, Hi.95)) |>
  rename(NM_forecast = Point.Forecast,
         NM_low = Lo.95,
         NM_high = Hi.95) |>
  rownames_to_column(var = "yob") |>
  mutate(yob = as.numeric(yob)) ->
  NM_for

NM |>
  filter(yob > 1990) |>
  ggplot(aes(x = yob, y = NM)) + 
  geom_line(color = "gold") +
  geom_line(data = NM_for, aes(x = yob, y = NM_forecast), color = "black") +
  geom_point(data = NM_for, aes(x = yob, y = NM_forecast), color = "black") +
  geom_ribbon(data = NM_for, aes(x = yob, y = NM_forecast, 
                                 ymin = NM_low, ymax = NM_high), 
              fill = "grey", alpha = 0.25) +
  geom_point(color = "gold") +
  geom_vline(xintercept = 2009, linetype = 2) +
  theme_bw() + 
  xlab("Year of birth") + ylab("Average Net Merit")


data_full |>
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob) |> 
  summarise(NM_unadj = mean(NM_unadj, na.rm = TRUE)) ->
  NM_est

NM_est$num_cattle <- c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                9.25, 9.3, 9.4, 9.4, 9.3, 9.35)
NM_est |>
  mutate(factor = (1+0.045)^(-(yob-2010))) ->
  NM_est

NM_est |>
  filter(yob > 2011) |>
  mutate(NM = 0.5*NM_unadj*num_cattle*factor) |>
  summarise(NM = sum(NM))

## 23% interest rate
NM_est |>
  mutate(factor = (1+0.23)^(-(yob-2010))) |>
  filter(yob > 2011) |>
  mutate(NM = 0.5*NM_unadj*num_cattle*factor) |>
  summarise(NM = sum(NM))



NM_est |>
  filter(yob > 2008) |>
  mutate(NM_for = 26.6691+0.7373*dplyr::lag(NM_unadj))
  
data_full |>
  mutate(treat = factor(treat, labels = c("control", "treatment"))) |>
  filter(yob > 2004) |>
  group_by(yob, treat) |>
  summarise(NM = mean(NM, na.rm = TRUE),
            NM_unadj = mean(NM_unadj, na.rm = TRUE)) |>
  pivot_longer(NM:NM_unadj, names_to = "type", values_to = "NM") |>
  ggplot(aes(x = yob, y = NM, group = treat)) +
  geom_line(aes(color = treat)) +
  geom_point(aes(color = treat)) +
  theme_bw() + 
  facet_wrap(.~type, scales = "free_y")
