library(tidyverse)
library(bigrquery)
library(statar)
library(stringi)
library(glmnet)
library(broom)
library(caret)
library(forecast)

setwd("C:/Users/victo/Box/dairy_inbreeding/code")

projectid <- "dairy-168114"
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

NM_ts <- ts(NM$NM, start = 1985, frequency = 1)
NM_arima <- Arima(NM_ts, order = c(1, 2, 1))

<<<<<<< HEAD
NM_alt <- window(NM_ts, start = 1985, end = 2008)
auto.arima(NM_alt)

NM_alt_arima <- Arima(NM_alt, order = c(1, 1, 0), include.drift = TRUE)
NM_for <- forecast(NM_alt_arima, h = 12)
=======
NM_alt <- window(NM_ts, start = 1985, end = 2009)
auto.arima(NM_alt)

NM_alt_arima <- Arima(NM_alt, order = c(1, 1, 0), include.drift = TRUE)
NM_for <- forecast(NM_alt_arima, h = 10)
>>>>>>> second-repo-remote/main

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

sum(NM[25:36,2] - NM_for[,2])/11

cb_df <- data.frame(year = 2009:2020,
                    NM = NM[25:36,2],
                    NM_alt = NM_for[,2], 
                    num_cattle = c(9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                                   9.25, 9.3, 9.4, 9.4, 9.3, 9.35))


cost <- read_csv("../data/cost.csv")

## Cost-benefit analysis
cb_df |> 
  mutate(tot_benefit = 0.5*num_cattle*(NM-NM_alt)) ->
  cb_df

sum(cb_df$tot_benefit)
cost |>
  mutate(total_cost = cost_animal*num_cattle) ->
  cost

cost |> 
  filter(yob > 2008) |>
  summarise(total_cost = sum(total_cost),
            total_cost_alt = sum(total_cost_alt))
