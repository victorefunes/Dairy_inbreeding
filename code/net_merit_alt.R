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
  breed,
  period,
  status,
  naab_code,
  pta_milk,
  pta_fat_lb,
  pta_protein_lb,
  pta_pl,
  pta_scs,
  pta_dpr,
  pta_hcr,
  pta_ccr,
  pta_liv,
  pta_milk_fever,
  pta_da as pta_disp_abomasum,
  pta_ketosis,
  pta_metritis,
  pta_mastitis,
  pta_rp as pta_r_placenta,
  pta_efc as pta_efcalving,
  pta_heifer_liv,
  NM,
  NM_unadj,
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
  mutate(id_number = stringr::str_replace(id_number, "\\.", ""),
         id_number = str_pad(id_number, width = 12, side = "left", pad = "0"),
         reg_id = paste0(breed, country, id_number)) ->
  naab

# Download inbreeding data
query <- "SELECT
  reg_id, 
  inbreeding
FROM
  CDN_data.CDN_bulls
"
cdn <- bq_project_query(projectid, query)
cdn <- bq_table_download(cdn)

naab <- left_join(naab, cdn, by = "reg_id")

# Remove inbreeding correction 
# PTA(adj) = PTA(raw)-0.5*b*F
# b: estimated phenotypic regression per 1% inbreeding
# pta_milk         -74
# pta_fat_lb       -2.79
# pta_protein_lb	 -2.13
# pta_pl:          -0.28
# pta_scs           0.01
# pta_dpr          -0.23
# pta_hcr          -0.23
# pta_ccr          -0.33
# pta_liv          -0.11
# pta_milk_fever   -0.01
# pta_disp_abomasum 0.01
# pta_ketosis       0.01  
# pta_mastitis     -0.02
# pta_metritis     -0.09
# pta_r_placenta   -0.02
# pta_efcalving    -0.64
# pta_heifer_liv   -0.23

naab |>
  mutate(pta_milk_raw = pta_milk+0.5*(-74)*inbreeding,
         pta_fat_lb_raw = pta_fat_lb+0.5*(-2.79)*inbreeding,
         pta_protein_lb_raw = pta_protein_lb+0.5*(-2.13)*inbreeding,
         pta_pl_raw = pta_pl+0.5*(-0.28)*inbreeding,
         pta_scs_raw = pta_scs+0.5*0.01*inbreeding,
         pta_dpr_raw = pta_dpr+0.5*(-0.23)*inbreeding,
         pta_hcr_raw = pta_hcr+0.5*(-0.23)*inbreeding,
         pta_ccr_raw = pta_ccr+0.5*(-0.33)*inbreeding,
         pta_liv_raw = pta_liv+0.5*(-0.11)*inbreeding,
         pta_milk_fever_raw = pta_milk_fever+0.5*(-0.01)*inbreeding,
         pta_disp_abomasum_raw = pta_disp_abomasum+0.5*(0.01)*inbreeding,
         pta_ketosis_raw = pta_ketosis+0.5*(0.01)*inbreeding,
         pta_mastitis_raw = pta_mastitis+0.5*(-0.02)*inbreeding,
         pta_metritis_raw = pta_metritis+0.5*(-0.09)*inbreeding,
         pta_r_placenta_raw = pta_r_placenta+0.5*(-0.02)*inbreeding,
         pta_efcalving_raw = pta_efcalving+0.5*(-0.64)*inbreeding,
         pta_heifer_liv_raw = pta_heifer_liv+0.5*(-0.23)*inbreeding) ->
  naab

# Recalculate NM using 2010 weights
# pta_milk:           -0.006
# pta_fat_lb:         3.22
# pta_protein_lb:     4.14
# pta_pl:             29
# pta_scs:            -122
# udder_composite:      31
# feet_legs_composite:    10
# body_weight_composite: -16
# pta_dpr: 11
# pta_hcr: 2.3
# pta_ccr: 2.2
# pta_efcalving: 1

naab |>
  mutate(NM_alt = 0.02*pta_milk_raw+5.01*pta_fat_lb_raw+3.33*pta_protein_lb_raw+
           30*pta_pl_raw-74*pta_scs_raw+6*pta_dpr_raw+1.5*pta_hcr_raw+
           4.3*pta_ccr_raw+14.3*pta_liv_raw+1.19*pta_milk_fever_raw+
           6.91*pta_disp_abomasum_raw+0.97*pta_ketosis_raw+
           2.65*pta_mastitis_raw+3.94*pta_metritis_raw+2.38*pta_r_placenta_raw+
           2*pta_efcalving_raw+8.2*pta_heifer_liv_raw) ->
  naab

naab |>
  filter(yob > 1984) |>
  group_by(yob) |>
  summarise(NM_alt = mean(NM_alt, na.rm = TRUE),
            NM_unadj = mean(NM_unadj, na.rm = TRUE),
            NM = mean(NM, na.rm = TRUE)) ->
  NM

NM_ts <- ts(NM$NM, start = 1985, frequency = 1)
auto.arima(NM_ts)
NM_arima <- Arima(NM_ts, order = c(1, 0, 1))

NM_alt <- window(NM_ts, start = 1985, end = 2008)
auto.arima(NM_alt)

NM_alt_arima <- Arima(NM_alt, order = c(1, 1, 0), include.drift = TRUE)
NM_for <- forecast(NM_alt_arima, h = 12)

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
