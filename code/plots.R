library(tidyverse)
library(bigrquery)
library(lubridate)
library(statar)
library(gganimate)
library(stringi)
library(glmnet)
library(broom)
library(ggrepel)
library(ggfortify)
library(caret)
library(tikzDevice)

setwd("C:/Users/victo/Box/dairy_matching/code/AISS data")

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

naab <- naab %>%
  mutate(status_new = case_when(
    status == "A" ~ "active",
    status == "F" ~ "foreign",
    status == "G" ~ "genomic"
  ))

naab$status_new <- replace(naab$status_new, (is.na(naab$status_new) == TRUE) & 
                             !(naab$country %in% c("840", "USA")), "foreign")
naab$status_new <- replace(naab$status_new, (is.na(naab$status_new) == TRUE) & 
                             (naab$country %in% c("840", "USA")), "active")

# PTA plot
tikz(file = "C:/Users/victo/Box/dairy_matching/figs/pta_milk.tex", width = 5, height = 5)
naab |>
  mutate(status_alt = ifelse(status_new %in% c("active", "foreign"), 1, 0),
         status_alt = factor(status_alt, labels = c("genomic proven", "daughter proven")),
         year =  ymd(paste(year, "01-01", sep = "-")),
         year_new = year(year)) |>
  filter(year_new %in% c(2004, 2009, 2013, 2018)) |>
  ggplot(aes(x = pta_milk, group = status_alt)) + 
  geom_density(aes(y = after_stat(density), fill = status_alt), alpha = 0.5) + 
  scale_fill_manual(name = "", values = c("#ffa500", "#87ceeb"))  + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '', x = 'PTA Milk (lbs)', y = 'Density') +
  facet_wrap(.~year_new, scales = "fixed")
dev.off()  