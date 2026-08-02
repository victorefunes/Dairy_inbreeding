# Load packages
options(warn=-1)
options(scipen=999)
library(tidyverse)
library(bigrquery)
library(lubridate)
library(statar)
library(cobalt)
library(Hmisc)
library(stringi)
library(broom)
library(MatchIt)

bq_auth(email = "vf006@uark.edu")

# Download pedigree data
projectid <- "dairy-168114"
query <- "
SELECT
    reg_id,
    id_number,
    breed,
    country,
    primary_naab_code,
    naab_codes,
    reg_name,
    birthdate,
    semen_release_date,
    genotype_information,
    sire_breed,
    sire_country,
    sire_id_number,
    dam_breed,
    dam_country,
    dam_id_number,
    mgs_breed,
    mgs_country,
    mgs_id_number,
    status
FROM
    CDCB_data.NAAB_Info
"

naab <- bq_project_query(projectid, query)
naab <- bq_table_download(naab)

# Convert birthrate dta to date format
naab <- naab |>
  mutate(birthdate = mdy(birthdate),
         yob = year(birthdate))

# Parse IDs of bulls, sires and dams
naab <- naab |>
  mutate(id_number = stringr::str_replace(id_number, "\\.", ""),
         id_number = str_pad(id_number, width = 12, side = "left", pad = "0"),
<<<<<<< HEAD
         reg_id = paste0(breed, country, id_number),
         sire_id_number = stringr::str_replace(sire_id_number, "\\.", ""),
         sire_id_number = str_pad(sire_id_number, width = 12, side = "left", pad = "0"),
         sire_id = paste0(sire_breed, sire_country, sire_id_number),
         sire_id = replace(sire_id, sire_id == "NANANA", NA),
         mgs_id_number = stringr::str_replace(mgs_id_number, "\\.", ""),
         mgs_id_number = str_pad(mgs_id_number, width = 12, side = "left", pad = "0"),
         mgs_id = paste0(mgs_breed, mgs_country, mgs_id_number),
         mgs_id = replace(mgs_id, mgs_id == "NANANA", NA),
=======
         reg_id = paste0(breed, country, id_number), 
         sire_id_number = stringr::str_replace(sire_id_number, "\\.", ""),
         sire_id_number = str_pad(sire_id_number, width = 12, side = "left", pad = "0"),
         sire_id = paste0(sire_breed, sire_country, sire_id_number),
         sire_id = replace(sire_id, sire_id == "NANANA", NA), 
         mgs_id_number = stringr::str_replace(mgs_id_number, "\\.", ""),
         mgs_id_number = str_pad(mgs_id_number, width = 12, side = "left", pad = "0"),
         mgs_id = paste0(mgs_breed, mgs_country, mgs_id_number),
         mgs_id = replace(mgs_id, mgs_id == "NANANA", NA), 
>>>>>>> second-repo-remote/main
         dam_id_number = stringr::str_replace(dam_id_number, "\\.", ""),
         dam_id_number = str_pad(dam_id_number, width = 12, side = "left", pad = "0"),
         dam_id = paste0(dam_breed, dam_country, dam_id_number),
         dam_id = replace(dam_id, dam_id == "NANANA", NA))

# Download evaluations data
query <- "SELECT
  country,
  id_number,
  name,
  primary_stud_code,
  sire_id,
  mgs_id,
  period,
  status,
  breed,
  naab_code,
  pta_milk as pta_milk,
  pta_fat_lb as pta_fat_lb,
  pta_protein_lb as pta_protein_lb,
  pta_scs as pta_scs,
  pta_pl as pta_pl,
  pta_dpr as pta_dpr,
  pta_hcr as pta_hcr,
  pta_ccr as pta_ccr,
  pta_liv as pta_liv,
  pta_ssb as pta_ssb,
  pta_dsb as pta_dsb,
  pta_type as pta_type,
  pta_stature as pta_stature,
  pta_strength as pta_strength,
  pta_body_depth as pta_body_depth,
  pta_dairy_form as pta_dairy_form,
  pta_rump_angle as pta_rump_angle,
  pta_thurl_width as pta_thurl_width,
  pta_rear_legs_side as pta_rear_legs_side,
  pta_rear_legs_rear as pta_rear_legs_rear,
  pta_foot_angle as pta_foot_angle,
  pta_foot_leg_score as pta_foot_leg_score,
  pta_fore_udder as pta_fore_udder,
  pta_rear_udder_height as pta_rear_udder_height,
  pta_rear_udder_width as pta_rear_udder_width,
  pta_udder_cleft as pta_udder_cleft,
  pta_udder_depth as pta_udder_depth,
  pta_teat_front_place as pta_teat_front_place,
  pta_teat_rear_place as pta_teat_rear_place,
  pta_teat_length as pta_teat_length,
  pta_mobility,
  pta_gest_length as pta_gest_length,
  pta_ketosis as pta_ketosis,
  pta_metritis as pta_metritis,
  pta_mastitis as pta_mastitis,
  pta_milk_fever as pta_milk_fever,
  FM as FM,
  CM as CM,
  GM as GM,
  NM as NM,
  NM_unadj as NM_unadj,
  TPI,
<<<<<<< HEAD
  udder_composite,
=======
  udder_composite, 
>>>>>>> second-repo-remote/main
  body_weight_composite,
  feet_legs_composite,
  aaa_rating,
  daughts_in_milk_pta,
  dob,
  semen_price
FROM
  CDCB_data.NAAB_AISS
"

naab_aiss <- bq_project_query(projectid, query)
naab_aiss <- bq_table_download(naab_aiss)

# Parse IDs and dates of birth
naab_aiss <- naab_aiss |>
  separate(period, into = c("year", "per_num"), sep = "-", remove = FALSE) |>
  mutate(dob = ymd(dob),
<<<<<<< HEAD
         yob = year(dob),
=======
         yob = year(dob), 
>>>>>>> second-repo-remote/main
         id_number = stringr::str_replace(id_number, "\\.", ""),
         id_number = str_pad(id_number, width = 12, side = "left", pad = "0"),
         reg_id = paste0(breed, country, id_number))

# Download "old" data
query <- "SELECT
<<<<<<< HEAD
  reg_id,
  reg_name,
  country,
  breed,
  naab_code,
  status,
  controller_name,
  controller_number,
  dob,
  yob,
  aaa_codes,
  genetic_codes,
  beta_casein,
  kappa_casein,
  beta_lactaglobulin,
  haplotypes,
  num_dtrs,
  num_herds,
  reliability,
  pta_milk,
  pta_fat_lb,
  pta_protein_lb,
  pta_scs,
  pta_pl,
  pta_dpr,
  pta_hcr,
  pta_ccr,
  pta_scr,
  pta_liv,
  pta_type,
  pta_gest_length,
  pta_heifer_liv,
  pta_efcalving,
  pta_mastitis,
  pta_metritis,
  pta_disp_abomasum,
  pta_ketosis,
  pta_r_placenta,
  pta_rear_legs_rear,
  pta_foot_leg_score,
  pta_teat_rear_place,
  pta_rump_angle,
  pta_thurl_width,
  pta_rear_legs_side,
  pta_foot_angle,
  pta_fore_udder,
  pta_rear_udder_height,
  pta_rear_udder_width,
  pta_udder_cleft,
  pta_udder_depth,
  pta_teat_front_place,
  pta_teat_length,
  pta_milk_fever,
  pta_stature,
  pta_strength,
  pta_mobility,
  pta_milk_speed,
  pta_dairy_form,
  pta_body_depth,
  sire_ce,
  daughter_ce,
  sire_sb,
  daughter_sb,
  udder_composite,
  NM,
  PTI,
  CM,
  FM,
  GM,
  GTPI,
  sire_name,
=======
  reg_id, 
  reg_name,
  country,
  breed,
  naab_code,            
  status, 
  controller_name,
  controller_number,
  dob, 
  yob, 
  aaa_codes, 
  genetic_codes,
  beta_casein, 
  kappa_casein,
  beta_lactaglobulin, 
  haplotypes, 
  num_dtrs, 
  num_herds, 
  reliability, 
  pta_milk, 
  pta_fat_lb, 
  pta_protein_lb, 
  pta_scs, 
  pta_pl, 
  pta_dpr, 
  pta_hcr, 
  pta_ccr, 
  pta_scr, 
  pta_liv, 
  pta_type,  
  pta_gest_length, 
  pta_heifer_liv,
  pta_efcalving,
  pta_mastitis, 
  pta_metritis, 
  pta_disp_abomasum, 
  pta_ketosis, 
  pta_r_placenta, 
  pta_rear_legs_rear,
  pta_foot_leg_score, 
  pta_teat_rear_place, 
  pta_rump_angle,
  pta_thurl_width,
  pta_rear_legs_side,
  pta_foot_angle, 
  pta_fore_udder, 
  pta_rear_udder_height,
  pta_rear_udder_width,
  pta_udder_cleft, 
  pta_udder_depth, 
  pta_teat_front_place,
  pta_teat_length, 
  pta_milk_fever,
  pta_stature, 
  pta_strength, 
  pta_mobility,
  pta_milk_speed, 
  pta_dairy_form, 
  pta_body_depth, 
  sire_ce, 
  daughter_ce, 
  sire_sb, 
  daughter_sb, 
  udder_composite, 
  NM, 
  PTI,
  CM, 
  FM,
  GM, 
  GTPI,
  sire_name, 
>>>>>>> second-repo-remote/main
  mgs_name,
  status_alt
FROM
  CDCB_data.NAAB_evals
"

naab_old <- bq_project_query(projectid, query)
naab_old <- bq_table_download(naab_old)

# Join animal data with sires data
naab <- left_join(naab, naab_old[,c("reg_id", "sire_name", "mgs_name",
<<<<<<< HEAD
                                    "status_alt", "controller_number",
=======
                                    "status_alt", "controller_number", 
>>>>>>> second-repo-remote/main
                                    "controller_name")], by = "reg_id")

# Keep only unique observations and remove data with missing sire and dam data
naab |>
  distinct(reg_id, .keep_all = TRUE) |>
  filter(!(is.na(sire_id) | is.na(dam_id))) ->
  naab

# Find all ancestor of every bull using recursive joins
connections <- naab[, c('reg_id', 'sire_id')] |>
  distinct(reg_id, .keep_all = TRUE)

names(connections) <- c("reg_id", "sire_id_g0")

connect <- naab[, c('reg_id', 'sire_id')] |>
  distinct(reg_id, .keep_all = TRUE)

cnt = 1
ix = 0

while(cnt != 0){
  connect_temp <- connect
<<<<<<< HEAD
  names(connect_temp) <- c(paste0('sire_id_g', as.character(ix)),
                           paste0('sire_id_g', as.character(ix+1)))
  connections <- left_join(connections, connect_temp,
=======
  names(connect_temp) <- c(paste0('sire_id_g', as.character(ix)), 
                           paste0('sire_id_g', as.character(ix+1)))
  connections <- left_join(connections, connect_temp, 
>>>>>>> second-repo-remote/main
                           by = paste0('sire_id_g', as.character(ix)))
  ix <- ix + 1
  cnt <- table(connections[, paste0('sire_id_g', as.character(ix))]) |>
    dim()
  print(cnt)
  rm(connect_temp)
}

naab_line <- connections[, -18]
rm(connections, connect)

naab_line  |>
<<<<<<< HEAD
  pivot_longer(sire_id_g0:sire_id_g15, names_to = "generation",
=======
  pivot_longer(sire_id_g0:sire_id_g15, names_to = "generation", 
>>>>>>> second-repo-remote/main
               values_to = "sire_id") |>
  separate(generation, into = c("type", "id", "gen"), sep = "_") |>
  select(-id) |>
  filter(!is.na(sire_id)) |>
  separate(gen, into = c("g_var", "gen_num"), sep = 1) |>
  select(-g_var) |>
  mutate(gen_num = as.numeric(gen_num)+1) ->
<<<<<<< HEAD
  naab_line
=======
  naab_line 
>>>>>>> second-repo-remote/main


# Top sires
# Merge lines with years of birth data
naab_sample <- left_join(naab_line, naab[, c("reg_id", "yob")],
                         by = join_by(sire_id == reg_id)) |>
  rename(yob_sire = yob)

naab_sample <- left_join(naab_sample, naab[, c("reg_id", "yob", "breed")],
                         by = "reg_id")

# Identify number of sons (1st generation) to define superstars
<<<<<<< HEAD
sons <- naab_sample |>
  filter((yob_sire > yob_start & yob_sire < yob_end) & gen_num == 1) |>
  group_by(sire_id) |>
  summarise(N = n()) |>
  arrange(desc(N)) |>
  data.frame() |>
=======
sons <- naab_sample |> 
  filter((yob_sire > yob_start & yob_sire < yob_end) & gen_num == 1) |>  
  group_by(sire_id) |> 
  summarise(N = n()) |> 
  arrange(desc(N)) |>
  data.frame() |> 
>>>>>>> second-repo-remote/main
  sum_up(N, d = TRUE) |>
  data.frame() |>
  select(perc) |>
  as.integer()

<<<<<<< HEAD
naab_sample |>
  filter(yob_sire > yob_start & yob_sire < yob_end &
           gen_num == 1) |>
  group_by(sire_id) |>
  summarise(N = n()) |>
  arrange(desc(N)) |>
=======
naab_sample |> 
  filter(yob_sire > yob_start & yob_sire < yob_end & 
           gen_num == 1) |> 
  group_by(sire_id) |> 
  summarise(N = n()) |> 
  arrange(desc(N)) |> 
>>>>>>> second-repo-remote/main
  data.frame() |>
  mutate(superstar = ifelse(N > sons, 1, 0)) |>
  select(c(sire_id, superstar)) ->
  sires

<<<<<<< HEAD
# Matching estimator
=======
# Matching estimator 
>>>>>>> second-repo-remote/main
naab_old |>
  filter(reg_id %in% sires[sires$superstar == 1, "sire_id"]) |>
  tab(yob)

naab_old |>
  filter(reg_id %in% sires$sire_id) |>
  mutate(superstar = ifelse(reg_id %in% sires[sires$superstar == 1, "sire_id"], 1, 0)) ->
  base_sample

# Convert all genetic codes to dummies
<<<<<<< HEAD
data_1 <- base_sample |>
=======
data_1 <- base_sample |> 
>>>>>>> second-repo-remote/main
  filter(breed == "HO") |>
  mutate(TR = ifelse(str_detect(genetic_codes, "TR"), 1, 0),
         TC = ifelse(str_detect(genetic_codes, "TC"), 1, 0),
         TV = ifelse(str_detect(genetic_codes, "TV"), 1, 0),
         TD = ifelse(str_detect(genetic_codes, "TD"), 1, 0),
         TL = ifelse(str_detect(genetic_codes, "TL"), 1, 0),
         TM = ifelse(str_detect(genetic_codes, "TM"), 1, 0),
         TW = ifelse(str_detect(genetic_codes, "TW"), 1, 0),
         TY = ifelse(str_detect(genetic_codes, "TY"), 1, 0),
         TP = ifelse(str_detect(genetic_codes, "TP"), 1, 0),
         BL = ifelse(str_detect(genetic_codes, "BL"), 1, 0),
         BY = ifelse(str_detect(genetic_codes, "BY"), 1, 0),
         CV = ifelse(str_detect(genetic_codes, "CV"), 1, 0),
         BR = ifelse(str_detect(genetic_codes, "B/R"), 1, 0),
         DW = ifelse(str_detect(genetic_codes, "D W"), 1, 0),
         M = ifelse(str_detect(genetic_codes, "M*"), 1, 0),
         D = ifelse(str_detect(genetic_codes, "D"), 1, 0),
         RC = ifelse(str_detect(genetic_codes, "RC"), 1, 0),
         PC = ifelse(str_detect(genetic_codes, "PC"), 1, 0),
         CD = ifelse(str_detect(genetic_codes, "CD"), 1, 0),
         JNSC = ifelse(str_detect(genetic_codes, "JNSC"), 1, 0),
         AH1C = ifelse(str_detect(haplotypes, "AH1C"), 1, 0),
         AH2C = ifelse(str_detect(haplotypes, "AH2C"), 1, 0),
         AH2T = ifelse(str_detect(haplotypes, "AH2T"), 1, 0),
         BH2C = ifelse(str_detect(haplotypes, "BH2C"), 1, 0),
         BH2T = ifelse(str_detect(haplotypes, "BH2T"), 1, 0),
         BHP = ifelse(str_detect(haplotypes, "BHP"), 1, 0),
         BHM = ifelse(str_detect(haplotypes, "BHM"), 1, 0),
         BHW = ifelse(str_detect(haplotypes, "BHW"), 1, 0),
         HH0C = ifelse(str_detect(haplotypes, "HH0-C"), 1, 0),
         HH1C = ifelse(str_detect(haplotypes, "HH1C"), 1, 0),
         HH2C = ifelse(str_detect(haplotypes, "HH2C"), 1, 0),
         HH3C = ifelse(str_detect(haplotypes, "HH3C"), 1, 0),
         HH5C = ifelse(str_detect(haplotypes, "HH5C"), 1, 0),
         HH1T = ifelse(str_detect(haplotypes, "HH1T"), 1, 0),
         HH2T = ifelse(str_detect(haplotypes, "HH2T"), 1, 0),
         HH3T = ifelse(str_detect(haplotypes, "HH3T"), 1, 0),
         HH4T = ifelse(str_detect(haplotypes, "HH4T"), 1, 0),
         HH5T = ifelse(str_detect(haplotypes, "HH5T"), 1, 0),
         HH6T = ifelse(str_detect(haplotypes, "HH6T"), 1, 0),
         HHRC = ifelse(str_detect(haplotypes, "HHR-C"), 1, 0),
         HHCC = ifelse(str_detect(haplotypes, "HHC-C"), 1, 0),
         HCDT = ifelse(str_detect(haplotypes, "HCD-T"), 1, 0),
         HCDC = ifelse(str_detect(haplotypes, "HCD-C"), 1, 0),
         HHPA = ifelse(str_detect(haplotypes, "HHP-A"), 1, 0),
         HDRC = ifelse(str_detect(haplotypes, "HDR-C"), 1, 0),
         JH1C = ifelse(str_detect(haplotypes, "JH1C"), 1, 0),
         JH1F = ifelse(str_detect(haplotypes, "JH1F"), 1, 0),
         JHP = ifelse(str_detect(haplotypes, "JHP"), 1, 0),
         A1A1 = ifelse(str_detect(beta_casein, "A1A1"), 1, 0),
         A1A2 = ifelse(str_detect(beta_casein, "A1A2"), 1, 0),
         A2A2 = ifelse(str_detect(beta_casein, "A2A2"), 1, 0),
         AA = ifelse(str_detect(kappa_casein, "AA"), 1, 0),
         AB = ifelse(str_detect(kappa_casein, "AB"), 1, 0),
         AE = ifelse(str_detect(kappa_casein, "AE"), 1, 0),
         BB = ifelse(str_detect(kappa_casein, "BB"), 1, 0),
         BE = ifelse(str_detect(kappa_casein, "BE"), 1, 0)) |>
  select(c(reg_id, reg_name, yob, pta_milk, pta_fat_lb,
<<<<<<< HEAD
           pta_protein_lb, pta_scs, pta_pl, pta_dpr,
           pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length,
           pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis,
           pta_disp_abomasum, NM, CM, FM, GM, superstar, num_dtrs,
           num_herds, pta_strength, pta_mobility, TR, TC, TV, TD,
           TL, TM, TW, TY, TP, BL, BY, CV, BR, DW, M, D, RC,
           PC, CD, JNSC, JNSC, AH1C,
           AH2C, AH2T, BH2C, BH2T, BHP, BHM, BHW, HH0C, HH1C, HH2C,
           HH3C, HH5C, HH1T, HH2T, HH3T, HH4T, HH5T, HH6T, HHRC,
           HHCC, HCDT, HCDC, HHPA, HDRC, JH1C, JH1F, JHP, A1A1,
           A1A2, A2A2, AA, AB, AE, BB, BE, pta_r_placenta,
           pta_rear_legs_rear, pta_foot_leg_score, pta_teat_rear_place,
           pta_rump_angle, pta_thurl_width, pta_rear_legs_side,
           pta_foot_angle, pta_fore_udder, pta_rear_udder_height,
           pta_rear_udder_width, pta_udder_cleft, pta_udder_depth,
           pta_teat_front_place, pta_teat_length, pta_milk_fever,
=======
           pta_protein_lb, pta_scs, pta_pl, pta_dpr, 
           pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length,
           pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis,
           pta_disp_abomasum, NM, CM, FM, GM, superstar, num_dtrs, 
           num_herds, pta_strength, pta_mobility, TR, TC, TV, TD, 
           TL, TM, TW, TY, TP, BL, BY, CV, BR, DW, M, D, RC, 
           PC, CD, JNSC, JNSC, AH1C, 
           AH2C, AH2T, BH2C, BH2T, BHP, BHM, BHW, HH0C, HH1C, HH2C,
           HH3C, HH5C, HH1T, HH2T, HH3T, HH4T, HH5T, HH6T, HHRC,
           HHCC, HCDT, HCDC, HHPA, HDRC, JH1C, JH1F, JHP, A1A1, 
           A1A2, A2A2, AA, AB, AE, BB, BE, pta_r_placenta,
           pta_rear_legs_rear, pta_foot_leg_score, pta_teat_rear_place, 
           pta_rump_angle, pta_thurl_width, pta_rear_legs_side,
           pta_foot_angle, pta_fore_udder, pta_rear_udder_height, 
           pta_rear_udder_width, pta_udder_cleft, pta_udder_depth, 
           pta_teat_front_place, pta_teat_length, pta_milk_fever, 
>>>>>>> second-repo-remote/main
           pta_stature, pta_dairy_form, pta_body_depth, sire_ce,
           daughter_ce, controller_number)) |>
  data.frame()

# Convert missing values to zero
<<<<<<< HEAD
data_1[is.na(data_1)] <- 0

# Download inbreeding data
query <- "SELECT
  reg_id,
=======
data_1[is.na(data_1)] <- 0 

# Download inbreeding data
query <- "SELECT
  reg_id, 
>>>>>>> second-repo-remote/main
  inbreeding
FROM
  CDN_data.CDN_bulls
"
cdn <- bq_project_query(projectid, query)
cdn <- bq_table_download(cdn)

data_1 <- left_join(data_1, cdn, by = "reg_id")

data_1 |>
  mutate(inbreeding = ifelse(!is.na(inbreeding), inbreeding/100, 0)) ->
  data_1

<<<<<<< HEAD
# Load genetics companies data
=======
# Load genetics companies data 
>>>>>>> second-repo-remote/main
companies <- read_csv("../data/company_info.csv")

companies |>
  mutate(date_1 = str_split(assigned, " ", simplify = TRUE)[,2],
         date_2 = str_split(assigned, " ", simplify = TRUE)[,5]) |>
  separate(date_1, into = c("assigned_date", "temp"), sep = ";") |>
  select(-temp) |>
  separate(date_2, into = c("end_date", "temp"), sep = ";") |>
  select(-temp) |>
  mutate(assigned_date = mdy(assigned_date),
         end_date = mdy(end_date)) |>
  select(-assigned) ->
  companies

<<<<<<< HEAD
data_1 <- left_join(data_1, companies,
                       by = join_by(controller_number == primary_stud_code))
data_1 |>
  mutate(parent_company = replace(parent_company, is.na(parent_company), "Other")) ->
  data_1
=======
data_1 <- left_join(data_1, companies, 
                       by = join_by(controller_number == primary_stud_code))
data_1 |>
  mutate(parent_company = replace(parent_company, is.na(parent_company), "Other")) ->
  data_1	
>>>>>>> second-repo-remote/main

## Convert company names to dummies
data_1 |>
  mutate(abs = ifelse(parent_company == "ABS Global", 1, 0),
         alta_gen = ifelse(parent_company == "Alta Genetics", 1, 0),
         crv = ifelse(parent_company == "CRV Holding", 1, 0),
         genex = ifelse(parent_company == "Genex", 1, 0),
         ips = ifelse(parent_company == "International Protein Sires", 1, 0),
         jlg = ifelse(parent_company == "JLG Enterprises", 1, 0),
         other = ifelse(parent_company == "Other", 1, 0),
         stg = ifelse(parent_company == "STgenetics", 1, 0),
         ssires = ifelse(parent_company == "Select Sires", 1, 0),
         semex = ifelse(parent_company == "Semex Alliance", 1, 0)) ->
  data_1

match_data_1 <- matchit(superstar ~ pta_milk+pta_fat_lb+pta_protein_lb+pta_pl+
                          pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+pta_gest_length+
                          pta_heifer_liv+pta_efcalving+pta_mastitis+pta_metritis+
                          pta_strength+pta_rear_legs_rear+ pta_foot_leg_score+
                          pta_teat_rear_place+pta_rump_angle+
                          pta_thurl_width+pta_rear_legs_side+pta_foot_angle+
                          pta_fore_udder+pta_rear_udder_height+
                          pta_rear_udder_width+pta_udder_cleft+pta_udder_depth+
                          pta_teat_front_place+pta_teat_length+pta_milk_fever+
                          pta_stature+pta_dairy_form+pta_body_depth+sire_ce+
                          daughter_ce+TR+TC+TD+TL+TM+TY+TP+BY+BR+D+RC+CD+HH1C+
                          HH2C+HH5C+HH1T+HH2T+HH4T+HH5T+HH6T+HHRC+HCDT+HCDC+
                          A1A1+A1A2+A2A2+AA+AB+BB+abs+alta_gen+crv+genex+ips+
                          jlg+other+stg+ssires+semex,
<<<<<<< HEAD
                        data = data_1, method = "nearest", replace = TRUE,
                        verbose = TRUE, ratio = ratio, distance = "glm",
=======
                        data = data_1, method = "nearest", replace = TRUE, 
                        verbose = TRUE, ratio = ratio, distance = "glm", 
>>>>>>> second-repo-remote/main
                        link = "logit", discard = "none")

# Identify matches
select <- dplyr::select
match_data_1$match.matrix |>
  data.frame() |>
  rownames_to_column(var = "superstar") |>
  pivot_longer(-superstar, names_to = "match", values_to = "alt_sire") |>
  filter(!is.na(alt_sire)) |>
  separate(match, into = c("X", "match_num"), sep = 1) |>
  select(-X) ->
  rel_df

match.data(match_data_1, distance = "pscore") |>
  select(reg_id, pscore) |>
  rownames_to_column(var = "num_id") ->
  index_df

matches <- get_matches(match_data_1, distance = "pscore") |>
  select(id, subclass, reg_id)

<<<<<<< HEAD
match_data_1 |>
=======
match_data_1 |> 
>>>>>>> second-repo-remote/main
  match.data(distance = "pscore", drop.unmatched = FALSE) |>
  left_join(matches, by = "reg_id") |>
  mutate(group = case_when(!is.na(subclass)  & superstar == 1 ~ "Treated",
                           !is.na(subclass)  & superstar == 0 ~ "Control",
                           TRUE ~ "Unmatched")) |>
  filter(inbreeding > 0) |>
  ggplot(aes(x = inbreeding, after_stat(density))) +
  geom_density(aes(color = group), size = .75) +
<<<<<<< HEAD
  theme_bw() +
=======
  theme_bw() + 
>>>>>>> second-repo-remote/main
  geom_hline(yintercept = 0) +
  xlab("Inbreeding rate") + ylab("Density") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
<<<<<<< HEAD
        text = element_text(family = "Palatino"))

### Balance plot
v <- data.frame(old = c("pta_milk", "pta_fat_lb", "pta_protein_lb", "pta_pl",
                        "pta_dpr", "pta_hcr", "pta_ccr", "pta_liv", "pta_type",
                        "pta_gest_length", "pta_heifer_liv", "pta_efcalving",
                        "pta_mastitis", "pta_metritis", "pta_strength",
                        "pta_rear_legs_rear", "pta_foot_leg_score",
                        "pta_teat_rear_place", "pta_rump_angle", "pta_thurl_width",
                        "pta_rear_legs_side", "pta_foot_angle", "pta_fore_udder",
                        "pta_rear_udder_height", "pta_rear_udder_width",
                        "pta_udder_cleft", "pta_udder_depth",
                        "pta_teat_front_place", "pta_teat_length", "pta_milk_fever",
                        "pta_stature", "pta_dairy_form", "pta_body_depth",
                        "sire_ce", "daughter_ce", "TR", "TC", "TD", "TL", "TM",
                        "TY", "TP", "BY", "BR", "D", "RC", "CD", "HH1C", "HH2C",
                        "HH5C", "HH1T", "HH2T", "HH4T", "HH5T", "HH6T", "HHRC",
                        "HCDT", "HCDC", "A1A1", "A1A2", "A2A2", "AA", "AB", "BB",
                        "abs", "alta_gen", "crv", "genex", "ips", "jlg", "other",
                        "stg", "ssires", "semex"),
                new = c("Milk PTA", "Fat PTA", "Protein PTA", "Prod. life PTA",
                        "DPR PTA", "HCR PTA", "CCR PTA", "Liveability PTA", "Type PTA",
                        "Gest. length PTA", "Heifer liv. PTA", "Eff. calving PTA",
                        "Mastitis PTA", "Metritis PTA", "Strength PTA",
                        "Rear legs (r) PTA", "Foot leg score PTA",
                        "Teat rear place PTA", "Rump angle PTA", "Thurl width PTA",
                        "Rear legs(s) PTA", "Foot angle PTA", "Fore udder PTA",
                        "Rear udder ht. PTA", "Rear udder wt. PTA", "Udder cleft PTA",
                        "Udder depth PTA", "Teat front place PTA", "Teat length PTA",
                        "Milk fever PTA", "Stature PTA", "Dairy form PTA",
                        "Body depth PTA", "Sire CE", "Daughter CE", "TR", "TC",
                        "TD", "TL", "TM",  "TY", "TP", "BY", "BR", "D", "RC",
                        "CD", "HH1C", "HH2C",  "HH5C", "HH1T", "HH2T", "HH4T",
                        "HH5T", "HH6T", "HHRC", "HCDT", "HCDC", "A1A1", "A1A2",
=======
        text = element_text(family = "Palatino")) 

### Balance plot
v <- data.frame(old = c("pta_milk", "pta_fat_lb", "pta_protein_lb", "pta_pl", 
                        "pta_dpr", "pta_hcr", "pta_ccr", "pta_liv", "pta_type",
                        "pta_gest_length", "pta_heifer_liv", "pta_efcalving",
                        "pta_mastitis", "pta_metritis", "pta_strength", 
                        "pta_rear_legs_rear", "pta_foot_leg_score", 
                        "pta_teat_rear_place", "pta_rump_angle", "pta_thurl_width", 
                        "pta_rear_legs_side", "pta_foot_angle", "pta_fore_udder", 
                        "pta_rear_udder_height", "pta_rear_udder_width", 
                        "pta_udder_cleft", "pta_udder_depth", 
                        "pta_teat_front_place", "pta_teat_length", "pta_milk_fever",
                        "pta_stature", "pta_dairy_form", "pta_body_depth", 
                        "sire_ce", "daughter_ce", "TR", "TC", "TD", "TL", "TM", 
                        "TY", "TP", "BY", "BR", "D", "RC", "CD", "HH1C", "HH2C", 
                        "HH5C", "HH1T", "HH2T", "HH4T", "HH5T", "HH6T", "HHRC", 
                        "HCDT", "HCDC", "A1A1", "A1A2", "A2A2", "AA", "AB", "BB",
                        "abs", "alta_gen", "crv", "genex", "ips", "jlg", "other",
                        "stg", "ssires", "semex"),
                new = c("Milk PTA", "Fat PTA", "Protein PTA", "Prod. life PTA", 
                        "DPR PTA", "HCR PTA", "CCR PTA", "Liveability PTA", "Type PTA",
                        "Gest. length PTA", "Heifer liv. PTA", "Eff. calving PTA",
                        "Mastitis PTA", "Metritis PTA", "Strength PTA", 
                        "Rear legs (r) PTA", "Foot leg score PTA", 
                        "Teat rear place PTA", "Rump angle PTA", "Thurl width PTA", 
                        "Rear legs(s) PTA", "Foot angle PTA", "Fore udder PTA", 
                        "Rear udder ht. PTA", "Rear udder wt. PTA", "Udder cleft PTA", 
                        "Udder depth PTA", "Teat front place PTA", "Teat length PTA", 
                        "Milk fever PTA", "Stature PTA", "Dairy form PTA", 
                        "Body depth PTA", "Sire CE", "Daughter CE", "TR", "TC", 
                        "TD", "TL", "TM",  "TY", "TP", "BY", "BR", "D", "RC", 
                        "CD", "HH1C", "HH2C",  "HH5C", "HH1T", "HH2T", "HH4T", 
                        "HH5T", "HH6T", "HHRC", "HCDT", "HCDC", "A1A1", "A1A2", 
>>>>>>> second-repo-remote/main
                        "A2A2", "AA", "AB", "BB", "ABS Global", "Alta Genetics",
                        "CRV Holding", "Genex", "IPS", "JLG Enterprises", "Other",
                        "STGenetics", "Select Sires", "Semex"))

love.plot(match_data_1, thresholds = (ks = .1), stats = "mean.diffs",
          drop.distance = TRUE, var.names = v) +
  paletteer::scale_color_paletteer_d("rtist::vangogh") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "Palatino")) +
  ggtitle("")

# Construct descendants data base
rel_df <- left_join(rel_df, index_df, by = join_by("alt_sire" == "num_id"))

data_1 |>
  rownames_to_column(var = "index") |>
  filter(superstar == 1) |>
  select(c(reg_id, index)) ->
  index_df

rel_df <- left_join(rel_df, index_df, by = join_by("superstar" == "index"))
rel_df |>
  rename(reg_id = reg_id.y,
         reg_id_alt = reg_id.x) |>
  select(-c(superstar, alt_sire)) ->
  rel_df

# Keep only latest evaluation data
naab_aiss |>
  mutate(period = ym(period),
         year_proof = year(period)) |>
  group_by(reg_id) |>
  arrange(year_proof) |>
  slice_tail(n = 1) |>
  ungroup() ->
  naab_unique

# Assemble control group
naab_line |>
  filter(sire_id %in% rel_df$reg_id_alt) |>
  mutate(weight = 2^(-gen_num)) |>
<<<<<<< HEAD
  select(c(reg_id, sire_id, weight, gen_num)) ->
  control

control <- left_join(control, naab_old, by = "reg_id")

control <- left_join(control, cdn[, c("reg_id", "inbreeding")],
                     by = "reg_id")

control <- left_join(control, rel_df[, c("reg_id_alt", "pscore")],
                     by = join_by("sire_id" == "reg_id_alt"))

control |>
  distinct(reg_id, .keep_all = TRUE) ->
  control

# Assemble treatment group
naab_line |>
  filter(sire_id %in% rel_df$reg_id) |>
  mutate(weight = 2^(-gen_num)) |>
  select(c(reg_id, sire_id, weight, gen_num)) ->
  treat

treat <- left_join(treat, naab_old, by = "reg_id")

treat <- left_join(treat, cdn,
                   by = "reg_id")

# Put it all together
data_full <- rbind(treat |>
                     mutate(treat  = 1),
                   control |>
=======
  select(c(reg_id, sire_id, weight, gen_num)) -> 
  control

control <- left_join(control, naab_old, by = "reg_id") 

control <- left_join(control, cdn[, c("reg_id", "inbreeding")], 
                     by = "reg_id") 

control <- left_join(control, rel_df[, c("reg_id_alt", "pscore")], 
                     by = join_by("sire_id" == "reg_id_alt"))

control |> 
  distinct(reg_id, .keep_all = TRUE) ->
  control

# Assemble treatment group 
naab_line |>
  filter(sire_id %in% rel_df$reg_id) |>
  mutate(weight = 2^(-gen_num)) |>
  select(c(reg_id, sire_id, weight, gen_num)) -> 
  treat

treat <- left_join(treat, naab_old, by = "reg_id") 

treat <- left_join(treat, cdn, 
                   by = "reg_id")

# Put it all together
data_full <- rbind(treat |> 
                     mutate(treat  = 1),
                   control |> 
>>>>>>> second-repo-remote/main
                     select(-pscore) |>
                     mutate(treat = 0))

inb_alt <- read_csv("../data/inb_alt.csv")
data_full <- left_join(data_full, inb_alt, by = "reg_id")

data_full |>
  mutate(inbreeding = ifelse(is.na(inbreeding.x), inbreeding.y, inbreeding.x)) |>
  select(-c(inbreeding.x, inbreeding.y)) ->
  data_full

<<<<<<< HEAD
data_full <- left_join(data_full, companies,
                    by = join_by(controller_number == primary_stud_code))
data_full |>
  mutate(parent_company = replace(parent_company, is.na(parent_company), "Other")) ->
  data_full

naab_aiss |>
  group_by(reg_id) |>
  arrange(period) |>
=======
data_full <- left_join(data_full, companies, 
                    by = join_by(controller_number == primary_stud_code))
data_full |>
  mutate(parent_company = replace(parent_company, is.na(parent_company), "Other")) ->
  data_full	

naab_aiss |>
  group_by(reg_id) |>
  arrange(period) |> 
>>>>>>> second-repo-remote/main
  slice_head(n = 1) |>
  ungroup() |>
  select(c(reg_id, NM_unadj)) ->
  NM_df

data_full <- left_join(data_full, NM_df, by = "reg_id")

<<<<<<< HEAD
rm(cdn, naab_aiss, naab_old, companies, control, treat, sires,
   base_sample, data_1, index_df, naab, naab_unique, match_data_1,
   rel_df, naab_line, naab_sample, inb_alt, sons, ratio, cnt, ix, NM_df)
=======
rm(cdn, naab_aiss, naab_old, companies, control, treat, sires, 
   base_sample, data_1, index_df, naab, naab_unique, match_data_1,
   rel_df, naab_line, naab_sample, inb_alt, sons, ratio, cnt, ix, NM_df)
>>>>>>> second-repo-remote/main
