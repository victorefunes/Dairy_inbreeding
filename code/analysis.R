options(warn=-1)
options(scipen=999)
library(tidyverse)
library(bigrquery)
library(lubridate)
library(statar)
library(Hmisc)
library(stringi)
library(broom)
library(ggroups)
library(kinship2)

folder <- "C:/Users/victorf2"
#folder <- "C:/Users/victo"

setwd(paste0(folder, "/Box/dairy_matching/code/AISS data"))

bq_auth(email = "victorf2@illinois.edu")

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

naab <- naab |>
  mutate(birthdate = mdy(birthdate),
         yob = year(birthdate))

naab <- naab |>
  mutate(id_number = stringr::str_replace(id_number, "\\.", ""),
         id_number = str_pad(id_number, width = 12, side = "left", pad = "0"),
         reg_id = paste0(breed, country, id_number))

naab <- naab |>
  mutate(sire_id_number = stringr::str_replace(sire_id_number, "\\.", ""),
         sire_id_number = str_pad(sire_id_number, width = 12, side = "left", pad = "0"),
         sire_id = paste0(sire_breed, sire_country, sire_id_number))

naab$sire_id <- replace(naab$sire_id, naab$sire_id == "NANANA", NA)

naab <- naab |>
  mutate(mgs_id_number = stringr::str_replace(mgs_id_number, "\\.", ""),
         mgs_id_number = str_pad(mgs_id_number, width = 12, side = "left", pad = "0"),
         mgs_id = paste0(mgs_breed, mgs_country, mgs_id_number))

naab$mgs_id <- replace(naab$mgs_id, naab$mgs_id == "NANANA", NA)

naab <- naab |>
  mutate(dam_id_number = stringr::str_replace(dam_id_number, "\\.", ""),
         dam_id_number = str_pad(dam_id_number, width = 12, side = "left", pad = "0"),
         dam_id = paste0(dam_breed, dam_country, dam_id_number))

naab$dam_id <- replace(naab$dam_id, naab$dam_id == "NANANA", NA)


naab_old <- read_csv("../../data/old_evals_clean.csv")

naab <- left_join(naab, naab_old[,c("reg_id", "sire_name", "mgs_name",
                                    "status_alt", "controller_number", 
                                    "controller_name")], by = "reg_id")

naab |>
  distinct(reg_id, .keep_all = TRUE) |>
  filter(!is.na(sire_id) & !is.na(dam_id)) ->
  naab

cdn <- read.csv(paste0(folder, "/Box/Dairy Industry in US/Data/Network data/cdn_data.csv"))

cdn |>
  separate(sire_id, into = c("sire_breed", "temp"), sep = 2) |>
  separate(temp, into = c("sire_country", "temp"), sep = 3) |>
  separate(temp, into = c("temp", "sire_id_number"), sep = 1) |>
  filter(!is.na(sire_id_number)) |>
  select(-temp) |>
  mutate(sire_id_number = str_pad(sire_id_number, width = 12, side = "left", pad = "0"),
         sire_id = paste0(sire_breed, sire_country, sire_id_number)) ->
  cdn
cdn$sire_id <- replace(cdn$sire_id, cdn$sire_id == "NANANA", NA)

cdn |>
  separate(dam_id, into = c("dam_breed", "temp"), sep = 2) |>
  separate(temp, into = c("dam_country", "temp"), sep = 3) |>
  separate(temp, into = c("temp", "dam_id_number"), sep = 1) |>
  filter(!is.na(dam_id_number)) |>
  select(-temp) |>
  mutate(dam_id_number = str_pad(dam_id_number, width = 12, side = "left", pad = "0"),
         dam_id = paste0(dam_breed, dam_country, dam_id_number)) ->
  cdn
cdn$dam_id <- replace(cdn$dam_id, cdn$dam_id == "NANANA", NA)

cdn |> 
  separate(mgs_id, into = c("mgs_breed", "temp"), sep = 2) |>
  separate(temp, into = c("mgs_country", "temp"), sep = 3) |>
  separate(temp, into = c("temp", "mgs_id_number"), sep = 1) |>
  filter(!is.na(mgs_id_number)) |>
  select(-temp) |>
  mutate(mgs_id_number = str_pad(mgs_id_number, width = 12, side = "left", pad = "0"),
         mgs_id = paste0(mgs_breed, mgs_country, mgs_id_number)) ->
  cdn
cdn$mgs_id <- replace(cdn$mgs_id, cdn$mgs_id == "NANANA", NA)

cdn |>
  separate(gmgs_id, into = c("gmgs_breed", "temp"), sep = 2) |>
  separate(temp, into = c("gmgs_country", "temp"), sep = 3) |>
  separate(temp, into = c("temp", "gmgs_id_number"), sep = 1) |>
  filter(!is.na(gmgs_id_number)) |>
  select(-temp) |>
  mutate(gmgs_id_number = str_pad(gmgs_id_number, width = 12, side = "left", pad = "0"),
         gmgs_id = paste0(gmgs_breed, gmgs_country, gmgs_id_number)) ->
  cdn
cdn$gmgs_id <- replace(cdn$gmgs_id, cdn$gmgs_id == "NANANA", NA)

cdn |>
  separate(gmgd_id, into = c("gmgd_breed", "temp"), sep = 2) |>
  separate(temp, into = c("gmgd_country", "temp"), sep = 3) |>
  separate(temp, into = c("temp", "gmgd_id_number"), sep = 1) |>
  filter(!is.na(gmgd_id_number)) |>
  select(-temp) |>
  mutate(gmgd_id_number = str_pad(gmgd_id_number, width = 12, side = "left", pad = "0"),
         gmgd_id = paste0(gmgd_breed, gmgd_country, gmgd_id_number)) ->
  cdn
cdn$gmgd_id <- replace(cdn$gmgd_id, cdn$gmgd_id == "NANANA", NA)

cdn |> 
  distinct(reg_id, .keep_all = TRUE) -> 
  cdn

cdn |>
  select(-reg_id) |>
  rename(reg_id = reg_id_alt) ->
  cdn

cdn |>
  filter(reg_id %in% naab$reg_id) ->
  cdn

connections <- naab[, c('reg_id', 'sire_id')] |>
  distinct(reg_id, .keep_all = TRUE)
names(connections) <- c("reg_id", "sire_id_g0")
connect <- naab[, c('reg_id', 'sire_id')] |>
  distinct(reg_id, .keep_all = TRUE)

cnt = 1
ix = 0

while(cnt != 0){
  connect_temp <- connect
  names(connect_temp) <- c(paste0('sire_id_g', as.character(ix)), paste0('sire_id_g', as.character(ix+1)))
  connections <- left_join(connections, connect_temp, by = paste0('sire_id_g', as.character(ix)))
  ix <- ix + 1
  cnt <- table(connections[, paste0('sire_id_g', as.character(ix))]) |>
    dim()
  print(cnt)
  rm(connect_temp)
}

naab_line <- connections[, -18]
rm(connections, connect)

naab_line  |>
  pivot_longer(sire_id_g0:sire_id_g15, names_to = "generation", values_to = "sire_id") |>
  separate(generation, into = c("type", "id", "gen"), sep = "_") |>
  select(-id) |>
  filter(!is.na(sire_id)) |>
  separate(gen, into = c("g_var", "gen_num"), sep = 1) |>
  select(-g_var) %>%
  mutate(gen_num = as.numeric(gen_num)+1) ->
  naab_line

library(ggroups)
library(kinship2)

naab |>
  select(reg_id, sire_id, dam_id) |>
  filter(!is.na(sire_id)) |>
  distinct(reg_id, .keep_all = TRUE) |>
  data.frame() ->
  ped_alt

ped_alt |>
  mutate(sex = "male") ->
  ped_alt

ped_alt <- fixParents(id = ped_alt$reg_id, dadid = ped_alt$sire_id, 
                      momid = ped_alt$dam_id, sex  = ped_alt$sex, missid = 0)
ped_kin <- with(ped_alt, pedigree(id, dadid, momid, sex, missid="0"))

ggped <- gghead(ped_alt[, c("id", "dadid", "momid")])

yob_start <- 1998
yob_end <- 2007

naab_sample <- left_join(naab_line, naab[, c("reg_id", "yob")],
                         by = join_by(sire_id == reg_id)) |>
  rename(yob_sire = yob)

naab_sample <- left_join(naab_sample, naab[, c("reg_id", "yob")],
                         by = "reg_id")

p95_sons <- naab_sample |> 
  filter(yob_sire > yob_start & yob_sire < yob_end & gen_num == 1) |> 
  tab(sire_id) |> 
  arrange(desc(Freq.)) |> 
  data.frame() |> 
  rename(N = Freq.) |> 
  sum_up(N, d = TRUE) |>
  data.frame() |>
  select(p95) |>
  as.integer()

naab_sample |> 
  filter(yob_sire > yob_start & yob_sire < yob_end & gen_num == 1) |> 
  tab(sire_id) |> 
  arrange(desc(Freq.)) |> 
  data.frame() |>
  rename(N = Freq.) |>
  mutate(superstar = ifelse(N > p95_sons, 1, 0)) |>
  select(c(sire_id, superstar)) ->
  sires

naab_old |>
  filter(reg_id %in% sires[sires$superstar == 1, "sire_id"]) |>
  tab(yob)

naab_old |>
  filter(reg_id %in% sires$sire_id) |>
  mutate(superstar = ifelse(reg_id %in% sires[sires$superstar == 1, "sire_id"], 1, 0)) ->
  base_sample

library(MatchIt)

data_1 <- base_sample |> 
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
           pta_stature, pta_dairy_form, pta_body_depth, sire_ce,
           daughter_ce)) |>
  data.frame()

data_1[is.na(data_1)] <- 0
match_data_1 <- matchit(superstar ~ pta_milk+pta_fat_lb+pta_protein_lb+pta_pl+
                          pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+pta_gest_length+
                          pta_heifer_liv+pta_efcalving+pta_mastitis+pta_metritis+
                          pta_strength+num_dtrs+num_herds+pta_rear_legs_rear+ 
                          pta_foot_leg_score+pta_teat_rear_place+pta_rump_angle+
                          pta_thurl_width+pta_rear_legs_side+pta_foot_angle+
                          pta_fore_udder+pta_rear_udder_height+
                          pta_rear_udder_width+pta_udder_cleft+pta_udder_depth+
                          pta_teat_front_place+pta_teat_length+pta_milk_fever+
                          pta_stature+pta_dairy_form+pta_body_depth+sire_ce+
                          daughter_ce+TR+TC+TD+TL+TM+TY+TP+BY+BR+D+RC+CD+HH1C+
                          HH2C+HH5C+HH1T+HH2T+HH4T+HH5T+HH6T+HHRC+HCDT+HCDC+
                          A1A1+A1A2+A2A2+AA+AB+BB,
                        data = data_1, method = "nearest", distance = "lasso",
                        link = "logit", replace = FALSE, reestimate = TRUE,
                        verbose = TRUE, ratio = 10, discard = "control",
                        type.measure="deviance",
                        caliper = 0.25)
summary(match_data_1)

plot(match_data_1, type = "histogram")

set.seed(123)
match_data_2 <- matchit(superstar ~ pta_milk+pta_fat_lb+pta_protein_lb+pta_pl+
                          pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+pta_gest_length+
                          pta_heifer_liv+pta_efcalving+pta_mastitis+pta_metritis+
                          pta_strength+num_dtrs+num_herds+ pta_rear_legs_rear+ 
                          pta_foot_leg_score+pta_teat_rear_place+pta_rump_angle+
                          pta_thurl_width+pta_rear_legs_side+pta_foot_angle+
                          pta_fore_udder+pta_rear_udder_height+
                          pta_rear_udder_width+pta_udder_cleft+pta_udder_depth+
                          pta_teat_front_place+pta_teat_length+pta_milk_fever+
                          pta_stature+pta_dairy_form+pta_body_depth+sire_ce+
                          daughter_ce+TR+TC+TD+TL+TM+TY+TP+BY+BR+D+RC+CD+HH1C+
                          HH2C+HH5C+HH1T+HH2T+HH4T+HH5T+HH6T+HHRC+HCDT+HCDC+
                          A1A1+A1A2+A2A2+AA+AB+BB,
                        data = data_1, method = "nearest", distance = "lasso",
                        link = "logit", replace = FALSE, reestimate = TRUE,
                        verbose = TRUE, ratio = 9, discard = "control")
summary(match_data_2)

plot(match_data_2, type = "histogram")

plot(match_data_1$model)

coef(match_data_1$model, s = "lambda.min")
coef(match_data_1$model, s = "lambda.1se")

lasso  <- cv.glmnet(x = as.matrix(data_1[,c("pta_milk", "pta_fat_lb", "pta_protein_lb", 
                                         "pta_pl", "pta_dpr", "pta_hcr", "pta_ccr",
                                         "pta_liv", "pta_type", "pta_gest_length", 
                                         "pta_heifer_liv", "pta_efcalving", 
                                         "pta_mastitis", "pta_metritis", "pta_strength",
                                         "pta_rear_legs_rear", #"num_dtrs", "num_herds", 
                                         "pta_foot_leg_score", "pta_teat_rear_place", 
                                         "pta_rump_angle", "pta_thurl_width", 
                                         "pta_rear_legs_side", "pta_foot_angle", 
                                         "pta_fore_udder", "pta_rear_udder_height", 
                                         "pta_rear_udder_width", "pta_udder_cleft", 
                                         "pta_udder_depth", "pta_teat_front_place",
                                         "pta_teat_length", "pta_milk_fever",
                                         "pta_stature", "pta_dairy_form", 
                                         "pta_body_depth", "sire_ce", "daughter_ce",
                                         "TR", "TC", "TD", "TL", "TM", "TY", "TP",
                                         "BY", "BR", "D", "RC", "CD", "HH1C", 
                                         "HH2C", "HH5C", "HH1T", "HH2T", "HH4T", 
                                         "HH5T", "HH6T", "HHRC", "HCDT", "HCDC",
                                         "A1A1", "A1A2", "A2A2", "AA", "AB", "BB")]),
                     y = as.matrix(data_1$superstar), nfolds = 5, 
                    family = binomial(link = "logit"), alpha = 1)
plot(lasso)

match.data(match_data_1, distance = "pscore")  |>
  mutate(superstar = factor(superstar, labels = c("non-stars", "superstars"))) |>
  ggplot() + 
  geom_histogram(bins = 20, aes(x = pscore, fill = superstar), color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Pr(treat)") + ylab("Density")

data_1[match_data_1$match.matrix |> 
         t() |> 
         as.numeric(), c("reg_id", "reg_name")] |>
  filter(!is.na(reg_id)) -> 
  alt_stars

naab_line |>
  filter(sire_id %in% alt_stars$reg_id) |>
  mutate(weight = 2^(-gen_num)) |>
  select(c(reg_id, sire_id, weight)) -> 
  control

control <- left_join(control, naab_old, by = "reg_id")  

data_1 |> 
  filter(superstar == 1) |> 
  select(reg_id) ->
  treat_ids

naab_line |>
  filter(sire_id %in% treat_ids$reg_id) |>
  mutate(weight = 2^(-gen_num)) |>
  select(c(reg_id, sire_id, weight)) -> 
  treat

treat <- left_join(treat, naab_old, by = "reg_id") 

control <- left_join(control, cdn[, c("reg_id", "inbreeding")], 
                     by = "reg_id") 
treat <- left_join(treat, cdn[, c("reg_id", "inbreeding")], 
                   by = "reg_id")
control |> 
  distinct(reg_id, .keep_all = TRUE) ->
  control

treat |> 
  distinct(reg_id, .keep_all = TRUE) ->
  treat

alpha <- qnorm(0.95)
alpha_2 <- qnorm(0.99)
control |> 
  group_by(yob) |> 
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) |>
  summarise(inb_control = weighted.mean(inbreeding, w = weight),
            se_control = sqrt(wtd.var(inbreeding, weights = weight)),
            N_control = n()) |>
  data.frame() ->
  temp1

treat |> 
  group_by(yob) |> 
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) |>
  summarise(inb_treat = weighted.mean(inbreeding, w = weight),
            se_treat = sqrt(wtd.var(inbreeding, weights = weight)),
            N_treat = n()) |>
  data.frame() ->
  temp2

inb <- left_join(temp1, temp2, by = "yob");rm(temp1, temp2)
inb |> 
  pivot_longer(-yob, names_to = "variable", values_to = "values") |>
  separate(variable, into = c("var", "group"), sep = "_") |>
  pivot_wider(id_cols = c(yob, group), names_from = "var", values_from = "values") |>
  mutate(inb = inb/100, se = se/100) |>
  filter(yob < 2019) |>
  ggplot(aes(x = yob, y = inb, group = group)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) + 
  geom_ribbon(aes(ymin = inb-alpha*se/sqrt(N), 
                  ymax = inb+alpha*se/sqrt(N), fill = group),
              alpha = 0.35) +
  geom_ribbon(aes(ymin = inb-alpha_2*se/sqrt(N), 
                  ymax = inb+alpha_2*se/sqrt(N), fill = group),
              alpha = 0.15) +
  geom_vline(xintercept = 2009, linetype = 2) +
  xlab("Year of Birth") + ylab("Weighted Inbreeding Coefficient") +
  scale_x_continuous(breaks = 2003:2018) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

control |> 
  group_by(yob) |> 
  sum_up(inbreeding, d = TRUE) |> 
  ungroup() |> 
  filter(yob > 2004 & yob < 2018) |>
  select(-Variable) |>
  mutate(group = "control") ->
  temp1

treat |> 
  group_by(yob) |> 
  sum_up(inbreeding, d = TRUE) |> 
  ungroup() |> 
  filter(yob > 2004 & yob < 2018) |> 
  select(-Variable) |>
  mutate(group = "treat")  ->
  temp2

stats <- rbind(temp1, temp2);rm(temp1, temp2)

stats |>
  pivot_longer(-c(yob, group), names_to = "statistic", values_to = "values") |>
  filter(statistic %in% c("Obs", "Mean", "p50", "p10", "p90", "p99")) |>
  ggplot(aes(x = yob, y = values, group = group)) +
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) + 
  theme_minimal() + 
  geom_vline(xintercept = 2009, linetype  = 2) +
  scale_x_continuous(breaks = seq(2005, 2018, 3)) + 
  facet_wrap(.~statistic, scales = "free_y")

stats |>
  select(c(yob, group, Mean, StdDev, Obs)) |>
  mutate(group = replace(group, group == "treat", "treatment"),
         Mean = Mean/100,
         StdDev = StdDev/100) |>
  ggplot(aes(x = yob, y = Mean, group = group)) +
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) + 
  geom_ribbon(aes(ymin=Mean-alpha*StdDev/sqrt(Obs), 
                  ymax=Mean+alpha*StdDev/sqrt(Obs), fill = group),
              alpha = 0.35) +
  geom_ribbon(aes(ymin=Mean-alpha_2*StdDev/sqrt(Obs), 
                  ymax=Mean+alpha_2*StdDev/sqrt(Obs), fill = group),
              alpha = 0.15) +
  geom_vline(xintercept = 2009, linetype  = 2) +
  xlab("Year of Birth") + ylab("Inbreeding Coefficient") +
  scale_x_continuous(breaks = 2005:2017) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

data <- rbind(treat |> mutate(group = "treatment"), 
              control|> mutate(group = "control"))

data |> 
  filter(inbreeding > 0) |>
  mutate(post = case_when(yob<2009 ~ "2000-2009", 
                          yob>=2009 ~ "2010-2017"),
         post = factor(post, levels = c("2000-2009", "2010-2017")),
         inbreeding = inbreeding/100) |>
  ggplot(aes(x = inbreeding, y = NM)) +
  geom_jitter(aes(color = group)) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  facet_wrap(.~post, scales = "fixed")


rm(cdn, ggped, ped_kin)

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

rel_df <- left_join(rel_df, index_df, by = join_by("alt_sire" == "num_id"))


naab_line |>
  filter(sire_id %in% alt_stars$reg_id) |>
  mutate(weight = 2^(-gen_num)) |>
  select(c(reg_id, sire_id, weight, gen_num)) -> 
  control

control <- left_join(control, naab_old, by = "reg_id") 

control <- left_join(control, cdn[, c("reg_id", "inbreeding")], 
                     by = "reg_id") 

control <- left_join(control, rel_df[, c("reg_id", "pscore")], 
                     by = join_by("sire_id" == "reg_id"))

control |> 
  distinct(reg_id, .keep_all = TRUE) ->
  control

control |>
  mutate(pscore_w = pscore*weight) ->
  control

control |> 
  group_by(yob) |> 
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) |>
  summarise(inb_control = weighted.mean(inbreeding, w = pscore),
            se_control = sqrt(wtd.var(inbreeding, weights = pscore)),
            N_control = n()) |>
  data.frame() ->
  temp1

treat |> 
  group_by(yob) |> 
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) |>
  summarise(inb_treat = mean(inbreeding),
            se_treat = sd(inbreeding),
            N_treat = n()) |>
  data.frame() ->
  temp2

inb <- left_join(temp1, temp2, by = "yob");rm(temp1, temp2)
inb |> 
  pivot_longer(-yob, names_to = "variable", values_to = "values") |>
  separate(variable, into = c("var", "group"), sep = "_") |>
  pivot_wider(id_cols = c(yob, group), names_from = "var", values_from = "values") |>
  mutate(inb = inb/100, se = se/100) |>
  filter(yob < 2019) |>
  ggplot(aes(x = yob, y = inb, group = group)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) + 
  geom_ribbon(aes(ymin = inb-alpha*se/sqrt(N), 
                  ymax = inb+alpha*se/sqrt(N), fill = group),
              alpha = 0.35) +
  geom_ribbon(aes(ymin = inb-alpha_2*se/sqrt(N), 
                  ymax = inb+alpha_2*se/sqrt(N), fill = group),
              alpha = 0.15) +
  geom_vline(xintercept = 2009, linetype = 2) +
  xlab("Year of Birth") + ylab("Weighted Inbreeding Coefficient") +
  scale_x_continuous(breaks = 2003:2018) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


library(lfe) 
library(estimatr)

event <- rbind(treat |> mutate(group = 1), control |> select(-c(gen_num, pscore, pscore_w)) |> mutate(group = 0))
event |>
  mutate(post = ifelse(yob > 2010, 1, 0)) ->
  event

summary(model_1 <- lm(inbreeding ~ factor(group)+factor(post)+group:post, 
                      data = event, weights = weight))

summary(model_2 <- felm(inbreeding ~ group:factor(yob)+pta_milk+pta_fat_lb+pta_protein_lb+ 
                          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+ 
                          pta_gest_length+pta_heifer_liv+pta_efcalving+
                          pta_mastitis+pta_metritis+pta_strength+pta_ketosis+
                          pta_r_placenta+pta_rear_legs_rear+pta_foot_leg_score+
                          pta_teat_rear_place+pta_rump_angle+pta_thurl_width+
                          pta_rear_legs_side+pta_foot_angle+pta_fore_udder+
                          pta_rear_udder_height+pta_rear_udder_width+
                          pta_udder_cleft+pta_udder_depth+pta_teat_front_place+
                          pta_teat_length+pta_milk_fever+pta_stature+
                          pta_strength+pta_dairy_form+pta_body_depth|yob|0|0, 
                        data = event), robust = TRUE)

event %>%
  filter(yob > 2004 &  yob < 2018) %>%
  lm_robust(inbreeding ~ group+group:factor(yob)+pta_milk+pta_fat_lb+pta_protein_lb+ 
         pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+ 
         pta_gest_length+pta_heifer_liv+pta_efcalving+
         pta_mastitis+pta_metritis+pta_strength+pta_ketosis+
         pta_r_placenta+pta_rear_legs_rear+pta_foot_leg_score+
         pta_teat_rear_place+pta_rump_angle+pta_thurl_width+
         pta_rear_legs_side+pta_foot_angle+pta_fore_udder+
         pta_rear_udder_height+pta_rear_udder_width+
         pta_udder_cleft+pta_udder_depth+pta_teat_front_place+
         pta_teat_length+pta_milk_fever+pta_stature+
         pta_strength+pta_dairy_form+pta_body_depth, data = .,
       se_type = "stata", fixed_effects = ~yob) ->
  model_3

summary(model_3)

event %>%
  filter(yob > 2004 &  yob < 2017) %>%
  felm(inbreeding ~ group:I(yob==2011)+group:I(yob==2012)+
              group:I(yob==2013)+group:I(yob==2014)+group:I(yob==2015)+
              group:I(yob==2016)+
              pta_milk+pta_fat_lb+pta_protein_lb+ 
              pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+ 
              pta_gest_length+pta_heifer_liv+pta_efcalving+
              pta_mastitis+pta_metritis+pta_strength+pta_ketosis+
              pta_r_placenta+pta_rear_legs_rear+pta_foot_leg_score+
              pta_teat_rear_place+pta_rump_angle+pta_thurl_width+
              pta_rear_legs_side+pta_foot_angle+pta_fore_udder+
              pta_rear_udder_height+pta_rear_udder_width+
              pta_udder_cleft+pta_udder_depth+pta_teat_front_place+
              pta_teat_length+pta_milk_fever+pta_stature+
              pta_strength+pta_dairy_form+pta_body_depth|sire_id|0|0, data = .) ->
  model_4

summary(model_4)

event %>%
  filter(yob > 2004 &  yob < 2017) %>%
  lm_robust(inbreeding ~ group:factor(yob)+
              pta_milk+pta_fat_lb+pta_protein_lb+ 
              pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+ 
              pta_gest_length+pta_heifer_liv+pta_efcalving+
              pta_mastitis+pta_metritis+pta_strength+pta_ketosis+
              pta_r_placenta+pta_rear_legs_rear+pta_foot_leg_score+
              pta_teat_rear_place+pta_rump_angle+pta_thurl_width+
              pta_rear_legs_side+pta_foot_angle+pta_fore_udder+
              pta_rear_udder_height+pta_rear_udder_width+
              pta_udder_cleft+pta_udder_depth+pta_teat_front_place+
              pta_teat_length+pta_milk_fever+pta_stature+
              pta_strength+pta_dairy_form+pta_body_depth, data = .,
            se_type = "stata", fixed_effects = ~yob) ->
  model_5

summary(model_5)


event |> 
  tab(post, group)

event %>%
  lm_robust(inbreeding~group*post, data = ., cluster = controller_number+yob, 
            se_type = "stata") ->
  reg1
summary(reg1)

event %>%
  mutate(treat_post = ifelse(group == 1 & post == 1, 1, 0)) %>%
  lm_robust(inbreeding ~ group + treat_post + I(yob == 2011)+
              I(yob==2012)+I(yob==2013)+I(yob==2014)+I(yob==2015)+
              I(yob==2016), data = ., cluster = controller_number, 
            se_type = "stata") ->
  reg2
summary(reg2)

event %>%
  mutate(treat_post = ifelse(group == 1 & post == 1, 1, 0)) %>%
  lm_robust(inbreeding ~ treat_post:I(yob == 2011)+
              treat_post:I(yob==2012)+treat_post:I(yob==2013)+
              treat_post:I(yob==2014)+treat_post:I(yob==2015)+
              treat_post:I(yob==2016), data = ., 
            fixed_effects = ~controller_number+yob, 
            se_type = "stata") ->
  reg3
summary(reg3)

event %>%
  lm_robust(inbreeding ~ group:I(yob == 2011)+
              group:I(yob==2012)+group:I(yob==2013)+
              group:I(yob==2014)+group:I(yob==2015)+
              group:I(yob==2016), data = ., 
            fixed_effects = ~controller_number+yob, 
            se_type = "stata") ->
  reg4
summary(reg4)

event |> 
  filter(yob > 2004 &  yob < 2017) |>
  select(-c(reg_id, reg_name, country, naab_code, 
            controller_number, dob, aaa_codes, breed,
            genetic_codes, beta_casein, kappa_casein,
            beta_lactaglobulin, haplotypes, mgs_name, 
            status, status_alt)) |>
  group_by(sire_id, yob, group) |>
  summarise(across(c("inbreeding", "pta_milk", "pta_fat_lb", "pta_protein_lb",
           "pta_pl", "pta_dpr", "pta_hcr", "pta_ccr", 
           "pta_liv", "pta_type", "pta_gest_length", 
           "pta_heifer_liv","pta_efcalving", "pta_mastitis",
           "pta_metritis", "pta_strength", "pta_ketosis",
           "pta_r_placenta", "pta_rear_legs_rear", 
           "pta_foot_leg_score", "pta_teat_rear_place", 
           "pta_rump_angle", "pta_thurl_width", "pta_rear_legs_side",
           "pta_foot_angle", "pta_fore_udder", "pta_rear_udder_height", 
           "pta_rear_udder_width", "pta_udder_cleft", "pta_udder_depth",
           "pta_teat_front_place","pta_teat_length", "pta_milk_fever", 
           "pta_stature", "pta_strength", "pta_dairy_form", "pta_body_depth"), 
         ~ weighted.mean(.x, weights = weight, na.rm = TRUE))) |>
  ungroup() |>
  group_by(sire_id, yob) |>
  mutate(id = 1:n(),
         first_treat = ifelse(group == 1, 2011, 0)) |>
  ungroup() ->
  event_mean
  
attgt <- att_gt(yname = "inbreeding",
                tname = "yob",
                idname = "id",
                gname = "first_treat",
                panel = FALSE,
                control_group = "nevertreated",
                print_details = TRUE,
                data = event_mean)

summary(attgt)


data <- left_join(naab_old, cdn, by = "reg_id")
data |> 
  filter(inbreeding >= 0 & yob.x > 2009) |>
  ggplot(aes(x = inbreeding, y = pta_fat_lb)) +
  geom_point() +
  geom_smooth()
