library(tidyverse)

#Base scenario
yob_start <- 1997
yob_end <- 2005
ratio <- 50
perc <- "p95"

pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]
setwd(paste0(folder, "/Box/Dairy_inbreeding/code"))

<<<<<<< HEAD
source("generate_db.R")
=======
source("generate_db_rr.R")
>>>>>>> second-repo-remote/main
library(lfe)
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

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  felm(inbreeding ~ treat+treat:post|post|0|sire_id, 
       data = .)  ->
  fit5

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
  feols(inbreeding ~ i(post, treat, ref = 0)|treat+post, cluster = ~sire_id,
        data = .)   ->
  fit51

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004) %>%
  felm(inbreeding ~ treat+treat:I(yob == 2005)+treat:I(yob == 2006)+
         treat:I(yob == 2007)+treat:I(yob == 2008)+treat:I(yob == 2010)+
         treat:I(yob == 2011)+treat:I(yob == 2012)+treat:I(yob == 2013)+
         treat:I(yob == 2014)+treat:I(yob == 2015)+treat:I(yob == 2016)+
         treat:I(yob == 2017)+treat:I(yob == 2018)|yob|0|sire_id, 
       data = .)  ->
  fit6

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)|treat+yob, cluster = ~sire_id,
        data = .)   ->
  fit61

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004) %>%
  felm(inbreeding ~ treat+treat:I(yob == 2005)+treat:I(yob == 2006)+
         treat:I(yob == 2007)+treat:I(yob == 2008)+treat:I(yob == 2010)+
         treat:I(yob == 2011)+treat:I(yob == 2012)+treat:I(yob == 2013)+
         treat:I(yob == 2014)+treat:I(yob == 2015)+treat:I(yob == 2016)+
         treat:I(yob == 2017)+treat:I(yob == 2018)+pta_milk+pta_fat_lb+
         pta_protein_lb+pta_scs+pta_pl+pta_dpr+pta_hcr+
         pta_ccr+pta_liv+pta_type+
         pta_gest_length+pta_heifer_liv+pta_efcalving+
         pta_mastitis+pta_metritis+pta_disp_abomasum+
         pta_ketosis+pta_r_placenta+pta_milk_fever+
         pta_stature+pta_strength+pta_dairy_form|
         yob+parent_company|0|sire_id, 
<<<<<<< HEAD
       data = .)   ->
=======
       data = .) ->
>>>>>>> second-repo-remote/main
  fit7

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit71

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  felm(inbreeding ~ treat+treat:I(yob == 2005)+treat:I(yob == 2006)+
         treat:I(yob == 2007)+treat:I(yob == 2008)+treat:I(yob == 2010)+
         treat:I(yob == 2011)+treat:I(yob == 2012)+treat:I(yob == 2013)+
         treat:I(yob == 2014)+treat:I(yob == 2015)+treat:I(yob == 2016)+
         treat:I(yob == 2017)+treat:I(yob == 2018)+pta_milk+pta_fat_lb+
         pta_protein_lb+pta_scs+
         pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
         pta_gest_length+pta_heifer_liv+pta_efcalving+
         pta_mastitis+pta_metritis+pta_disp_abomasum+
         pta_ketosis+pta_r_placenta+pta_milk_fever+
         pta_stature+pta_strength+pta_dairy_form+
         pta_milk:post+pta_fat_lb:post+pta_protein_lb:post+pta_scs:post+
         pta_pl:post+pta_dpr:post+pta_hcr:post+pta_ccr:post+pta_liv:post+pta_type:post+
         pta_gest_length:post+pta_heifer_liv:post+pta_efcalving:post+
         pta_mastitis:post+pta_metritis:post+pta_disp_abomasum:post+
         pta_ketosis:post+pta_r_placenta:post+pta_milk_fever:post+
         pta_stature:post+pta_strength:post+pta_dairy_form:post|
         yob+parent_company|0|sire_id, 
       data = .) ->
  fit8

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
  fit81

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) %>%
  mutate(post = ifelse(yob > 2011, 1, 0),
         post = factor(post)) %>%
  felm(inbreeding ~ treat+treat:post|post|0|sire_id, 
       data = .)  ->
  fit9


data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004) %>%
  mutate(post = ifelse(yob > 2011, 1, 0),
         post = factor(post)) %>%
  felm(inbreeding ~ treat+treat:post+pta_milk+pta_fat_lb+pta_protein_lb+pta_scs+
         pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
         pta_gest_length+pta_heifer_liv+pta_efcalving+
         pta_mastitis+pta_metritis+pta_disp_abomasum+
         pta_ketosis+pta_r_placenta+pta_milk_fever+
         pta_stature+pta_strength+pta_dairy_form+
         pta_milk:post+pta_fat_lb:post+pta_protein_lb:post+pta_scs:post+
         pta_pl:post+pta_dpr:post+pta_hcr:post+pta_ccr:post+pta_liv:post+pta_type:post+
         pta_gest_length:post+pta_heifer_liv:post+pta_efcalving:post+
         pta_mastitis:post+pta_metritis:post+pta_disp_abomasum:post+
         pta_ketosis:post+pta_r_placenta:post+pta_milk_fever:post+
         pta_stature:post+pta_strength:post+pta_dairy_form:post|
         post+parent_company|0|sire_id, 
       data = .)  ->
  fit10

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) %>%
  felm(inbreeding ~ treat+treat:I(yob == 2005)+treat:I(yob == 2006)+
         treat:I(yob == 2007)+treat:I(yob == 2008)+
         treat:I(yob == 2010)+treat:I(yob == 2011)+
         treat:I(yob == 2012)+treat:I(yob == 2013)+
         treat:I(yob == 2014)+treat:I(yob == 2015)+
         treat:I(yob == 2016)+treat:I(yob == 2017)+
         treat:I(yob == 2018)+
         pta_milk+pta_fat_lb+pta_protein_lb+pta_scs+
         pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
         pta_gest_length+pta_heifer_liv+pta_efcalving+
         pta_mastitis+pta_metritis+pta_disp_abomasum+
         pta_ketosis+pta_r_placenta+pta_milk_fever+
         pta_stature+pta_strength+pta_dairy_form|
         yob+parent_company|0|sire_id, 
       data = .)  ->
  fit11

library(car)
test6 <- linearHypothesis(fit6, 
                 c("treat:I(yob == 2005)TRUE = 0", 
                   "treat:I(yob == 2006)TRUE = 0", 
                   "treat:I(yob == 2007)TRUE = 0", 
                   "treat:I(yob == 2008)TRUE = 0"),
                 vcov = vcov(fit6))

test7 <- linearHypothesis(fit7, 
                 c("treat:I(yob == 2005)TRUE = 0", 
                   "treat:I(yob == 2006)TRUE = 0", 
                   "treat:I(yob == 2007)TRUE = 0", 
                   "treat:I(yob == 2008)TRUE = 0"),
                 vcov = vcov(fit7))

test8 <- linearHypothesis(fit8, 
                 c("treat:I(yob == 2005)TRUE = 0", 
                   "treat:I(yob == 2006)TRUE = 0", 
                   "treat:I(yob == 2007)TRUE = 0", 
                   "treat:I(yob == 2008)TRUE = 0"),
                 vcov = vcov(fit8))

pval_6 <- test6$`Pr(>Chisq)`[2] |> round(digits = 3)
pval_7 <- test7$`Pr(>Chisq)`[2] |> round(digits = 3)
pval_8 <- test8$`Pr(>Chisq)`[2] |> round(digits = 3)

test61 <- linearHypothesis(fit61, 
                          c("yob::2005:treat = 0", 
                            "yob::2006:treat = 0", 
                            "yob::2007:treat = 0", 
                            "yob::2008:treat = 0"),
                          error.df = glance(fit61)$nobs-dim(tidy(fit61))[1],
                          vcov = vcov(fit61),
                          test = "F")

test71 <- linearHypothesis(fit71, 
                           c("yob::2005:treat = 0", 
                             "yob::2006:treat = 0", 
                             "yob::2007:treat = 0", 
                             "yob::2008:treat = 0"),
                           error.df = glance(fit71)$nobs-dim(tidy(fit71))[1],
                           vcov = vcov(fit71),
                           test = "F")

test81 <- linearHypothesis(fit81, 
                           c("yob::2005:treat = 0", 
                             "yob::2006:treat = 0", 
                             "yob::2007:treat = 0", 
                             "yob::2008:treat = 0"),
                           error.df = glance(fit81)$nobs-dim(tidy(fit81))[1],
                           vcov = vcov(fit81),
                           test = "F")

pval_61 <- test61$`Pr(>F)`[2] |> round(digits = 3)
pval_71 <- test71$`Pr(>F)`[2] |> round(digits = 3)
pval_81 <- test81$`Pr(>F)`[2] |> round(digits = 3)

library(stargazer)
pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]

stargazer(fit5, fit6, fit7, fit8, 
          omit = "pta_",
          table.placement = "H",
          style = "AER",
          out = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table.tex"),
          type = "latex",
          font.size = "small",
          #single.row = TRUE,
          title = "Difference-in-Differences estimates",
          label = "tab:table1",
          covariate.labels = c("treat", "$treat \\times post\\_2009$", 
                               paste0("$treat \\times\\mathbf{I}(yob = ", 2005:2008, ")$"), 
                               paste0("$treat \\times\\mathbf{I}(yob = ", 2010:2017, ")$")),
          dep.var.labels = "Inbreeding rate (\\%)",
          column.labels = c("No covariates", "No covariates", "Traits", "Traits and interactions"),
          add.lines = list(c("p-value for nonzero pre-effect", "", pval_6, pval_7, pval_8)))

etable(fit51, fit61, fit71, fit81, 
       tex = TRUE,
       keep = "treat",
       style.tex = style.tex("aer"),
       replace = TRUE,
       #se.below = FALSE,
       title = "Difference-in-Differences estimates",
       label = "tab:table1",
       extralines = list(c("p-value for nonzero pre-effect", pval_61, pval_71, pval_81)),
       headers = c("No covariates", "No covariates", "Traits", "Traits and interactions"),
       file = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table_alt.tex"))

# ATE
data_full|> 
  filter_at(vars(pta_milk, pta_fat_lb, pta_protein_lb, pta_scs, pta_pl, pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_gest_length, pta_type,
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever), 
            all_vars(!is.na(.))) |>
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) |>
  mutate(first_treat = ifelse(treat  == 1, 2010, 0),
         first_treat_alt = ifelse(treat  == 1, 2012, 0),
         post = ifelse(yob > 2009, 1, 0),
         id = as.integer(factor(reg_id, levels = unique(reg_id))),
         line_id = as.integer(factor(sire_id, levels = unique(sire_id))),
         treat = factor(treat),
         yob = factor(yob)) -> 
  data_alt 

data_alt %>%
  lm(inbreeding ~ treat*yob, data = .)   ->
  fit62

data_alt %>%
  lm(inbreeding ~ treat*yob+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form+
       factor(parent_company),
        data = .)   ->
  fit72

data_alt %>%
  lm(inbreeding ~ treat*yob+pta_milk+pta_fat_lb+
       pta_protein_lb+pta_scs+
       pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
       pta_gest_length+pta_heifer_liv+pta_efcalving+
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
       pta_dairy_form:post+factor(parent_company),
     data = .)   ->
  fit82

vcov62 <- clubSandwich::vcovCR(fit62, cluster = data_alt$sire_id, type = "CR1S")
vcov72 <- clubSandwich::vcovCR(fit72, cluster = data_alt$sire_id, type = "CR1S")
vcov82 <- clubSandwich::vcovCR(fit82, cluster = data_alt$sire_id, type = "CR1S")

library(margins)
margins(fit62, variables = "treat", 
        at = list(yob = as.character(2005:2019)), 
        data = data_alt |> filter(treat == 1), 
        vcov = vcov62,
        vce = "delta") ->
  marg62 

marg62 %>%
  summary()

margins(fit72, variables = "treat", 
        at = list(yob = as.character(2005:2019)), 
        data = data_alt |> filter(treat == 1), 
        vcov = vcov72,
        vce = "delta") ->
  marg72 

marg72 %>%
  summary()

margins(fit82, variables = "treat", 
        at = list(yob = as.character(2005:2019)), 
        data = data_alt |> filter(treat == 1), 
        vcov = vcov82,
        vce = "delta") ->
  marg82 

marg82 %>%
  summary()


library(did) 
data_full|> 
  filter_at(vars(pta_milk, pta_fat_lb, pta_protein_lb, pta_scs, pta_pl, pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_gest_length, pta_type,
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever), 
            all_vars(!is.na(.))) |>
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) |>
  mutate(first_treat = ifelse(treat  == 1, 2010, 0),
         first_treat_alt = ifelse(treat  == 1, 2012, 0),
         post = ifelse(yob > 2009, 1, 0),
         id = as.integer(factor(reg_id, levels = unique(reg_id))),
         line_id = as.integer(factor(sire_id, levels = unique(sire_id)))) -> 
  data_alt 

<<<<<<< HEAD
attgt <- att_gt(yname = "inbreeding", 
                tname = "yob", 
                idname = "id", 
                gname = "first_treat", 
                control_group = "notyettreated",
                panel = FALSE,
                bstrap = TRUE,
                biters = 1500,
                alp = 0.01,
                data = data_alt) 
=======
#attgt <- att_gt(yname = "inbreeding", 
#                tname = "yob", 
#                idname = "id", 
#                gname = "first_treat", 
#                control_group = "notyettreated",
#                panel = FALSE,
#                bstrap = TRUE,
#                biters = 1500,
#                alp = 0.01,
#                data = data_alt) 
>>>>>>> second-repo-remote/main

summary(attgt)


library(etwfe)
decomp_1 <- etwfe(fml = inbreeding~1, 
                tvar = yob, 
                tref = 2009,
                gvar = treat, 
                ivar = line_id,
                vcov = ~sire_id,
                cgroup = "never",
                data = data_alt) 
summary(decomp_1)
emfx(decomp_1, type = "calendar", by_xvar = FALSE, post_only = TRUE)

decomp_2 <- etwfe(fml = inbreeding~pta_milk+pta_fat_lb+
                  pta_protein_lb+pta_scs+pta_pl+pta_dpr+pta_hcr+pta_ccr+
                  pta_liv+pta_type+pta_gest_length+pta_heifer_liv+pta_efcalving+
                  pta_mastitis+pta_metritis+pta_disp_abomasum+
                  pta_ketosis+pta_r_placenta+pta_milk_fever+
                  pta_stature+pta_strength+pta_dairy_form, 
                tvar = yob, 
                tref = 2009,
                gvar = treat, 
                vcov = ~sire_id,
                cgroup = "never",
                data = data_alt) 
summary(decomp_2)
emfx(decomp_2, type = "calendar", by_xvar = FALSE, post_only = TRUE)


library(did2s)
data_full %>%
  filter_at(vars(pta_milk, pta_fat_lb, pta_protein_lb, pta_scs, pta_pl, pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_gest_length, pta_type,
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) %>%
  mutate(post = ifelse(yob > 2011, 1, 0),
         post = factor(post)) ->
  data_decomp


did2 <- did2s(
  data = data_decomp,
  yname = "inbreeding", 
  first_stage = ~ pta_milk+pta_fat_lb+
    pta_protein_lb+pta_scs+pta_pl+pta_dpr+pta_hcr+pta_ccr+
    pta_liv+pta_type+pta_gest_length+pta_heifer_liv+pta_efcalving+
    pta_mastitis+pta_metritis+pta_disp_abomasum+
    pta_ketosis+pta_r_placenta+pta_milk_fever+
    pta_stature+pta_strength+pta_dairy_form+pta_pl:post+
    pta_dpr:post+pta_hcr:post+pta_ccr:post+pta_liv:post+pta_type:post+
    pta_gest_length:post+pta_heifer_liv:post+pta_efcalving:post+
    pta_mastitis:post+pta_metritis:post+pta_disp_abomasum:post+
    pta_ketosis:post+pta_r_placenta:post+pta_milk_fever:post+
    pta_stature:post+pta_strength:post+pta_dairy_form:post|yob+parent_company,
  second_stage = ~ i(yob, treat, ref = 2009), 
  treatment = "treat",
  cluster_var = "sire_id")


etable(did2)
iplot(did2)

linearHypothesis(fit11, 
                 c("treat:I(yob == 2005)TRUE = 0", 
                   "treat:I(yob == 2006)TRUE = 0", 
                   "treat:I(yob == 2007)TRUE = 0", 
                   "treat:I(yob == 2008)TRUE = 0"),
                 vcov = vcov(fit11))

linearHypothesis(fit11, 
                 c("treat:I(yob == 2005)TRUE = 0", 
                   "treat:I(yob == 2006)TRUE = 0", 
                   "treat:I(yob == 2007)TRUE = 0", 
                   "treat:I(yob == 2008)TRUE = 0",
                   "treat:I(yob == 2010)TRUE = 0",
                   "treat:I(yob == 2011)TRUE = 0"),
                 vcov = vcov(fit11))


data_alt %>%
  filter(yob > 2006 & yob < 2018) %>%
  feols(inbreeding ~ treat + i(qob, treat, ref = "2009-1") + 
          pta_milk+pta_fat_lb+pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form |
          qob+parent_company, cluster = ~sire_id,
        data = .)   ->
  fit17
etable(fit17)
iplot(fit17, pt.join = TRUE)

## Sup-t critical values
library(suptCriticalValue)
set.seed(19281)

main <- function() {
  conf_level  <- 0.95
  
  #model       <- lm(Price ~ Weight + Wheelbase + Cylinders, data=Cars93)
  
  beta        <- matrix(coef(fit8)[24:35])
  vcov_matrix <- vcov(fit8)[24:35,24:35]
  std_error   <- sqrt(diag(vcov_matrix))
  
  pw_crit     <- qt(1 - ((1 - conf_level) / 2), fit8$df.residual) 
  supt_crit   <- suptCriticalValue(vcov_matrix = vcov_matrix)
  
  pw_ci_lb    <- beta - pw_crit * std_error 
  pw_ci_ub    <- beta + pw_crit * std_error 
  supt_ci_lb  <- beta - supt_crit * std_error 
  supt_ci_ub  <- beta + supt_crit * std_error 
  
  print("POINTWISE 95 PERCENT CONFIDENCE INTERVAL:")
  print(pw_ci_lb)
  print(pw_ci_ub)
  
  print("SIMULTANEOUS SUP-T 95 PERCENT CONFIDENCE INTERVAL:")
  print(supt_ci_lb)
  print(supt_ci_ub)
}

main()

library(modelsummary)
library(showtext)

back <- list(geom_vline(xintercept = 0, linetype = 2),
             geom_line(aes(y = term, x = estimate)))

add_rows <- data.frame(
  term = "2009",
  model = c("Model (2)", "Model (3)", "Model (4)"),
  estimate = c(0,0,0))
attr(add_rows, "position") = 12

modelplot(list("Model (2)" = fit6, 
               "Model (3)" = fit7, 
               "Model (4)" = fit8), 
          coef_omit = "^(?!.*TRUE)", 
          size = 1,
          coef_rename = c(as.character(seq(2005, 2008)), as.character(seq(2010,2018))),
          add_rows = add_rows,
          background = back) +
  coord_flip() +
  xlab("Inbreeding Rate Coefficients and 95% CI") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text=element_text(family="Palatino")) ->
  df


alpha <- 0.95
vcov_matrix6 <- vcov(fit6)[2:14,2:14]
vcov_matrix7 <- vcov(fit7)[24:36,24:36]
vcov_matrix8 <- vcov(fit8)[24:36,24:36]

fit6_coef <- tidy(fit6, conf.int = TRUE)[2:14,] |>
  add_row(term = "treat:I(yob == 2009)TRUE", estimate = 0, std.error = 0,
          statistic = 0, p.value = 0, conf.low = 0, 
          conf.high = 0, .before = 5) |>
  mutate(term = seq(2005, 2018), 
         model = 2,
         pw_crit = qt(1 - ((1 - alpha) / 2), fit6$df.residual),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix6),
         conf.low = replace(conf.low, term < 2009, estimate - supt_crit * std.error),
         conf.high = replace(conf.high, term < 2009, estimate + supt_crit * std.error))

fit7_coef <- tidy(fit7, conf.int = TRUE)[24:36,] |>
  add_row(term = "treat:I(yob == 2009)TRUE", estimate = 0, std.error = 0,
          statistic = 0, p.value = 0, conf.low = 0, 
          conf.high = 0, .before = 5) |>
  mutate(term = seq(2005, 2018), 
         model = 3,
         pw_crit = qt(1 - ((1 - alpha) / 2), fit7$df.residual),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix7),
         conf.low = replace(conf.low, term < 2009, estimate - supt_crit * std.error),
         conf.high = replace(conf.high, term < 2009, estimate + supt_crit * std.error))

fit8_coef <- tidy(fit8, conf.int = TRUE)[24:36,] |>
  add_row(term = "treat:I(yob == 2009)TRUE", estimate = 0, std.error = 0,
          statistic = 0, p.value = 0, conf.low = 0, 
          conf.high = 0, .before = 5) |>
  mutate(term = seq(2005, 2018), 
         model = 4,
         pw_crit = qt(1 - ((1 - alpha) / 2), fit8$df.residual),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix8),
         conf.low = replace(conf.low, term < 2009, estimate - supt_crit * std.error),
         conf.high = replace(conf.high, term < 2009, estimate + supt_crit * std.error))

rbind(fit6_coef, fit7_coef, fit8_coef) -> df_alt

df$data |>
  arrange(model, term) |>
  mutate(conf.low = df_alt$conf.low,
         conf.high = df_alt$conf.high) -> df$data
df + 
  paletteer::scale_color_paletteer_d("rtist::vangogh") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino"))


data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
  feols(inbreeding ~ i(qob, treat, ref = "2009-4")|treat+yob, cluster = ~sire_id,
        data = .)   ->
  fit12

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
  feols(inbreeding ~ i(qob, treat, ref = "2009-4")+pta_milk+pta_fat_lb+pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob, cluster = ~sire_id,
        data = .)   ->
  fit13

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  feols(inbreeding ~ i(qob, treat, ref = "2009-4")+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form+
          pta_milk:post+pta_fat_lb:post+pta_protein_lb:post+pta_scs:post+
          pta_pl:post+pta_dpr:post+pta_hcr:post+pta_ccr:post+pta_liv:post+pta_type:post+
          pta_gest_length:post+pta_heifer_liv:post+pta_efcalving:post+
          pta_mastitis:post+pta_metritis:post+pta_disp_abomasum:post+
          pta_ketosis:post+pta_r_placenta:post+pta_milk_fever:post+
          pta_stature:post+pta_strength:post+pta_dairy_form:post|treat+yob, 
        cluster = ~sire_id,
        data = .)   ->
  fit14

## Covariance matrices
vcov_matrix12 <- vcov(fit12)[1:55,1:55]
vcov_matrix13 <- vcov(fit13)[1:55,1:55]
vcov_matrix14 <- vcov(fit14)[1:55,1:55]

plot12 <- iplot(fit12)
plot12$prms |>
  rownames_to_column(var = "period") |>
  mutate(period = yq(period), 
         std.error = c(se(vcov_matrix12)[1:19],0, se(vcov_matrix12)[20:55]), 
         pw_crit = qt(1 - ((1 - alpha) / 2), fit12$nobs-fit12$nparams),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix12),
         ci_low = replace(ci_low, period < yq("2009-4"), estimate - supt_crit * std.error),
         ci_high = replace(ci_high, period < yq("2009-4"), estimate + supt_crit * std.error)) ->
  plot12$prms

plot13 <- iplot(fit13)
plot13$prms |>
  rownames_to_column(var = "period") |>
  mutate(period = yq(period), 
         std.error = c(se(vcov_matrix13)[1:19],0, se(vcov_matrix13)[20:55]), 
         pw_crit = qt(1 - ((1 - alpha) / 2), fit13$nobs-fit13$nparams),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix13),
         ci_low = replace(ci_low, period < yq("2009-4"), estimate - supt_crit * std.error),
         ci_high = replace(ci_high, period < yq("2009-4"), estimate + supt_crit * std.error)) ->
  plot13$prms

plot14 <- iplot(fit14)
plot14$prms |>
  rownames_to_column(var = "period") |>
  mutate(period = yq(period), 
         std.error = c(se(vcov_matrix14)[1:19],0, se(vcov_matrix14)[20:55]), 
         pw_crit = qt(1 - ((1 - alpha) / 2), fit14$nobs-fit14$nparams),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix14),
         ci_low = replace(ci_low, period < yq("2009-4"), estimate - supt_crit * std.error),
         ci_high = replace(ci_high, period < yq("2009-4"), estimate + supt_crit * std.error)) ->
  plot14$prms

plot14$prms |>
  ggplot(aes(x = period, y = estimate, group = 1)) + 
  geom_line(aes(x = period, y = ci_low), color = "grey", size = 0.25) +
  geom_line(aes(x = period, y = ci_high), color = "grey", size = 0.25) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill="grey", alpha=0.5) +
  geom_point(color = "blue", size = 3.5) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = yq("2010-2"), linetype = 2) + 
  theme_classic() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-1.5, 5, 0.5)) +
  ylab("Estimate and 95% Conf. Int.") +
  xlab("Period")

quarter_plot <- rbind(plot12$prms |> 
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 1),
                      plot13$prms |> 
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 2),
                      plot14$prms |> 
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 3))

quarter_plot |>
  mutate(model = factor(model, labels = c("No covariates", "PTAs", "PTAs and interactions"))) |>
  ggplot(aes(x = period, y = estimate, group = model)) + 
  geom_line(aes(x = period, y = ci_low, fill = model), color = "grey", size = 0.25) +
  geom_line(aes(x = period, y = ci_high, fill = model), color = "grey", size = 0.25) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, color = model), fill="grey", alpha=0.5) +
  geom_point(aes(color = model), size = 3.5) +
  geom_line(aes(color = model)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = yq("2010-2"), linetype = 2) + 
  theme_classic() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-1.5, 5, 0.5)) +
  paletteer::scale_color_paletteer_d("rtist::vangogh") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino")) +
  ylab("Estimate and 95% Conf. Int.") +
  xlab("Period") +
  facet_wrap(.~ model, ncol = 2, nrow  = 2)

etable(fit12, fit13, fit14, 
       tex = TRUE,
       keep = "treat",
       style.tex = style.tex("aer"),
       replace = TRUE,
       title = "Difference-in-Differences estimates (quarter fixed effects)",
       label = "tab:table3",
       headers = c("No covariates", "Traits", "Traits and interactions"),
       file = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table_quarter.tex"))

# Multiple plot at the quarter of birth level
coefplot(list(
  fit12$coeftable,
  fit13$coeftable[1:51,],
  fit14$coeftable[1:51,])
)

## Cost estimations
data_full |> 
  mutate(treat = factor(treat, labels = c("control", "treatment"))) |>
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob, treat) |> 
  summarise(inbreeding = mean(inbreeding, na.rm = TRUE)) |> 
  pivot_wider(id_cols = "yob", names_from = "treat", values_from = "inbreeding") |>
  mutate(cost_animal = 23*(treatment-control)) ->
  cost

cost$num_cattle <- c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                     9.25, 9.3, 9.4, 9.4, 9.3, 9.35)

cost |>
  mutate(total_cost = cost_animal*num_cattle) ->
  cost

sum(cost[cost$yob > 2011, "total_cost"])

data_full |> 
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob) |> 
  summarise(inbreeding_mean = mean(inbreeding, na.rm = TRUE)) |>
  mutate(cost_animal_alt = 23*(inbreeding_mean - 6.25)) ->
  cost_alt

cost <- left_join(cost, cost_alt, by = "yob")

cost |>
  mutate(total_cost_alt = cost_animal_alt*num_cattle) ->
  cost

write_csv(cost, file = "../data/cost.csv")

sum(cost_alt[cost_alt$yob > 2011, "total_cost_alt"])

# Inbreeding plot
alpha_2 <- 0.95
alpha <- 0.9
data_full |> 
  mutate(treat = factor(treat, labels = c("Control", "Treatment"))) |>
  group_by(yob, treat) |> 
  mutate(inbreeding = inbreeding/100) |>
  summarise(inb_m = mean(inbreeding, na.rm = TRUE),
            inb_se = sd(inbreeding, na.rm = TRUE),
            N = n()) |> 
  filter(yob > 2004 & yob < 2018) |>
  ggplot(aes(x = yob, y = inb_m, group = treat)) + 
  geom_line(aes(color = treat)) + 
  geom_point(aes(color = treat)) + 
  geom_ribbon(aes(ymin = inb_m-alpha_2*inb_se/sqrt(N), 
                  ymax = inb_m+alpha_2*inb_se/sqrt(N), fill = treat),
              alpha = 0.15) +
  geom_vline(xintercept = 2009, linetype = 2) +
  xlab("Year of birth") + ylab("Inbreeding rate") +
  scale_x_continuous(breaks = 2005:2017) + 
  theme_minimal() +
  paletteer::scale_fill_paletteer_d("rtist::vangogh") +
  paletteer::scale_color_paletteer_d("rtist::vangogh") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino"))


data_full |> 
  mutate(treat = factor(treat, labels = c("Control", "Treatment"))) |>
  group_by(yob, treat) |> 
  mutate(inbreeding = inbreeding/100) |>
  filter(yob > 2004 & yob < 2018) |>
  summarise(inb_m = mean(inbreeding, na.rm = TRUE),
            inb_se = sd(inbreeding, na.rm = TRUE)) ->
  data_sum

data_full |> 
  mutate(treat = factor(treat, labels = c("Control", "Treatment"))) |>
  group_by(yob, treat, sire_id) |> 
  mutate(inbreeding = inbreeding/100) |>
  summarise(inb = mean(inbreeding, na.rm = TRUE)) |> 
  filter(yob > 2004 & yob < 2018 & !is.na(inb)) |>
  ggplot(aes(x = yob, y = inb, group = sire_id)) + 
  geom_line(color = "lightgrey") + 
  geom_line(data = data_sum |> mutate(sire_id = treat), 
            aes(x = yob, y = inb_m, color = treat), size = 1.5) +
  geom_point(data = data_sum |> mutate(sire_id = treat), 
            aes(x = yob, y = inb_m, color = treat), size = 2) +
  geom_vline(xintercept = 2010, linetype = 2) +
  xlab("Year of Birth") + ylab("Inbreeding rate") +
  scale_x_continuous(breaks = 2005:2017) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
