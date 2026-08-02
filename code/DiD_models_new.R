<<<<<<< HEAD
#library(tidyverse)
=======
library(tidyverse)
>>>>>>> second-repo-remote/main

#Base scenario
yob_start <- 1997
yob_end <- 2005
yob_prg <- 2010
ratio <- 50
perc <- "p95"

<<<<<<< HEAD
#pwd <- getwd()
#folder <- str_split(pwd, "/Dairy_inbreeding")[[1]][1]
#setwd(paste0(folder, "/Dairy_inbreeding/code"))
=======
pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]
setwd(paste0(folder, "/Box/Dairy_inbreeding/code"))
>>>>>>> second-repo-remote/main

source("generate_db_rr.R")
library(fixest)

data_full |>
  mutate(dob = ymd(dob),
         quarter = case_when(
           month(dob) %in% c(1,2,3) ~ 1,
           month(dob) %in% c(4,5,6) ~ 2,
           month(dob) %in% c(7,8,9) ~ 3,
           month(dob) %in% c(10,11,12) ~ 4),
<<<<<<< HEAD
         qob = paste0(yob, "-", quarter)) ->
=======
         qob = paste0(yob, "-", quarter),
         company_name = ifelse(is.na(company_name), "missing", company_name)) ->
>>>>>>> second-repo-remote/main
  data_full

data_full |> 
  group_by(treat, yob) |>
  summarise(N= n()) |>
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
  feols(inbreeding ~ i(post, treat, ref = 0)|treat+post, cluster = ~sire_id,
        data = .)   ->
  fit1

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
  fit2

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
  fit3

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


library(car)
test2 <- linearHypothesis(fit2, 
                          c("yob::2005:treat = 0", 
                            "yob::2006:treat = 0", 
                            "yob::2007:treat = 0", 
                            "yob::2008:treat = 0"),
                          error.df = glance(fit2)$nobs-dim(tidy(fit2))[1],
                          vcov = vcov(fit2),
                          test = "F")

test3 <- linearHypothesis(fit3, 
                           c("yob::2005:treat = 0", 
                             "yob::2006:treat = 0", 
                             "yob::2007:treat = 0", 
                             "yob::2008:treat = 0"),
                           error.df = glance(fit3)$nobs-dim(tidy(fit3))[1],
                           vcov = vcov(fit3),
                           test = "F")

test4 <- linearHypothesis(fit4, 
                           c("yob::2005:treat = 0", 
                             "yob::2006:treat = 0", 
                             "yob::2007:treat = 0", 
                             "yob::2008:treat = 0"),
                           error.df = glance(fit4)$nobs-dim(tidy(fit4))[1],
                           vcov = vcov(fit4),
                           test = "F")

pval_2 <- test2$`Pr(>F)`[2] |> round(digits = 3)
pval_3 <- test3$`Pr(>F)`[2] |> round(digits = 3)
pval_4 <- test4$`Pr(>F)`[2] |> round(digits = 3)

pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]

<<<<<<< HEAD
etable(fit1, fit2, fit3, fit4, keep = "treat")
=======
n_clus <- c(fitstat(fit1, "g") |> as.numeric(), 
            fitstat(fit2, "g") |> as.numeric(), 
            fitstat(fit3, "g") |> as.numeric(), 
            fitstat(fit4, "g") |> as.numeric())

etable(fit1, fit2, fit3, fit4, keep = "treat",
       extralines = list("Number of clusters" = n_clus))
>>>>>>> second-repo-remote/main
etable(fit1, fit2, fit3, fit4, 
       tex = TRUE,
       keep = "treat",
       style.tex = style.tex("aer"),
       replace = TRUE,
<<<<<<< HEAD
       title = "Difference-in-Differences estimates from Equation \\ref{eq:inb_eq}",
       label = "tab:table1",
       extralines = list("p-value for nonzero pre-effect"= c("", pval_2, pval_3, pval_4)),
       headers = c("No covariates", "No covariates", "Traits", "Traits and interactions"),
       file = "../tables/results_table_alt.tex")

# ATE
data_full|> 
=======
       title = "Difference-in-Differences estimates from Equation \\ref{eq:inb\\_eq}",
       label = "tab:table1",
       extralines = list("p-value for nonzero pre-effect"= c("", pval_2, pval_3, pval_4),
                         "Number of clusters" = n_clus),
       headers = c("No covariates", "No covariates", "Traits", "Traits and interactions"),
       file = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table_alt.tex"))


## Add firm ID
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
  feols(inbreeding ~ i(post, treat, ref = 0)|treat+post, 
        cluster = ~sire_id+parent_company,
        data = .)   ->
  fit1c

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)|treat+yob, 
        cluster = ~sire_id+parent_company,
        data = .)   ->
  fit2c

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
        cluster = ~sire_id+parent_company,
        data = .)   ->
  fit3c

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
        cluster = ~sire_id+parent_company,
        data = .)   ->
  fit4c

n_clus <- c(fitstat(fit1c, "g") |> as.numeric(), 
            fitstat(fit2c, "g") |> as.numeric(), 
            fitstat(fit3c, "g") |> as.numeric(), 
            fitstat(fit4c, "g") |> as.numeric())

etable(fit1c, fit2c, fit3c, fit4c, keep = "treat",
       extralines = list("Number of clusters" = n_clus))
etable(fit1c, fit2c, fit3c, fit4c, 
       tex = TRUE,
       keep = "treat",
       style.tex = style.tex("aer"),
       replace = TRUE,
       title = "Difference-in-Differences estimates from Equation \\ref{eq:inb\\_eq}, using two-way clustering",
       label = "tab:table1c",
       extralines = list("p-value for nonzero pre-effect"= c("", pval_2, pval_3, pval_4),
                         "Number of clusters" = n_clus),
       headers = c("No covariates", "No covariates", "Traits", "Traits and interactions"),
       file = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table_firm.tex"))


# ATE
data_full |> 
>>>>>>> second-repo-remote/main
  filter_at(vars(pta_milk, pta_fat_lb, pta_protein_lb, pta_scs, pta_pl, pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_gest_length, pta_type,
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever), 
            all_vars(!is.na(.))) |>
  filter(inbreeding >= 0 & yob > 2004 & yob < 2020) |>
  mutate(first_treat = ifelse(treat  == 1, 2010, 0),
         first_treat_alt = ifelse(treat  == 1, 2012, 0),
         post = ifelse(yob > 2009, 1, 0),
         post = factor(post),
         id = as.integer(factor(reg_id, levels = unique(reg_id))),
         line_id = as.integer(factor(sire_id, levels = unique(sire_id))),
         treat = factor(treat),
         yob = factor(yob)) -> 
  data_alt 

data_alt %>%
  lm(inbreeding ~ treat*post, data = .)   ->
  fit12

data_alt %>%
  lm(inbreeding ~ treat*yob, data = .)   ->
  fit22

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
  fit32

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
  fit42

vcov12 <- clubSandwich::vcovCR(fit12, cluster = data_alt$sire_id, type = "CR1S")
vcov22 <- clubSandwich::vcovCR(fit22, cluster = data_alt$sire_id, type = "CR1S")
vcov32 <- clubSandwich::vcovCR(fit32, cluster = data_alt$sire_id, type = "CR1S")
vcov42 <- clubSandwich::vcovCR(fit42, cluster = data_alt$sire_id, type = "CR1S")

library(margins)
library(marginaleffects)

#margins(fit12, variables = "treat", 
#        at = list(post = "1"), 
#        data = data_alt |> filter(treat == 1), 
#        vcov = vcov12,
#        vce = "delta") ->
#  marg12

#marg12 %>%
#  tidy(conf.int = TRUE) %>%
#  mutate(model = 1) ->
#  mfx_1

#margins(fit22, variables = "treat", 
#        at = list(yob = as.character(2005:2019)), 
#        data = data_alt |> filter(treat == 1), 
#        vcov = vcov22,
#        vce = "delta") ->
#  marg22 

#marg22 %>%
# tidy(conf.int = TRUE) %>%
#  mutate(model = 2) ->
#  mfx_2

#margins(fit32, variables = "treat", 
#        at = list(yob = as.character(2005:2019)), 
#        data = data_alt |> filter(treat == 1), 
#        vcov = vcov32,
#        vce = "delta") ->
#  marg32 

#marg32 %>%
#  tidy(conf.int = TRUE) %>%
#  mutate(model = 3) ->
#  mfx_3

#margins(fit42, variables = "treat", 
#        at = list(yob = as.character(2005:2019)), 
#        data = data_alt |> filter(treat == 1), 
#        vcov = vcov42,
#        vce = "delta") ->
#  marg42 

#marg42 %>%
#  tidy(conf.int = TRUE) %>%
#  mutate(model = 4) ->
#  mfx_4

#rbind(mfx_2, mfx_3, mfx_4) |>
#  mutate(model = factor(model, labels = c("No covariates", "PTAs", "PTAs and interactions"))) |>
#  ggplot(aes(x = at.value, y = estimate, group = 1)) + 
#  geom_line(aes(x = at.value, y = conf.low), color = "grey", size = 0.25) +
#  geom_line(aes(x = at.value, y = conf.high), color = "grey", size = 0.25) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill="grey", alpha=0.5) +
#  geom_point(aes(color = model), size = 3.5) +
#  geom_line(aes(color = model)) +
#  geom_hline(yintercept = 0) + 
#  geom_vline(xintercept = "2010", linetype = 2) + 
#  theme_classic() + 
#  paletteer::scale_color_paletteer_d("rtist::vangogh") +
#  theme(legend.title = element_blank(),
#        legend.position = "bottom",
#        text = element_text(family = "Palatino")) +
#  ylab("AME and 95% Conf. Int.") +
#  xlab("Year") +
#  facet_wrap(.~ model, ncol = 2, nrow  = 2)

#data_alt |>
#  mutate(yob = relevel(yob, ref = "2009")) ->
#  data_alt

#avg_comparisons(
#  fit22,
#  variables = list(yob = "reference"),
#  newdata = subset(data_alt, treat == 1),
#  vcov = vcov22) ->
#  ate_1

#avg_comparisons(
#  fit32,
#  variables =  list(yob = "reference"),
#  newdata = subset(data_alt, treat == 1),
#  vcov = vcov32) ->
#  ate_2

#avg_comparisons(
#  fit42,
#  variables = list(yob = "reference"),
#  newdata = subset(data_alt, treat == 1 & post == "1"),
#  vcov = vcov42) ->
#  ate_3


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


library(etwfe)
decomp_1 <- etwfe(fml = inbreeding~1, 
                tvar = yob, 
                tref = 2009,
                gvar = treat, 
                ivar = line_id,
                vcov = ~sire_id,
                cgroup = "never",
                data = data_alt) 
tidy(decomp_1)
#emfx(decomp_1, type = "calendar")

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
tidy(decomp_2)
#emfx(decomp_2, type = "calendar")


## Sup-t critical values
library(suptCriticalValue)
set.seed(19281)

main <- function() {
  conf_level  <- 0.95
  
  #model       <- lm(Price ~ Weight + Wheelbase + Cylinders, data=Cars93)
  
  beta        <- matrix(coef(fit2)[1:16])
  vcov_matrix <- vcov(fit2)[1:16,1:16]
  std_error   <- sqrt(diag(vcov_matrix))
  
  pw_crit     <- qt(1 - ((1 - conf_level) / 2), 
                    glance(fit2)$nobs - length(fit2$coefficients)) 
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

#main()

library(modelsummary)
library(showtext)

back <- list(geom_vline(xintercept = 0, linetype = 2),
             geom_line(aes(y = term, x = estimate)))

add_rows <- data.frame(
  term = "2009",
  model = c("Model (2)", "Model (3)", "Model (4)"),
  estimate = c(0,0,0))
attr(add_rows, "position") = 12

modelplot(list("Model (2)" = fit2, 
               "Model (3)" = fit3, 
               "Model (4)" = fit4), 
          coef_omit = "pta_", 
          size = 1,
          coef_rename = c(as.character(seq(2005, 2008)), 
                          as.character(seq(2010,2019))),
          add_rows = add_rows,
          background = back) + 
  geom_hline(yintercept = "2009", linetype = 3) +
  coord_flip() +
  xlab("Inbreeding Rate Coefficients and 95% CI") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text=element_text(family="Palatino")) ->
  df


alpha <- 0.95
vcov_matrix2 <- vcov(fit2)[1:14,1:14]
vcov_matrix3 <- vcov(fit3)[1:14,1:14]
vcov_matrix4 <- vcov(fit4)[1:14,1:14]

fit2_coef <- tidy(fit2, conf.int = TRUE)[1:14,] |>
  add_row(term = "yob::2009:treat", estimate = 0, std.error = 0,
          statistic = 0, p.value = 0, conf.low = 0, 
          conf.high = 0, .before = 5) |>
  mutate(term = seq(2005, 2019), 
         model = 2,
         pw_crit = qt(1 - ((1 - alpha) / 2), 
                      glance(fit2)$nobs - length(coef(fit2))),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix2),
         conf.low = replace(conf.low, term < 2009, estimate - supt_crit * std.error),
         conf.high = replace(conf.high, term < 2009, estimate + supt_crit * std.error))

fit3_coef <- tidy(fit3, conf.int = TRUE)[1:14,] |>
  add_row(term = "yob::2009:treat", estimate = 0, std.error = 0,
          statistic = 0, p.value = 0, conf.low = 0, 
          conf.high = 0, .before = 5) |>
  mutate(term = seq(2005, 2019), 
         model = 3,
         pw_crit = qt(1 - ((1 - alpha) / 2), 
                      glance(fit3)$nobs - length(coef(fit3))),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix3),
         conf.low = replace(conf.low, term < 2009, estimate - supt_crit * std.error),
         conf.high = replace(conf.high, term < 2009, estimate + supt_crit * std.error))

fit4_coef <- tidy(fit4, conf.int = TRUE)[1:14,] |>
  add_row(term = "yob::2009:treat", estimate = 0, std.error = 0,
          statistic = 0, p.value = 0, conf.low = 0, 
          conf.high = 0, .before = 5) |>
  mutate(term = seq(2005, 2019), 
         model = 4,
         pw_crit = qt(1 - ((1 - alpha) / 2),
                      glance(fit4)$nobs - length(coef(fit4))),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix4),
         conf.low = replace(conf.low, term < 2009, estimate - supt_crit * std.error),
         conf.high = replace(conf.high, term < 2009, estimate + supt_crit * std.error))

rbind(fit2_coef, fit3_coef, fit4_coef) -> df_alt

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
  fit5

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
  fit6

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
  fit7

## Covariance matrices
vcov_matrix5 <- vcov(fit5)[1:55,1:55]
vcov_matrix6 <- vcov(fit6)[1:55,1:55]
vcov_matrix7 <- vcov(fit7)[1:55,1:55]

plot5 <- iplot(fit5)
plot5$prms |>
  rownames_to_column(var = "period") |>
  mutate(period = yq(period), 
         std.error = c(se(vcov_matrix5)[1:19],0, se(vcov_matrix5)[20:55]), 
         pw_crit = qt(1 - ((1 - alpha) / 2), fit5$nobs-fit5$nparams),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix5),
         ci_low = replace(ci_low, period < yq("2009-4"), estimate - supt_crit * std.error),
         ci_high = replace(ci_high, period < yq("2009-4"), estimate + supt_crit * std.error)) ->
  plot5$prms

plot6 <- iplot(fit6)
plot6$prms |>
  rownames_to_column(var = "period") |>
  mutate(period = yq(period), 
         std.error = c(se(vcov_matrix6)[1:19],0, se(vcov_matrix6)[20:55]), 
         pw_crit = qt(1 - ((1 - alpha) / 2), fit6$nobs-fit6$nparams),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix6),
         ci_low = replace(ci_low, period < yq("2009-4"), estimate - supt_crit * std.error),
         ci_high = replace(ci_high, period < yq("2009-4"), estimate + supt_crit * std.error)) ->
  plot6$prms

plot7 <- iplot(fit7)
plot7$prms |>
  rownames_to_column(var = "period") |>
  mutate(period = yq(period), 
         std.error = c(se(vcov_matrix7)[1:19],0, se(vcov_matrix7)[20:55]), 
         pw_crit = qt(1 - ((1 - alpha) / 2), fit7$nobs-fit7$nparams),
         supt_crit = suptCriticalValue(vcov_matrix = vcov_matrix7),
         ci_low = replace(ci_low, period < yq("2009-4"), estimate - supt_crit * std.error),
         ci_high = replace(ci_high, period < yq("2009-4"), estimate + supt_crit * std.error)) ->
  plot7$prms

plot7$prms |>
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

quarter_plot <- rbind(plot5$prms |> 
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 1),
                      plot6$prms |> 
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 2),
                      plot7$prms |> 
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

etable(fit5, fit6, fit7, 
       tex = TRUE,
       keep = "treat",
       style.tex = style.tex("aer"),
       replace = TRUE,
       title = "Difference-in-Differences estimates (quarter fixed effects)",
       label = "tab:table3",
       headers = c("No covariates", "Traits", "Traits and interactions"),
<<<<<<< HEAD
       file = "../tables/results_table_quarter.tex")
=======
       file = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table_quarter.tex"))
>>>>>>> second-repo-remote/main

### Cost benefits analysis

data_full |>
  mutate(NM_cor = NM-40.11*inbreeding) ->
  data_full

# benefits
num_cattle <- c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
<<<<<<< HEAD
                9.25, 9.3, 9.4, 9.4, 9.3, 9.35)
=======
                     9.25, 9.3, 9.4, 9.4, 9.3, 9.35)
>>>>>>> second-repo-remote/main


data_full |> 
  filter(yob > 2005 & yob < 2020) |>
  group_by(yob) |>
  summarise(NM_cor = mean(NM_cor, na.rm = TRUE),
            NM = mean(NM_unadj, na.rm = TRUE)) |>
  ungroup() |>
  arrange(yob) |> 
  mutate(num_cattle = num_cattle, 
         NM = num_cattle*NM,
         NM_total = num_cattle*NM_cor) ->
  benefit

r <- 0.05
disc <- c(1/(1+r), 1/(1+r)^2, 1/(1+r)^3, 1/(1+r)^4, 1/(1+r)^5, 
          1/(1+r)^6, 1/(1+r)^7, 1/(1+r)^8)

benefit[benefit$yob > 2011,]$NM_total %*% disc

## Cost estimations
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

cost[cost$yob > 2011,]$total_cost %*% disc

data_full |> 
  filter(yob > 2005 & yob < 2020) |> 
  group_by(yob) |> 
  summarise(inbreeding = mean(inbreeding, na.rm = TRUE)) |>
  mutate(cost_animal = 40.11*(inbreeding - 6.25)) ->
  cost

cost$num_cattle <- c(9.05, 9.15, 9.15, 9.318, 9.204, 9.133, 9.202, 9.233, 
                     9.25, 9.3, 9.4, 9.4, 9.3, 9.35)

cost |>
  mutate(total_cost = cost_animal*num_cattle) ->
  cost

cost[cost$yob > 2011,]$total_cost %*% disc
sum(cost[cost$yob > 2011, "total_cost"])/sum(benefit[benefit$yob > 2011, "NM_total"])

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
  filter(yob > 2004 & yob < 2020) |>
  ggplot(aes(x = yob, y = inb_m, group = treat)) + 
  geom_line(aes(color = treat)) + 
  geom_point(aes(color = treat)) + 
  geom_ribbon(aes(ymin = inb_m-alpha*inb_se/sqrt(N), 
                  ymax = inb_m+alpha*inb_se/sqrt(N), fill = treat),
              alpha = 0.15) +
  geom_vline(xintercept = 2009, linetype = 2) +
  xlab("Year of birth") + ylab("Inbreeding rate") +
  scale_x_continuous(breaks = 2005:2019) + 
  scale_y_continuous(labels = scales::percent) +
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
<<<<<<< HEAD
             aes(x = yob, y = inb_m, color = treat), size = 2) +
=======
            aes(x = yob, y = inb_m, color = treat), size = 2) +
>>>>>>> second-repo-remote/main
  geom_vline(xintercept = 2010, linetype = 2) +
  xlab("Year of Birth") + ylab("Inbreeding rate") +
  scale_x_continuous(breaks = 2005:2017) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


data_full |> 
  group_by(yob) |> 
  summarise(pta_milk = mean(pta_milk, na.rm = TRUE)) |> 
  filter(yob > 2004 & yob < 2018) |>
  ggplot(aes(x = yob, y = pta_milk)) + 
  geom_line(color = "blue") + 
  geom_vline(xintercept = 2010, linetype = 2) +
  xlab("Year of Birth") + ylab("pta_milk") +
  scale_x_continuous(breaks = 2005:2017) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# Net Merit
data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form, NM_unadj), 
            all_vars(!is.na(.))) %>%
  filter(yob > 2004 & yob < 2020) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  feols(NM_unadj ~ i(post, treat, ref = 0)|treat+post, cluster = ~sire_id,
        data = .)   ->
  fit8

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form, NM_unadj), 
            all_vars(!is.na(.))) %>%
  filter(yob > 2004 & yob < 2020) %>%
  feols(NM_unadj ~ i(yob, treat, ref = 2009)|treat+yob, cluster = ~sire_id,
        data = .)   ->
  fit9

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form, NM_unadj), 
            all_vars(!is.na(.))) %>%
  filter(yob > 2004 & yob < 2020) %>%
  feols(NM_unadj ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit10

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form, NM_unadj), 
            all_vars(!is.na(.))) %>%
  filter(yob > 2004 & yob < 2020) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  feols(NM_unadj ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
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
  fit11

<<<<<<< HEAD
etable(fit8, fit9, fit10, fit11)
=======
etable(fit8, fit9, fit10, fit11)
>>>>>>> second-repo-remote/main
