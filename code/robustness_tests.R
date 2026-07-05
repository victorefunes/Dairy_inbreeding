library(tidyverse)

#Base scenario
yob_start <- 1997
yob_end <- 2005
yob_prg <- 2011
ratio <- 50
perc <- "p95"

pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]
setwd(paste0(folder, "/Box/Dairy_inbreeding/code"))

source("generate_db_alt.R")
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

etable(fit1, fit2, fit3, fit4, keep = "treat")
etable(fit1, fit2, fit3, fit4, 
       tex = TRUE,
       keep = "treat",
       style.tex = style.tex("aer"),
       replace = TRUE,
       title = "Difference-in-Differences estimates from alternative scenario",
       label = "tab:table4",
       extralines = list("p-value for nonzero pre-effect"= c("", pval_2, pval_3, pval_4)),
       headers = c("No covariates", "No covariates", "Traits", "Traits and interactions"),
       file = paste0(folder, "/Box/Dairy_inbreeding/tables/results_table_robust.tex"))

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

#Alternative scenario
yob_start <- 1997
yob_end <- 2005
ratio <- 100
perc <- "p90"

setwd("C:/Users/victorf2/Box/Dairy_inbreeding/")
source("./code/generate_db.R")
library(lfe)
library(fixest)

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) %>%
  mutate(post = ifelse(yob > 2009, 1, 0),
         post = factor(post)) %>%
  feols(inbreeding ~ i(post, treat, ref = 0)|treat+post, cluster = ~sire_id,
        data = .)   ->
  fit5

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)|treat+yob, cluster = ~sire_id,
        data = .)   ->
  fit6

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit7

data_full %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2018) %>%
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
  fit8
etable(fit5, fit6, fit7, fit8)