library(tidyverse)
library(lfe)
library(fixest)
library(haven)

#Base scenario
yob_start <- 1997
yob_end <- 2005
ratio <- 50
percs <- c("p75", "p90", "p95", "p99")

pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]
setwd(paste0(folder, "/Box/Dairy_inbreeding/code"))

data <- list()
for(i in 1:4){
  perc <- percs[i]
  source("generate_db.R")
  
  data_full |>
    mutate(dob = ymd(dob),
           quarter = case_when(
             month(dob) %in% c(1,2,3) ~ 1,
             month(dob) %in% c(4,5,6) ~ 2,
             month(dob) %in% c(7,8,9) ~ 3,
             month(dob) %in% c(10,11,12) ~ 4),
           qob = paste0(yob, "-", quarter)) ->
    data_full
  
  data[[i]] <- data_full
}

rm(list=setdiff(ls(), "data"))  

data[[1]] %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit1

data[[2]] %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit2

data[[3]] %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
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

data[[4]] %>%
  filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                 pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                 pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                 pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                 pta_stature,pta_strength,pta_dairy_form), 
            all_vars(!is.na(.))) %>%
  filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
  feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
          pta_protein_lb+pta_scs+
          pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
          pta_gest_length+pta_heifer_liv+pta_efcalving+
          pta_mastitis+pta_metritis+pta_disp_abomasum+
          pta_ketosis+pta_r_placenta+pta_milk_fever+
          pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
        cluster = ~sire_id,
        data = .)   ->
  fit4

etable(fit1, fit2, fit3, fit4, drop = "pta_")

dict <- c("Percentile 75", "Percentile 90", "Percentile 95", "Percentile 99")
esttex(fit1, fit2, fit3, fit4, 
       drop = "pta_",
       digits  = 3,
       se.below = FALSE,
       label = "tab:table2",
       title = "Alternative cutoffs from progeny distribution",
       headers = dict,
       replace = TRUE,
       file = "../tables/robustness_tests.tex")

plot1 <- iplot(fit1)
plot2 <- iplot(fit2)
plot3 <- iplot(fit3)
plot4 <- iplot(fit4)

placebo_plot <- rbind(plot1$prms |> 
                        rownames_to_column(var = "period") |>
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 1),
                      plot2$prms |> 
                        rownames_to_column(var = "period") |>
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 2),
                      plot3$prms |> 
                        rownames_to_column(var = "period") |>
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 3),
                      plot4$prms |> 
                        rownames_to_column(var = "period") |>
                        select(c(period, estimate, ci_low, ci_high)) |>
                        mutate(model = 4))

labels <- c("Percentile 75", "Percentile 90", "Percentile 95", "Percentile 99")
placebo_plot |>
  mutate(model = factor(model, labels = labels)) |>
  ggplot(aes(x = period, y = estimate, group = model)) + 
  geom_line(aes(x = period, y = ci_low, fill = model), 
            color = "grey", size = 0.25) +
  geom_line(aes(x = period, y = ci_high, fill = model), 
            color = "grey", size = 0.25) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, color = model), 
              fill="grey", alpha = 0.25) +
  geom_point(aes(color = model), size = 1.5) +
  geom_line(aes(color = model)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = "2010", linetype = 2) + 
  theme_classic() + 
  paletteer::scale_color_paletteer_d("calecopal::superbloom2") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino")) +
  ylab("Estimate and 95% Conf. Int.") +
  xlab("Period") +
  facet_wrap(.~ model, ncol = 2, nrow  = 2, scales = "free_y")

start <- 1995:1997
end <- 2003:2005

data <- list()
for(i in 1:6){
  yob_start <- start[i]
  for(j in 1:6){
    yob_end <- end[j]
    ratio <- 50
    perc <- "p95"
    source("generate_db.R")
    
    data_full |>
      mutate(dob = ymd(dob),
             quarter = case_when(
               month(dob) %in% c(1,2,3) ~ 1,
               month(dob) %in% c(4,5,6) ~ 2,
               month(dob) %in% c(7,8,9) ~ 3,
               month(dob) %in% c(10,11,12) ~ 4),
             qob = paste0(yob, "-", quarter))  %>%
      filter_at(vars(pta_milk,pta_fat_lb,pta_protein_lb,pta_scs, pta_pl,pta_dpr, 
                     pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length, 
                     pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis, 
                     pta_disp_abomasum, pta_ketosis, pta_r_placenta, pta_milk_fever, 
                     pta_stature,pta_strength,pta_dairy_form), 
                all_vars(!is.na(.))) %>%
      filter(inbreeding >= 0 & yob > 2004 & yob < 2019) %>%
      feols(inbreeding ~ i(yob, treat, ref = 2009)+pta_milk+pta_fat_lb+
              pta_protein_lb+pta_scs+
              pta_pl+pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+
              pta_gest_length+pta_heifer_liv+pta_efcalving+
              pta_mastitis+pta_metritis+pta_disp_abomasum+
              pta_ketosis+pta_r_placenta+pta_milk_fever+
              pta_stature+pta_strength+pta_dairy_form|treat+yob+parent_company, 
            cluster = ~sire_id,
            data = .)   ->
      fit
    
    data[[paste0(start[i], "-", end[j])]] <- tidy(fit)
  }
}

bind_rows(data, .id = "period_id") |>
  #separate(period_id, into = c("yob_start", "yob_end"), sep = "-") |>
  filter(grepl('treat', term)) |>
  mutate(yob = str_split_fixed(term, ":", n = 4)[,3]) ->
  data_alt

write_csv(data_alt, file = "../data/placebo_data.csv")

data_alt <- read_csv("../data/placebo_data.csv")

alpha <- 0.95
data_alt |>
  filter(period_id %in% c("1996-2004", "1996-2005", "1996-2006",
                          "1997-2004", "1997-2005", "1997-2006",
                          "1998-2004", "1998-2005", "1998-2006")) |>
  ggplot(aes(x = yob, y = estimate, group = 1)) +
  geom_point(color = "darkred") +
  geom_line() + 
  geom_ribbon(aes(ymin = estimate-alpha*std.error, 
                  ymax = estimate+alpha*std.error),
              alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic() +
  facet_wrap(.~period_id, scales = "free_y") + 
  xlab("Year of birth") + ylab("Inbreeding rate (%)") + 
  scale_x_continuous(breaks = c(2006, 2009, 2012, 2015, 2018))


  