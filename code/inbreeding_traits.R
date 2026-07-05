options(warn=-1)
library(tidyverse)
library(bigrquery)
library(lubridate)
library(statar)
library(knitr)
library(stringi)
library(broom)
library(ggrepel)
library(ggfortify)
library(ggh4x)

#folder <- "C:/Users/victorf2"
folder <- "C:/Users/victo"

setwd(paste0(folder, "/Box/dairy_matching/code/AISS data"))

bq_auth(email = "victorf2@illinois.edu")

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
  breed,
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
"

naab_aiss <- bq_project_query(projectid, query)
naab_aiss <- bq_table_download(naab_aiss)

naab_aiss <- naab_aiss %>%
  separate(period, into = c("year", "per_num"), sep = "-", remove = FALSE) |>
  mutate(dob = ymd(dob),
         yob = year(dob))

naab_aiss <- naab_aiss |>
  mutate(id_number = stringr::str_replace(id_number, "\\.", ""),
         id_number = str_pad(id_number, width = 12, side = "left", pad = "0"),
         reg_id = paste0(breed, country, id_number))

naab_aiss |> 
  mutate(period = ym(period),
         year = year(period),
         age = year-yob) ->
  naab_aiss

naab_aiss|> 
  group_by(reg_id) |> 
  fill(status, .direction = "down") |>
  ungroup() ->
  naab_aiss

naab_aiss |>
  mutate(year_proof = year(period)) ->
  naab_aiss

cdn <- read.csv(paste0(folder, "/Box/Dairy Industry in US/Data/Network data/cdn_data.csv"))

cdn |> 
  distinct(reg_id, .keep_all = TRUE) |>
  tab(country)

naab_aiss <- inner_join(naab_aiss, cdn[c("reg_id_alt", "inbreeding", "relationship")], by = join_by(reg_id == reg_id_alt))
naab_aiss |> 
  group_by(status) |> 
  summarise(inb = mean(inbreeding, na.rm = TRUE), 
            N = n())

naab_aiss |>
  distinct(reg_id, period, .keep_all = TRUE) ->
  naab_aiss


#naab_aiss |>
#  group_by(reg_id) |>
#  arrange(period) |>
#  slice_tail(n = 1) |>
#  ungroup() ->
#  naab_unique

naab_aiss |>
  group_by(reg_id) |>
  arrange(period) |>
  slice_head(n = 1) |>
  ungroup() ->
  naab_unique

naab_unique |> 
  filter(inbreeding >= 0 & breed == "HO" & !is.na(status)) |>
  mutate(post = case_when(yob<2009 ~ "2000-2009", 
                          yob>=2009 & yob < 2015 ~ "2010-2015", 
                          yob >= 2015 ~ "2015-2020"),
         post = factor(post, levels = c("2000-2009", "2010-2015", "2015-2020")),
         inbreeding = inbreeding/100,
         status_alt = case_when(
           status == "A" ~ "Active",
           status == "G" ~ "Genomic",
           status == "F" ~ "Foreign"))  |>
  ggplot(aes(x = inbreeding, y = pta_protein_lb)) +
  geom_jitter(aes(color = status_alt), alpha = 0.35) +
  geom_smooth(method = "lm", color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Inbreeding rate") + ylab("Pounds of protein in milk PTA") +
  facet_wrap(.~post, scales = "fixed")

naab_unique |> 
  filter(inbreeding >= 0 & breed == "HO" & !is.na(status)) |>
  mutate(post = ifelse(yob < 2009, "Pre-2009", "Post-2009"),
         post = factor(post, levels = c("Pre-2009", "Post-2009")),
         inbreeding = inbreeding/100,
         status_alt = case_when(
           status == "A" ~ "Active",
           status == "G" ~ "Genomic",
           status == "F" ~ "Foreign"))  |>
  ggplot(aes(x = inbreeding, y = pta_pl)) +
  geom_jitter(aes(color = status_alt), alpha = 0.35) +
  geom_smooth(method = "lm", color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Inbreeding rate") + ylab("Productive Life PTA") +
  facet_wrap(.~post, scales = "fixed")

naab_unique |> 
  filter(inbreeding >= 0 & !is.na(status) & breed == "HO") |>
  mutate(post = case_when(yob<2009 ~ "2000-2009", 
                          yob>=2009 & yob < 2015 ~ "2010-2015", 
                          yob >= 2015 ~ "2015-2020"),
         post = factor(post, levels = c("2000-2009", "2010-2015", "2015-2020")),
         inbreeding = inbreeding/100,
         status_alt = case_when(
           status == "A" ~ "Active",
           status == "G" ~ "Genomic",
           status == "F" ~ "Foreign"))  |>
  ggplot(aes(x = inbreeding, y = pta_dpr)) +
  geom_jitter(aes(color = status_alt), alpha = 0.35) +
  geom_smooth(method = "lm", color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Inbreeding rate") + ylab("Daughter Pregnancy Rate PTA") +
  facet_wrap(.~post, scales = "fixed")

naab_unique |> 
  filter(inbreeding >= 0 & breed == "HO" & !is.na(status)) |>
  mutate(post = ifelse(yob < 2009, "pre-2009", "post-2009"),
         post = factor(post, levels = c("pre-2009", "post-2009")))  |>
  ggplot(aes(x = inbreeding, y = pta_pl)) +
  geom_jitter(aes(color = status), alpha = 0.35) +
  geom_smooth(method = "lm") + 
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(.~post, scales = "fixed")

naab_unique |> 
  filter(inbreeding >= 0 & breed == "HO" & !is.na(status)) |>
  mutate(post = ifelse(yob < 2009, "pre-2009", "post-2009"),
         post = factor(post, levels = c("pre-2009", "post-2009")))  |>
  ggplot(aes(x = inbreeding, y = TPI)) +
  geom_jitter(color = "darkred", alpha = 0.35) +
  geom_smooth(method = "lm") + 
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(.~post, scales = "fixed")


naab_unique |> 
  filter(inbreeding >= 0 & breed == "HO" & !is.na(status)) |>
  mutate(post = ifelse(yob < 2009, "pre-2009", "post-2009"),
         post = factor(post, levels = c("pre-2009", "post-2009"))) |>
  ggplot(aes(x = inbreeding, y = NM)) +
  geom_jitter(color = "darkred", alpha = 0.35) +
  geom_smooth(method = "lm") + 
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic() +
  facet_wrap(.~post, scales = "fixed")


naab_unique |> 
  filter(inbreeding >= 0 & !is.na(status) & 
           breed %in% c("HO", "JE", "GU", "BS")) |>
  mutate(post = ifelse(yob < 2009, "pre-2009", "post-2009"),
         post = factor(post, levels = c("pre-2009", "post-2009"))) |>
  ggplot(aes(x = inbreeding, y = CM)) +
  geom_jitter(aes(color = breed)) +
  geom_smooth(method = "lm") + 
  theme_classic() +
  facet_wrap(.~post, scales = "fixed")

naab_unique |> 
  filter(inbreeding >= 0 & yob > 2000 & 
           !is.na(status) & breed == "HO") |>
  ggplot(aes(x = inbreeding, y = NM)) +
  geom_jitter(color = "darkred", alpha = 0.35) +
  geom_smooth(method = "lm") + 
  theme_classic() +
  facet_wrap(.~yob, scales = "fixed")

naab_unique |>
  filter(inbreeding >= 0 & !is.na(TPI) & breed == "HO" & yob > 2000) |>
  mutate(inb_bin = cut(inbreeding, breaks = seq(0, 40, by = 5), include.lowest = TRUE)) |>
  group_by(inb_bin, yob) |>
  summarise(TPI = mean(TPI, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = inb_bin, y = TPI, group = yob)) + 
  geom_point(color = "darkred", alpha = 0.35) +
  geom_line() +
  theme_classic() +
  facet_wrap(.~yob, scales = "fixed")


## Traits plots
naab_unique |> 
  filter(inbreeding >= 0  & !is.na(status) & breed == "HO") |>
  mutate(post = case_when(yob < 2009 ~ "A", 
                          yob >= 2009 & yob < 2015 ~ "B", 
                          yob >= 2015 ~ "C"),
         inbreeding = inbreeding/100,
         status_alt = case_when(
           status == "A" ~ "Active",
           status == "G" ~ "Genomic",
           status == "F" ~ "Foreign"))  |>
  select(c(inbreeding, post, status_alt, pta_milk, pta_protein_lb, pta_fat_lb)) |>
  pivot_longer(pta_milk:pta_fat_lb, names_to = "pta", values_to = "values") |> 
  ggplot(aes(x = inbreeding, y = values)) +
  geom_jitter(aes(color = status_alt), alpha = 0.35) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Inbreeding rate") + ylab("Productivity Traits") +
  facet_wrap(pta~post, scales = "free", 
             labeller = as_labeller(c(pta_fat_lb = "Fat yield", 
                                      pta_milk = "Milk yield", 
                                      pta_protein_lb = "Protein yield",
                                      A = "2000-2009", B = "2010-2015", C = "2016-2020"))) + 
  facetted_pos_scales(
    x = rep(list(
      scale_x_continuous(limits = c(0, 0.25)),
      scale_x_continuous(limits = c(0, 0.25)),
      scale_x_continuous(limits = c(0, 0.25))), 
      each = 3),
    y = rep(list(
      scale_y_continuous(limits = c(-50, 250)),
      scale_y_continuous(limits = c(-1500, 5000)),
      scale_y_continuous(limits = c(-25, 160))), 
      each = 3))

naab_unique |> 
  filter(inbreeding >= 0  & inbreeding < 30 & !is.na(status) & breed == "HO") |>
  mutate(post = case_when(yob < 2009 ~ "A", 
                          yob >= 2009 & yob < 2015 ~ "B", 
                          yob >= 2015 ~ "C"),
         inbreeding = inbreeding/100,
         status_alt = case_when(
           status == "A" ~ "Active",
           status == "G" ~ "Genomic",
           status == "F" ~ "Foreign"))  |>
  select(c(inbreeding, post, status_alt, pta_pl, pta_type, pta_scs)) |>
  pivot_longer(pta_pl:pta_scs, names_to = "pta", values_to = "values")  |> 
  ggplot(aes(x = inbreeding, y = values)) +
  geom_jitter(aes(color = status_alt), alpha = 0.35) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Inbreeding rate") + ylab("Health Traits") +
  facet_wrap(pta~post, scales = "free", labeller = 
               as_labeller(c(pta_pl = "Productive life", pta_type = "Type",
                             pta_scs = "Somatic Cell Score",
                             A = "2000-2009", B = "2010-2015", C = "2016-2020"))) +
  facetted_pos_scales(
    x = rep(list(
      scale_x_continuous(limits = c(0, 0.25)),
      scale_x_continuous(limits = c(0, 0.25)),
      scale_x_continuous(limits = c(0, 0.25))), 
      each = 3),
    y = rep(list(
      scale_y_continuous(limits = c(-5, 15)),
      scale_y_continuous(limits = c(2.4, 4)),
      scale_y_continuous(limits = c(-2, 7))
    ), each = 3))

naab_unique |> 
  filter(inbreeding >= 0  & inbreeding < 30 & !is.na(status) & breed == "HO") |>
  mutate(post = case_when(yob < 2009 ~ "A", 
                          yob >= 2009 & yob < 2015 ~ "B", 
                          yob >= 2015 ~ "C"),
         inbreeding = inbreeding/100,
         status_alt = case_when(
           status == "A" ~ "Active",
           status == "G" ~ "Genomic",
           status == "F" ~ "Foreign"))  |>
  select(c(inbreeding, post, status_alt, TPI, NM, FM)) |>
  pivot_longer(TPI:FM, names_to = "index", values_to = "values")  |> 
  ggplot(aes(x = inbreeding, y = values)) +
  geom_jitter(aes(color = status_alt), alpha = 0.35) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), color = "black") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Inbreeding rate") + ylab("Indices") +
  facet_wrap(index~post, scales = "free", labeller = 
               as_labeller(c(TPI = "Total Productivity Index", 
                             NM = "Net Merit",
                             FM = "Fluid Merit",
                             A = "2000-2009", B = "2010-2015", C = "2016-2020"))) +
  facetted_pos_scales(
    x = rep(list(
      scale_x_continuous(limits = c(0, 0.25)),
      scale_x_continuous(limits = c(0, 0.25)),
      scale_x_continuous(limits = c(0, 0.25))), 
      each = 3),
    y = rep(list(
      scale_y_continuous(limits = c(-500, 1500)),
      scale_y_continuous(limits = c(-400, 2000)),
      scale_y_continuous(limits = c(500, 3500))
    ), each = 3))

# Companies
companies <- read_csv("../../data/company_info.csv")
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

naab_aiss <- left_join(naab_aiss, companies, by = "primary_stud_code")

naab_aiss |>
  mutate(parent_company = ifelse(is.na(parent_company), company_name, parent_company),
         company_size = ifelse(parent_company %in% c("Select Sires", "Semex Alliance", "ABS Global", "STgenetics",
                                                     "Alta Genetics", "Genex", "CRV Holding"), "Large", "Small"),
         company_date = ifelse(parent_company %in%  c("AA Breeders", "ABS Global", 
                                                      "Alpenseme-Federazione Prov. Le Allevatori-Trento", 
                                                      "Alta Genetics", "Ambreez, NZ, Ltd.", "Androgenics", "CRV Holding", 
                                                      "Central Valley Dairy Breeders", "Cogent Breeding Ltd.", 
                                                      "Cooperative Agricole D'Insemination Artificielle de Crehen UNECO", "EastGen Inc.",
                                                      "Excelsior Farms", "Galaxie Genetics Reproductive Center, Inc.", "Genes Diffusion",
                                                      "Genex", "Golden State Breeders", "Hawkeye Breeders", "Hoffman A.I. Breeders", 
                                                      "Inseme SPA", "Interglobe Genetics", "JLG Enterprises", "KI Samen b.v.", 
                                                      "Livestock Improvement Corporation Ltd.", "Masterrind GmbH",  
                                                      "North American Breeders, Inc.", "Rocky Mountain Sire Services, Inc.", "STgenetics", 
                                                      "Select Sires", "Semex Alliance", "Southeastern Semen Services, Inc.", "Synetics",  
                                                      "Triple Crown Sires", "Universal Genetics Ltd.", "Urus", "WESTGEN",  
                                                      "Zimmerman's Custom Freezing", "['INTERMIZOO S.p.a.' 'INTERMIZOO S.p.A.']", 
                                                      "Centro Provinciale per la Fecondazione Artificiale", "Dependa-Bull Genetics",
                                                      "Genetic Connection", "Inseme, SPA", "VOST (Verein Ostfriesischer Stammviehzuchter e.G.",
                                                      "Belmont Breeders", "RBB (Rinderproduktion Berlin-Brandenburg GmbH)",  
                                                      "RPN (Rinderproduktion Niedersachsen Gmbh)", "RSA (Rinderzuchtverband Sachsen-Anhalt eG)", 
                                                      "RSH (Rinderzucht Schleswig-Holstein eG)", "RUW (Rinder-Union West e.G.)", 
                                                      "WEU (Weser-EMS-Union eG)", "C.O.F.A. (Cooperativa di Fecondazione Artificiale)", 
                                                      "Nederlandse Vereniging voor KI Kampen","Viking Genetics", 
                                                      "Union Regionale des Cooperaties d'Elevage de I'Ouest (URECO)", "Cogent Canada Corp.", 
                                                      "Swissgenetics/AI Center Muelligen", "CBS - Czech Breeding Services s.r.o.",
                                                      "Central Wisconsin Semen Freezing Service", "Rinderbesamungs-Genossenschaft Memmingen", 
                                                      "Shore Genetics, Inc.", "El Toro Genetics-Bill Hillier", "Global Ag Alliance, Inc.", 
                                                      "Natural SPOL-SRO", "Robert Bignami-Brentwood Farms",  
                                                      "Southern Idaho Reproductive Enterprises (S.I.R.E)", "CIA Alsace Genetique/UNECO", 
                                                      "CIA Mayenne UNECO", "Centro Tori Chiacchierini Di Chiacchierini Anna", 
                                                      "Flatness International, Inc.", "MIDA TEST", "Origenplus", "Xenetica Fontao S.A."), "existing", "new")) ->
  naab_aiss

naab_aiss |>
  group_by(reg_id) |>
  arrange(period) |>
  slice_head(n = 1) |>
  ungroup() ->
  naab_unique

naab_unique |>
  group_by(year_proof, company_size) |>
  summarise(pta_milk = mean(pta_milk, na.rm = TRUE)) |>
  ggplot(aes(x = year_proof, y = pta_milk)) + 
  geom_point(aes(color = company_size)) + 
  geom_line(aes(color = company_size)) + 
  theme_classic() + 
  geom_vline(xintercept =  2009, linetype = 2) +  
  xlab("Year of first proof") + 
  ylab("Average Milk yield PTA") + 
  scale_x_continuous(breaks = seq(2000, 2020, 5))


alpha <- qnorm(0.99)
naab_aiss |>
  group_by(reg_id) |>
  arrange(period) |>
  slice_head(n = 1) |>
  ungroup() |> 
  mutate(inbreeding = inbreeding/100) |>
  group_by(year_proof) |>
  summarise(TPI_m = mean(TPI, na.rm = TRUE),
            TPI_sd = sd(TPI, na.rm = TRUE), 
            TPI_N = n(),
            inb_m = mean(inbreeding, na.rm = TRUE),
            inb_sd = sd(inbreeding, na.rm = TRUE),
            inb_N = n()) |>
  pivot_longer(-year_proof, names_to = "var", values_to = "values") |>
  separate(var, into = c("variable", "stat"), sep = "_") |>
  pivot_wider(id_cols = c(year_proof, variable), 
               names_from = "stat", values_from = "values") |>
  ggplot(aes(x = year_proof, y = m)) + 
  geom_point(aes(color = variable), size = 3) + 
  geom_line(aes(color = variable), linewidth = 1) + 
  geom_ribbon(aes(ymin = m-alpha*sd/sqrt(N), 
                  ymax = m+alpha*sd/sqrt(N)),
              alpha = 0.15) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "none") +
  geom_vline(xintercept =  2009, linetype = 2) +
  facet_wrap(.~variable, scales = "free_y", 
             labeller = as_labeller(c(inb = "Inbreeding rate", 
                             TPI = "Total Productivity Index"))) +
  xlab("Year of first proof") + 
  ylab("Average Values")  +
  scale_x_continuous(breaks = seq(2000, 2020, 2))