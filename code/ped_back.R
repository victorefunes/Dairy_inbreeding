options(warn=-1)
library(tidyverse)
library(bigrquery)
library(lubridate)
library(statar)
library(stringi)
library(broom)
library(ggroups)
library(kinship2)

setwd("C:/Users/victo/Box/dairy_matching/code/AISS data")

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

cdn <- read.csv("C:/Users/victo/Box/Dairy Industry in US/Data/Network data/cdn_data.csv")

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

# NAAB line
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

# Lineages
naab_line |> 
  group_by(sire_id) |> 
  summarise(N = n()) |> 
  arrange(desc(N)) ->
  sires

sires <- left_join(sires, naab[, c("reg_id", "reg_name", "yob")],
                       by = join_by(sire_id == reg_id))

sires |>
  mutate(reg_name = replace(reg_name, sire_id == "HOUSA000000819476", "WEBER BURKE CYCLONE"),
         yob = replace(yob, sire_id == "HOUSA000000819476", 1940),
         reg_name = replace(reg_name, sire_id == "HOCAN000000260753", "ROSAFE PEARL HANNIBAL"),
         yob = replace(yob, sire_id == "HOCAN000000260753", 1953),
         reg_name = replace(reg_name, sire_id == "JEUSA000000399447", "WELCOME VOLUNTEER"),
         yob = replace(yob, sire_id == "HOUSA000001271810", 1939),
         reg_name = replace(reg_name, sire_id == "HOUSA000000848777", "OSBORNDALE TY VIC"),
         yob = replace(yob, sire_id == "HOUSA000000848777", 1941),
         reg_name = replace(reg_name, sire_id == "JEUSA000000488392", "JESTER STANDARD ADVANCER"),
         yob = replace(yob, sire_id == "JEUSA000000488392", 1940),
         reg_name = replace(reg_name, sire_id == "BSUSA000000090827", "ROYAL MERIDIAN OF LEE'S HILL"),
         yob = replace(yob, sire_id == "BSUSA000000090827", 1947),
         reg_name = replace(reg_name, sire_id == "HOUSA000000882933", "PABST GOVERNOR"),
         yob = replace(yob, sire_id == "HOUSA000000882933", 1943),
         reg_name = replace(reg_name, sire_id == "BSUSA000000071151", "THE LAIRD OF LEE'S HILL"),
         yob = replace(yob, sire_id == "BSUSA000000071151", 1945),
         reg_name = replace(reg_name, sire_id == "HODEU000000133479", "LUKAS"),
         yob = replace(yob, sire_id == "HODEU000000133479", 1992),
         reg_name = replace(reg_name, sire_id == "AYUSA000000091883", "BALIG BRUNO"),
         yob = replace(yob, sire_id == "AYUSA000000091883", 1947),
         reg_name = replace(reg_name, sire_id == "HOCAN000000198998", "A.B.C. REFLECTION SOVEREIGN"),
         yob = replace(yob, sire_id == "HOCAN000000198998", 1946),
         reg_name = replace(reg_name, sire_id == "HOCAN000000239045", "SEILING DOUBLE TRIUMPH"),
         yob = replace(yob, sire_id == "HOCAN000000239045", 1952),
         reg_name = replace(reg_name, sire_id == "JEUSA000000506147", "SPARKLE SUPREME"),
         yob = replace(yob, sire_id == "JEUSA000000506147", 1947),
         reg_name = replace(reg_name, sire_id == "GUUSA000000458786", "COLDSPRINGS B R FORCASTER"),
         reg_name = replace(reg_name, sire_id == "HOUSA000001161476", "WISEACRES BURKE STATESMAN"),
         yob = replace(yob, sire_id == "HOUSA000001161476", 1951),
         reg_name = replace(reg_name, sire_id == "HOCAN000000281397", "ROYBROOK ACE"),
         yob = replace(yob, sire_id == "HOCAN000000281397", 1962),
         reg_name = replace(reg_name, sire_id == "HOUSA000001104074", "WINTHERTUR APOLLO"),
         yob = replace(yob, sire_id == "HOUSA000001104074", 1950),
         reg_name = replace(reg_name, sire_id == "HOCAN000000265607", "ROMANDALE SHALIMAR"),
         yob = replace(yob, sire_id == "HOCAN000000265607", 1957),
         reg_name = replace(reg_name, sire_id == "HOUSA000001723121", "NEHLS CHIEF CRUSADER"),
         yob = replace(yob, sire_id == "HOUSA000001723121", 1976),
         reg_name = replace(reg_name, sire_id == "HOUSA000000837650", "CARNATION IMPERIAL MADCAP LAD"),
         yob = replace(yob, sire_id == "HOUSA000000837650", 1941),
         reg_name = replace(reg_name, sire_id == "HOUSA000001086797", "BROWNS MASTER VOYAGEUR"),
         yob = replace(yob, sire_id == "HOUSA000001086797", 1948),
         reg_name = replace(reg_name, sire_id == "HOUSA000000846425", "M F SIR PRIDE ADANTHA"),
         yob = replace(yob, sire_id == "HOUSA000000846425", 1941), 
         reg_name = replace(reg_name, sire_id == "HONZL000000081221", "BRIGHTWATER D C CARL"),
         yob = replace(yob, sire_id == "HONZL000000081221", 1980),
         reg_name = replace(reg_name, sire_id == "HOUSA000000973771", "SUTTEN OAKS SEXTON PRIDE"),
         yob = replace(yob, sire_id == "HOUSA000000973771", 1946),
         reg_name = replace(reg_name, sire_id == "HOUSA000002237388", "BESNE BUCK"),
         yob = replace(yob, sire_id == "HOUSA000002237388", 1986),
         reg_name = replace(reg_name, sire_id == "HOCAN000000212300", "SPRING FARM FOND HOPE"),
         yob = replace(yob, sire_id == "HOCAN000000212300", 1948),
         reg_name = replace(reg_name, sire_id == "HOCAN000000121004", "INKA SUPREME REFLECTION"),
         yob = replace(yob, sire_id == "HOCAN000000121004", 1937),
         reg_name = replace(reg_name, sire_id == "HODEU000345785578", "LICHTBLICK-RED-ET"),
         yob = replace(yob, sire_id == "HODEU000345785578", 2000),
         reg_name = replace(reg_name, sire_id == "HOUSA000017156650", "250 GIBBON"),
         yob = replace(yob, sire_id == "HOUSA000017156650", 1991),
         reg_name = replace(reg_name, sire_id == "HOUSA000072102057", "BRANDVALE STOIC DAMIEN-ET"),
         yob = replace(yob, sire_id == "HOUSA000072102057", 2014),
         reg_name = replace(reg_name, sire_id == "HOUSA000000878257", "CARNATION IMPERIAL EMPEROR"),
         yob = replace(yob, sire_id == "HOUSA000000878257", 1943),
         reg_name = replace(reg_name, sire_id == "HOAUS000001547601", "CURRAJUGLE GONZO"),
         yob = replace(yob, sire_id == "HOAUS000001547601", 2009),
         reg_name = replace(reg_name, sire_id == "HOUSA000000714704", "SENSATION MARATHON LAD"),
         yob = replace(yob, sire_id == "HOUSA000000714704", 1934),
         reg_name = replace(reg_name, sire_id == "HOUSA000000859213", "PABST ROAMER"),
         yob = replace(yob, sire_id == "HOUSA000000859213", 1942),
         reg_name = replace(reg_name, sire_id == "HOCAN000000137532", "MONTVIC RAG APPLE MARKSMAN"),
         yob = replace(yob, sire_id == "HOCAN000000137532", 1940),
         reg_name = replace(reg_name, sire_id == "HOITA004902063469", "ALPAR STADEL ELAYO-RED-ET"),
         yob = replace(yob, sire_id == "HOITA004902063469", 2001),
         reg_name = replace(reg_name, sire_id == "HOUSA000000879145", "DUNLOGGIN STRATH VAR"),
         yob = replace(yob, sire_id == "HOUSA000000879145", 1943),
         reg_name = replace(reg_name, sire_id == "HOUSA000001157409", "MEISEGEIER SCOTTY GINGER"),
         yob = replace(yob, sire_id == "HOUSA000001157409", 1951),
         reg_name = replace(reg_name, sire_id == "HOUSA000001080016", "ZIMMERMAN ROYAL STAR ALAN"),
         yob = replace(yob, sire_id == "HOUSA000001080016", 1949),
         reg_name = replace(reg_name, sire_id == "HONLD000117720006", "STADEL-RED"),
         yob = replace(yob, sire_id == "HONLD000117720006", 1994),
         reg_name = replace(reg_name, sire_id == "HOUSA000000835921", "WINTERTHUR SELECT FBS WALLACE"),
         yob = replace(yob, sire_id == "HOUSA000000835921", 1941),
         reg_name = replace(reg_name, sire_id == "HOUSA000001079736", "SKOKIE GOLDEN PRINCE"),
         yob = replace(yob, sire_id == "HOUSA000001079736", 1949),
         reg_name = replace(reg_name, sire_id == "HOUSA000001085978", "SEARSFARM DEAN ADA IMPERIAL"),
         yob = replace(yob, sire_id == "HOUSA000001085978", 1949),
         reg_name = replace(reg_name, sire_id == "HOUSA000000860768", "FOBES NETHERLAND ORM KORNDYKE"),
         yob = replace(yob, sire_id == "HOUSA000000860768", 1941),
         reg_name = replace(reg_name, sire_id == "HOUSA000000901195", "PABST REGAL"),
         yob = replace(yob, sire_id == "HOUSA000000901195", 1944),
         reg_name = replace(reg_name, sire_id == "HOUSA000000943802", "CARNATION HOMESTEAD REVELATION"),
         yob = replace(yob, sire_id == "HOUSA000000943802", 1945),
         reg_name = replace(reg_name, sire_id == "HOUSA000001068870", "WIS WHIRLWIND"),
         yob = replace(yob, sire_id == "HOUSA000001068870", 1949),
         reg_name = replace(reg_name, sire_id == "HOUSA000001087974", "SHIAWANA MUTUAL PAUL 17TH"),
         yob = replace(yob, sire_id == "HOUSA000001087974", 1949),
         reg_name = replace(reg_name, sire_id == "HOCAN000000249530", "ROSAFE SIGNET"),
         yob = replace(yob, sire_id == "HOCAN000000249530", 1954)) ->
  sires



# AISS data
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

# Pedigree
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


naab_sample <- left_join(naab_line, naab[, c("reg_id", "yob")],
                         by = join_by(sire_id == reg_id)) |>
  rename(yob_sire = yob)

naab_sample <- left_join(naab_sample, naab[, c("reg_id", "yob")],
                         by = "reg_id") 

p95_sons <- naab_sample |> 
  filter(yob_sire > 1998 & yob_sire < 2005 & gen_num == 1) |> 
  tab(sire_id) |> 
  arrange(desc(Freq.)) |> 
  data.frame() |> 
  rename(N = Freq.) |> 
  sum_up(N, d = TRUE) |>
  data.frame() |>
  select(p95) |>
  as.integer()

naab_sample |> 
  filter(yob_sire > 1998 & yob_sire < 2005 & gen_num == 1) |> 
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

# Matching estimators
library(MatchIt)

data_1 <- base_sample |> 
  filter(breed == "HO") |>
  select(c(reg_id, reg_name, yob, pta_milk, pta_fat_lb,
           pta_protein_lb, pta_scs, pta_pl, pta_dpr, 
           pta_hcr, pta_ccr, pta_liv, pta_type, pta_gest_length,
           pta_heifer_liv, pta_efcalving, pta_mastitis, pta_metritis,
           pta_disp_abomasum, NM, CM, FM, GM, superstar, num_dtrs, 
           num_herds, pta_strength, pta_mobility)) |>
  data.frame()

data_1[is.na(data_1)] <- 0
match_data_1 <- matchit(superstar ~ pta_milk+pta_fat_lb++pta_protein_lb+pta_pl+
                          pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+pta_gest_length+
                          pta_heifer_liv+pta_efcalving+pta_mastitis+pta_metritis+
                         pta_strength+pta_mobility+num_dtrs+num_herds,
                     data = data_1, method = "nearest", distance ="glm",
                     replace = FALSE, verbose = TRUE, ratio = 5)
summary(match_data_1)
  
data_1[match_data_1$match.matrix |> 
           t() |> 
           as.numeric(), c("reg_id", "reg_name")] -> 
  alt_stars


#naab_line |>
#  filter(sire_id %in% alt_stars$reg_id ) |>
#  tab(sire_id) |>
#  filter(Freq. > 50) |>
#  select(sire_id) -> 
#  sires_list

#alt_stars |>
#  filter(reg_id %in% sires_list$sire_id) ->
#  alt_stars

offs <- data.frame(reg_id = NA, weight = NA)
for(j in 1:dim(alt_stars)[1]){
  sire <- tryCatch(offspring(ggped, alt_stars$reg_id[j], 1:10000), error = function(e) -999)
  print(alt_stars$reg_name[j])
  offspring <- data.frame(reg_id = NA, weight = NA)
  for(i in 1:length(sire$prgn)){
    data <- sire$prgn[[i]] |> 
      data.frame()
    data <- data |>
      mutate(weight = 1/2^(i),
             sire_name = alt_stars$reg_name[j])
    colnames(data) <- c("reg_id", "weight", "sire_name")
    offspring <- bind_rows(offspring, data)
  }
  offspring <- offspring[-1,]
  offspring <- left_join(offspring, naab[, c("reg_id", "yob")], 
                         by = "reg_id")
  offspring |> 
    filter(!is.na(yob)) ->
    offspring
  offs <- bind_rows(offs, offspring)
}
offs |> 
  filter(!is.na(reg_id)) ->
  offs

offs

superstars <- sires |> 
  filter(superstar == 1) |>
  select(sire_id)
superstars <- left_join(superstars, naab[, c("reg_id", "reg_name")],
                        by = join_by(sire_id == reg_id))


offs2 <- data.frame(reg_id = NA, weight = NA)
for(j in 1:dim(superstars)[1]){
  sire <- tryCatch(offspring(ggped, superstars$sire_id[j], 1:10000), error = function(e) -999)
  print(superstars$reg_name[j])
  offspring <- data.frame(reg_id = NA, weight = NA)
  for(i in 1:length(sire$prgn)){
    data <- sire$prgn[[i]] |> 
      data.frame()
    data <- data |>
      mutate(weight = 1/2^(i),
             sire_name = alt_stars$reg_name[j])
    colnames(data) <- c("reg_id", "weight", "sire_name")
    offspring <- bind_rows(offspring, data)
  }
  offspring <- offspring[-1,]
  offspring <- left_join(offspring, naab[, c("reg_id", "yob")], 
                         by = "reg_id")
  offspring |> 
    filter(!is.na(yob)) ->
    offspring
  offs2 <- bind_rows(offs2, offspring)
}
offs2 |> 
  filter(!is.na(reg_id)) ->
  offs2

offs2 |>
  filter(!(reg_id %in% offs$reg_id)) ->
  offs2

offs <- left_join(offs, cdn[, c("reg_id", "inbreeding")], 
                  by = "reg_id") 
offs2 <- left_join(offs2, cdn[, c("reg_id", "inbreeding")], 
                  by = "reg_id")

library(Hmisc)
offs |> 
  group_by(yob) |> 
  filter(inbreeding >= 0) |>
  summarise(inb_control = weighted.mean(inbreeding, w = weight),
            se_control = sqrt(wtd.var(inbreeding, weights = weight)),
            N_control = n()) |>
  filter(yob > 2003 & yob < 2018) |> 
  data.frame() ->
  temp1

offs2 |> 
  group_by(yob) |> 
  filter(inbreeding >= 0) |>
  summarise(inb_treat = weighted.mean(inbreeding, w = weight),
            se_treat = sqrt(wtd.var(inbreeding, weights = weight)),
            N_treat = n()) |>
  filter(yob > 2003 & yob < 2018) |> 
  data.frame() ->
  temp2

inb <- left_join(temp1, temp2, by = "yob");rm(temp1, temp2)
inb |> 
  pivot_longer(-yob, names_to = "variable", values_to = "values") |>
  separate(variable, into = c("var", "group"), sep = "_") |>
  pivot_wider(id_cols = c(yob, group), names_from = "var", values_from = "values") |>
  filter(yob < 2016) |>
  ggplot(aes(x = yob, y = inb, group = group)) + 
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) + 
  geom_ribbon(aes(ymin = inb-1.96*se/sqrt(N), 
                  ymax = inb+1.96*se/sqrt(N), fill = group),
              alpha = 0.15) +
  geom_vline(xintercept = 2010) +
  xlab("Year of Birth") + ylab("Weighted Inbreeding Coefficient") +
  scale_x_continuous(breaks = 2003:2018) + 
  theme_classic()

offs |> 
  group_by(yob) |> 
  sum_up(inbreeding, d = TRUE) |> 
  ungroup() |> 
  filter(yob > 2003 & yob < 2018) |>
  select(-Variable) |>
  mutate(group = "control") ->
  temp1

offs2 |> 
  group_by(yob) |> 
  sum_up(inbreeding, d = TRUE) |> 
  ungroup() |> 
  filter(yob > 2003 & yob < 2018) |> 
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
  theme_classic() + 
  facet_wrap(.~statistic, scales = "free_y")

stats |>
  select(c(yob, group, Mean, StdDev, Obs)) |>
  ggplot(aes(x = yob, y = Mean, group = group)) +
  geom_line(aes(color = group)) + 
  geom_point(aes(color = group)) +
  geom_ribbon(aes(ymin=Mean-1.96*StdDev/sqrt(Obs), 
                  ymax=Mean+1.96*StdDev/sqrt(Obs), fill = group),
              alpha = 0.15) +
  geom_vline(xintercept = 2010) +
  xlab("Year of Birth") + ylab("Inbreeding Coefficient") +
  scale_x_continuous(breaks = 2003:2018) + 
  theme_classic() 

inb_avg <- left_join(naab[,c("reg_id", "yob")], cdn[,c("reg_id", "inbreeding")],
                     by = "reg_id")
inb_avg |> 
  filter(!is.na(inbreeding)) |>
  group_by(yob) |>
  sum_up(inbreeding, d = TRUE) |> 
  data.frame() |>
  filter(yob > 2000 & yob < 2018) |>
  ggplot(aes(x = yob, y = Mean)) +
  geom_line(color = "pink") + 
  geom_point(color = "black") +
  geom_ribbon(aes(ymin=Mean-1.96*StdDev/sqrt(Obs), 
                  ymax=Mean+1.96*StdDev/sqrt(Obs)),
              alpha = 0.15, color = "pink") +
  geom_vline(xintercept = 2010) +
  xlab("Year of Birth") + ylab("Inbreeding Coefficient") +
  theme_classic() 


#naab_sample |> 
#  filter(yob_sire > 1998 & yob_sire < 2005 & gen_num == 1) |> 
#  tab(sire_id) |> 
#  arrange(desc(Freq.)) |> 
#  data.frame() |> 
#  select(c(sire_id, Freq.)) |>
#  rename(N = Freq.) ->
#  lines

naab_sample |> 
  filter(sire_id %in% sires$sire_id) ->
  lines

lines |>
  group_by(sire_id) |>
  mutate(weight = 2^(-gen_num)) |>
  ungroup() ->
  lines

lines <- left_join(lines, naab_old, by = "reg_id")
lines <- left_join(lines, cdn[, c("reg_id", "inbreeding")], by = "reg_id")

lines |>
  select(-c(yob.y, pta_protein_lb_rel, pta_fat_lb_rel, pta_scs_rel, pta_pl_rel,
            pta_dpr_rel, pta_hcr_rel, pta_ccr_rel, pta_scr_rel, pta_liv_rel,
            pta_type_rel, pta_type_dtrs, pta_type_herds, pta_gest_length_rel, 
            pta_gest_length_obs, pta_gest_length_dtrs, pta_heifer_liv_rel,
            pta_efcalving_rel, pta_efcalving_obs, pta_efcalving_dtrs,
            pta_mastitis_rel, pta_metritis_rel, pta_disp_abomasum_rel,
            pta_ketosis_rel, pta_r_placenta_rel, pta_milk_fever_rel,
            pta_milk_speed_rel, sire_ce_rel, sire_ce_obs, daughter_ce_rel,
            sire_sb_rel, sire_sb_obs, daughter_sb_rel, daughter_sb_obs,
            feed_saved_rel, NM_rel, NM_percentile, pta_milk_speed, pta_mobility,
            pta_foot_leg_score, pta_scr)) |>
  rename(yob = yob.x) |>
  group_by(sire_id, yob) |>
  summarise(across(c(starts_with("pta"), inbreeding), weighted.mean, 
                   weights = weight,
                   na.rm = TRUE)) |>
  ungroup() ->
  lines

lines |> 
  filter(!is.na(pta_milk) & yob < 2017) |>
  arrange(sire_id, yob) ->
  lines

lines |> 
  tab(sire_id) |> 
  data.frame() |> 
  arrange(desc(Freq.)) |>
  filter(Freq. > 5) |> 
  select(sire_id) ->
  top_lines

lines |>
  filter(sire_id %in% top_lines$sire_id & inbreeding >= 0) |> 
  ggplot(aes(x = yob, y = inbreeding)) + 
  geom_line(aes(fill = sire_id), color = "black", alpha = 0.35) + 
  geom_smooth() +
  theme_classic() + 
  scale_x_continuous(breaks = 2000:2016) +
  theme(legend.position = "none") 

lines |>
  filter(sire_id %in% top_lines$sire_id) |> 
  ggplot(aes(x = yob, y = pta_milk)) + 
  geom_line(aes(color = sire_id)) + 
  geom_smooth() +
  theme_classic() + 
  scale_x_continuous(breaks = 2000:2016) +
  theme(legend.position = "none") 

