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

## Pedigree building
cdn |>
  filter(reg_id_alt %in% naab$reg_id) |>
  select(c(reg_id_alt, sire_id, dam_id, mgs_id, gmgs_id, gmgd_id)) ->
  naab_1

cdn |>
  filter(reg_id_alt %in% naab$sire_id) |>
  select(c(reg_id_alt, sire_id, dam_id, mgs_id, gmgs_id, gmgd_id)) ->
  naab_2

cdn |>
  filter(reg_id_alt %in% naab$mgs_id) |>
  select(c(reg_id_alt, sire_id, dam_id, mgs_id, gmgs_id, gmgd_id)) ->
  naab_3

rbind(naab_1, naab_2, naab_3) |>
  distinct(reg_id_alt, .keep_all = TRUE) |>
  rename(reg_id = reg_id_alt) ->
  naab_alt
rm(naab_1, naab_2, naab_3)

naab_alt <- left_join(naab_alt, naab[, c("reg_id", "sire_id", "dam_id", "mgs_id", "yob")],
                      by = "reg_id")

naab_alt |>
  mutate(sire_id = ifelse(is.na(sire_id.x), sire_id.y, sire_id.x),
         dam_id = ifelse(is.na(dam_id.x), dam_id.y, dam_id.x),
         mgs_id = ifelse(is.na(mgs_id.x), mgs_id.y, mgs_id.x)) |>
  select(-c(sire_id.x, sire_id.y, dam_id.x, dam_id.y, mgs_id.x, mgs_id.y)) |>
  relocate(gmgs_id, .after = mgs_id) |>
  relocate(gmgd_id, .after = gmgs_id) ->
  naab_alt

naab_alt |>
  mutate(yob = replace(yob, reg_id == "AYCAN000000672418", 1981)) |>
  arrange(yob) ->
  naab_alt

naab_alt <- left_join(naab_alt, cdn[, c("reg_id_alt", "dob")],
                      by = join_by(reg_id == reg_id_alt))

naab_alt |>
  mutate(dob = ymd(dob),
         year_alt = year(dob)) -> 
  naab_alt

naab_alt |> 
  mutate(yob = replace(yob, is.na(yob), year_alt)) |>
  select(-year_alt) |>
  arrange(yob) ->
  naab_alt

naab |> 
  filter(!(reg_id %in% naab_alt$reg_id)) |> 
  select(c(reg_id, sire_id, dam_id)) ->
  naab_miss

# Sons, Sires and Dams
naab_alt |>
  select(c(reg_id, sire_id, dam_id)) |>
  rename(id = reg_id,
         dadid = sire_id,
         momid = dam_id) |>
  mutate(sex = "male") |>
  distinct(id, .keep_all = TRUE) ->
  ped_1

#Dams, mgs and mgd
naab_alt |>
  select(c(dam_id, mgs_id)) |>
  distinct(dam_id, .keep_all = TRUE) |>
  rename(id = dam_id,
         dadid = mgs_id) |>
  mutate(momid = "0",
         sex = "female") |>
  filter(!is.na(dadid)) ->
  ped_2

#MGS, gmgs and gmgds
naab_alt |>
  select(c(mgs_id, gmgs_id, gmgd_id)) |>
  distinct(mgs_id, .keep_all = TRUE) |>
  filter(!is.na(mgs_id)) |>
  mutate(sex = "male") |>
  rename(id = mgs_id,
         dadid = gmgs_id,
         momid = gmgd_id) ->
  ped_3

rbind(ped_1, ped_2, ped_3) |> 
  distinct(id, .keep_all = TRUE) ->
  ped
rm(ped_1, ped_2, naab_miss)

#pedcheck(ped)

ped[is.na(ped)] <- 0

ped |>
  data.frame() ->
  ped

ped <- fixParents(id = ped$id, dadid = ped$dadid, momid = ped$momid, 
                  sex  = ped$sex, missid = 0)

ped |>
  rename(ID = id,
         SIRE = dadid,
         DAM = momid) ->
  ped

ggped <- gghead(ped[, c("ID", "SIRE", "DAM")])

ggped |>
  filter(ID != 0) ->
  ggped

ped <- pedigree(id = ped$ID, dadid = ped$SIRE, momid = ped$DAM, 
                sex = ped$sex, missid = "0")


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
ped_alt |>
  mutate(ped_depth = kindepth(ped_kin)) ->
  ped_alt

ped_alt <- left_join(ped_alt, cdn[,c("reg_id_alt", "dob")],
                     by = join_by(id == reg_id_alt))
ped_alt <- left_join(ped_alt, naab[,c("reg_id", "yob")],
                     by = join_by(id == reg_id))

ped_alt |>
  mutate(dob = ymd(dob),
         yob_alt = year(dob),
         temp = ifelse(is.na(yob_alt), yob, yob_alt)) |>
  select(-c(yob, yob_alt)) |>
  rename(yob = temp) |>
  arrange(ped_depth, yob) ->
  ped_alt

ped_alt |>
  separate(id, into = c("breed", "temp"), sep = 2, remove = FALSE) |>
  separate(temp, into = c("country", "temp"), sep = 3, remove = FALSE) |>
  select(-temp) ->
  ped_alt

ped_alt <- left_join(ped_alt, naab[,c("reg_id", "reg_name")],
                     by = join_by(id == reg_id))
ped_alt <- left_join(ped_alt, cdn[,c("reg_id_alt", "reg_name")],
                     by = join_by(id == reg_id_alt))

ped_alt |>
  mutate(reg_name = ifelse(is.na(reg_name.x), reg_name.y, reg_name.x)) |>
  select(-c(reg_name.x, reg_name.y)) ->
  ped_alt

ped_alt |>
  mutate(yob = replace(yob, id == "AYCAN000000672418", 1981),
         yob = replace(yob, id == "AYUSA000000118834", 1953)) ->
  ped_alt


# Sire selection
# Criteria: Holstein bulls born between 1990 and 2010
sires_df <- data.frame(reg_id = c("HOGBR000000598172", "HOUSA000122358313",
                               "HOCAN000005470579", "HOUSA000002250783",
                               "HOCAN000010705608", "HOUSA000002290977",
                               "HOUSA000060597003", "HOCAN000006026421",
                               "HOUSA000135746776", "HODEU000000253642", 
                               "HOUSA000002249055", "HOUSA000123586443", 
                               "HOUSA000062065919", "HOUSA000064966739", 
                               "HOUSA000017349617", "HOUSA000131823833", 
                               "HOUSA000002160458", "HOUSA000002147486",
                               "HOUSA000060372887", "HOUSA000060540164",
                               "HOUSA000060996956", "HOUSA000132973942",
                               "HOUSA000120780521", "HOUSA000065917481",
                               "HOUSA000002205082", "HOUSA000123066734",
                               "HOUSA000068977120", "HOUSA000002163822",
                               "HONLD000839380546", "HOUSA000066636657", 
                               "HOCAN000005457798", "HOUSA000002265005"),
                    reg_name = c("shottle", "manfred", "rudolph", "durham",
                                 "goldwyn", "marshall", "planet", "outside",
                                 "man_o_man", "ramos", "convincer", "boliver",
                                 "super", "robust", "morty", "bolton", "patron",
                                 "duster", "toystory", "mac", "freddie",
                                 "altabaxter", "altafinley", "observer",
                                 "winchester", "titanic", "shamrock",
                                 "altaformation", "addison", "bookem",
                                 "storm", "altaaaron"))

offs <- data.frame(reg_id = NA, weight = NA)
for(j in 1:dim(sires_df)[1]){
  sire <- offspring(ggped, sires_df$reg_id[j], 1:10000)
  print(sires_df$reg_name[j])
  offspring <- data.frame(reg_id = NA, weight = NA)
  for(i in 1:length(sire$prgn)){
    data <- sire$prgn[[i]] |> 
      data.frame()
    data <- data |>
      mutate(weight = 1/2^(i),
             sire_name = sires_df$reg_name[j])
    colnames(data) <- c("reg_id", "weight", "sire_name")
    offspring <- bind_rows(offspring, data)
  }
  offspring <- offspring[-1,]
  offspring <- left_join(offspring, cdn[, c("reg_id_alt", "dob")], 
                  by = join_by(reg_id == reg_id_alt))
  offspring |> 
    filter(!is.na(dob)) |>
    mutate(dob = ymd(dob),
           yob = year(dob)) ->
    offspring
  offs <- bind_rows(offs, offspring)
}
offs


sires_df <- left_join(sires_df, naab[, c("reg_id", "yob")],
                      by = "reg_id")

sires_df |>
  rename(yob_sire = yob) ->
  sires_df

offs <- left_join(offs, sires_df, 
                  by = join_by(sire_name == reg_name))

offs |>
  filter(!is.na(reg_id.x)) |>
  rename(reg_id = reg_id.x,
         sire_id = reg_id.y)  ->
  offs

offs |>
  group_by(yob, weight) |>
  summarise(N = n()) |>
  filter(yob > 2000 & yob < 2016) |>
  mutate(weight = factor(weight)) |>
  ggplot(aes(x = yob, y = N, group = weight)) + 
  geom_line(aes(color = weight)) + 
  theme_classic()

offs |> 
  distinct(reg_id, sire_id, .keep_all = TRUE) |>
  select(c(reg_id, sire_id, weight)) |>
  pivot_wider(id_cols = "reg_id", names_from = "sire_id", 
              values_from = "weight", values_fill = 0) |>
  data.frame() ->
  rel_table

write_csv(offs, file = "../../data/superstar_relatives.csv")


offs |>
  mutate(sire_group = case_when(
    sire_name %in% c("shottle", "manfred", "rudolph", "durham",
                    "goldwyn") ~ 1,
    sire_name %in% c("marshall", "planet", "outside",
                     "man_o_man", "ramos") ~ 2,
    sire_name %in% c("convincer", "boliver",
                     "super", "robust", "morty") ~ 3,
    sire_name %in% c("bolton", "patron",
                     "duster", "toystory", "mac") ~ 4,
    sire_name %in% c("bolton", "patron",
                     "duster", "toystory", "mac") ~ 5,
    sire_name %in% c("freddie", "altabaxter", "altafinley", 
                     "observer", "winchester") ~ 5,
    sire_name %in% c("titanic", "shamrock",
                     "altaformation", "addison", "bookem",
                     "storm", "altaaaron") ~ 6)) ->
  offs

offs |> 
  filter(yob < 2016) |>
  group_by(yob, weight, sire_group) |>
  summarise(N = n()) |>
  mutate(weight = factor(weight)) |>
  ggplot(aes(x = yob, y = N, group = weight)) + 
  geom_line(aes(color = weight)) + 
  theme_classic() +
  facet_wrap(.~sire_group)

offs |>
  filter(yob < 2016 & sire_group == 1) |>
  group_by(yob, sire_name) |>
  summarise(N = n()) |>
  mutate(weight = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  ylim(c(0, 1000)) +
  theme_classic() 

offs |>
  filter(yob < 2016 & sire_group == 2) |>
  group_by(yob, sire_name) |>
  summarise(N = n()) |>
  mutate(weight = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  ylim(c(0, 1000)) +
  theme_classic() 

offs |>
  filter(yob < 2016 & sire_group == 3) |>
  group_by(yob, sire_name) |>
  summarise(N = n()) |>
  mutate(weight = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  ylim(c(0, 1000)) +
  theme_classic() 

offs |>
  filter(yob < 2016 & sire_group == 4) |>
  group_by(yob, sire_name) |>
  summarise(N = n()) |>
  mutate(weight = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  ylim(c(0, 1000)) +
  theme_classic() 

offs |>
  filter(yob < 2016 & sire_group == 5) |>
  group_by(yob, sire_name) |>
  summarise(N = n()) |>
  mutate(weight = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  ylim(c(0, 1000)) +
  theme_classic() 

offs |>
  filter(yob < 2016 & sire_group == 6) |>
  group_by(yob, sire_name) |>
  summarise(N = n()) |>
  mutate(weight = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  ylim(c(0, 1000)) +
  theme_classic() 


offs |>
  filter(yob < 2016) |>
  group_by(yob, sire_name, weight) |>
  summarise(N = sum(weight)) |>
  mutate(sire_name = factor(sire_name)) |>
  ggplot(aes(x = yob, y = N, group = sire_name)) + 
  geom_line(aes(color = sire_name)) + 
  theme_classic()

# Matching on observables
reg_stars <- offs |>
  distinct(reg_id) |>
  filter(!is.na(reg_id))

naab_old |>
  filter(yob > 1980 & yob < 2010 & breed == "HO" & num_dtrs > 0) |>
  select(reg_id, yob, starts_with("pta"), num_dtrs, num_herds) |>
  mutate(stars = ifelse(reg_id %in% sires_df$reg_id, 1, 0)) ->
  naab_match

library(MatchIt)

naab_match[is.na(naab_match)] <- 0
match_obj <- matchit(stars ~ pta_milk+pta_fat_lb++pta_protein_lb+pta_pl+
                       pta_dpr+pta_hcr+pta_ccr+pta_liv+pta_type+pta_gest_length+
                       pta_heifer_liv+pta_efcalving+pta_mastitis+pta_metritis+
                       pta_disp_abomasum+pta_ketosis+pta_stature+pta_strength+
                       pta_dairy_form+num_dtrs+num_herds,
                     data = naab_match, method = "nearest", distance ="glm",
                     replace = FALSE, verbose = TRUE)
summary(match_obj)

naab_match |>
  mutate(weights = match_obj$weights) |>
  filter(weights == 1) ->
  naab_match

naab_match |>
  filter(!(reg_id %in% sires_df$reg_id)) |>
  select(reg_id) ->
  sires_df_alt

offs_alt <- data.frame(reg_id = NA, weight = NA)
skip_to_next <- FALSE
for(j in 1:dim(sires_df_alt)[1]){
  sire <- try(offspring(ggped, sires_df_alt$reg_id[j], 1:1000), silent = TRUE)
  offspring <- data.frame(reg_id = NA, weight = NA)
  for(i in 1:length(sire$prgn)){
    data <- sire$prgn[[i]] |> data.frame() 
    data <- data |>
      mutate(weight = 1/2^(i))
    colnames(data) <- c("reg_id", "weight", "sire_name")
    offspring <- bind_rows(offspring, data)
  }
  offspring <- offspring[-1,]
  offspring <- left_join(offspring, cdn[, c("reg_id_alt", "dob")], 
                         by = join_by(reg_id == reg_id_alt))
  offspring |> 
    filter(!is.na(dob)) |>
    mutate(dob = ymd(dob),
           yob = year(dob)) ->
    offspring
  offs_alt <- bind_rows(offs_alt, offspring)
}

offs_alt |>
  group_by(yob, weight) |>
  summarise(N = n()) |>
  filter(yob > 2000 & yob < 2016) |>
  mutate(weight = factor(weight)) |>
  ggplot(aes(x = yob, y = N, group = weight)) + 
  geom_line(aes(color = weight)) + 
  theme_classic()


