library(tidyverse)
library(bigrquery)
library(lubridate)
library(statar)

setwd("C:/Users/victo/Box/dairy_matching/data")

pedigree <- read_csv("pedigree_raw.csv")

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

cdn |>
  filter(inbreeding > 0 & breed %in% c("HO", "JE", "GU", "BS")) |>
  mutate(dob = ymd(dob),
         yob = year(dob),
         breed = replace(breed, breed == "HO", "Holstein"),
         breed = replace(breed, breed == "JE", "Jersey"),
         breed = replace(breed, breed == "GU", "Guernsey"),
         breed = replace(breed, breed == "BS", "Brown Swiss")) |>
  group_by(yob, breed) |>
  summarise(inb = mean(inbreeding, na.rm = TRUE)) |>
  filter(yob > 1989 & yob < 2019) |>
  ggplot(aes(x = yob, y = inb)) + 
  geom_line(aes(color = breed))+
  geom_point(aes(color = breed)) + 
  theme_classic() + 
  geom_vline(xintercept = 2010, linetype = 2) +
  xlab("Year of Birth") + ylab("Inbreeding Coefficient") +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018)) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

