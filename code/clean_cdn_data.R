library(tidyverse)
library(haven)
library(statar)
library(lubridate)
library(bigrquery)
library(rvest)

pwd <- getwd()
folder <- str_split(pwd, "/Box/Dairy_inbreeding")[[1]][1]
setwd(paste0(folder, "/Box/Dairy_inbreeding/code"))

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
  arrange(dob) |>
  rename(temp = reg_id,
         reg_id =  reg_id_alt) |>
  arrange(dob) |>
  rename(reg_id_alt = temp) ->
  cdn

cdn |>
  relocate(reg_id_alt, .after = last_col()) |>
  relocate(reg_id, .before = reg_name) |>
  mutate(dob = ymd(dob)) ->
  cdn

cdn |>
  filter(inbreeding < 0) |>
  mutate(breed = substr(reg_id, 1, 2),
         country = substr(reg_id, 3, 5),
         id_number = substr(reg_id, 6, 17),
         id_number = str_replace(id_number, "^0+" ,""),
         url = paste0("https://www.cdn.ca/query/detail_ge.php?breed=", breed, 
                      "&country=", country, "&sex=M&regnum=", id_number)) ->
  bulls

bull_data <- list()
for(i in 3353:dim(bulls)[1]){
  print(i)
  Sys.sleep(2)
  url <- bulls$url[i]
  tryCatch(url |>
             read_html(options = "RECOVER") |>
             html_element(css = "#ContentArea > div.AnimalDetails > table:nth-child(1)") |>
             html_table(),
           error = function(e){NA}) ->
    bull_data[[i]]
}

names(bull_data) <- bulls$reg_id[3353:5850]

# 3353:5850

bull_data |> 
  map(~ .x %>% 
        pluck("X11") %>% 
        as_tibble() %>%
        slice(1)) ->
  inb_list

bind_rows(inb_list, .id = "id") |>
  mutate(inbreeding = as.numeric(str_remove(value, "%INB"))) |>
  select(-value) ->
  inb_alt

inb_alt |> 
  filter(!is.na(inbreeding)) ->
  inb_alt

bulls |>
  rownames_to_column(var = "id") |>
  mutate(id = as.character(id)) ->
  bulls

inb_alt <- left_join(inb_alt, bulls[, c("id", "reg_id")], by = "id")
inb_alt |>
  select(-id) |>
  relocate(inbreeding, .after = reg_id) ->
  inb_alt

write_csv(inb_alt, file = "../data/inb_alt_2.csv")

inb_alt_1 <- read_csv("../data/inb_alt_1.csv")
inb_alt <- rbind(inb_alt_1, inb_alt)

cdn <- left_join(cdn, inb_alt, by = "reg_id")

cdn |>
  mutate(inbreeding = ifelse(inbreeding.x == -9.99, inbreeding.y, inbreeding.x)) |>
  select(-c(inbreeding.x, inbreeding.y)) ->
  cdn

cdn <- read_csv("../data/cdn_data.csv")
bq_auth(email = "victorf2@illinois.edu")

projectid <- "dairy-168114"
dataset_name <- "cdn_data"

cdn_table <- bq_table(project = projectid, dataset = dataset_name, 
                      table = "CDN_info")

bq_table_create("dairy-168114.CDN_data.CDN_bulls", fields = as_bq_fields(cdn))

bq_table_upload("dairy-168114.CDN_data.CDN_bulls", values = cdn)


write_csv(cdn, file = "../data/cdn_data.csv")
