library(tidyverse)
library(readxl)
library(statar)
library(haven)

firms <- read_excel("./data/company_info.xlsx")

firms |>
  mutate(date_1 = mark::str_extract_date(assigned, format = "%m/%d/%Y"),
         assigned_new = assigned) |>
  separate(assigned, into = c("temp", "assigned"), sep = " ") |>
  select(-temp) |>
  mutate(date_1 = replace(date_1, is.na(date_1), mdy(assigned))) |>
  mutate(assigned = paste0("0", assigned)) |>
  mutate(date_1 = replace(date_1, is.na(date_1), mdy(assigned))) |>
  select(-assigned) |>
  rename(assigned = assigned_new,
         assigned_date = date_1) ->
  firms

firms |>
  select(-ending_date) |>
  separate(`end Date`, into = c("temp", "date_1"), 
           sep = -10, remove = FALSE) |>
  select(-temp) |> 
  mutate(date_1 = mdy(date_1)) |>
  rename(ending_date = date_1) ->
  firms


write_csv(firms, "./data/company_info_new.csv")