library(tidyverse)
library(haven)
library(rvest)

inb_missing <- read_csv("../data/inb_missing.csv")

inb_missing|>
  mutate(breed = substr(reg_id, 1, 2),
         country = substr(reg_id, 3, 5),
         id_number = substr(reg_id, 6, 17),
         id_number = str_replace(id_number, "^0+" ,""),
         url = paste0("https://www.cdn.ca/query/detail_ge.php?breed=", breed, 
                      "&country=", country, "&sex=M&regnum=", id_number)) ->
  inb_missing

bull_data <- list()
for(i in 1:dim(inb_missing)[1]){
  print(i)
  Sys.sleep(1)
  url <- inb_missing$url[i]
  tryCatch(url |>
             read_html(options = "RECOVER") |>
             html_element(css = "#ContentArea > div.AnimalDetails > table:nth-child(1)") |>
             html_table(),
           error = function(e){NA}) ->
    bull_data[[i]]
}

names(bull_data) <- inb_missing$reg_id

bull_data |> 
  map(~ .x %>% 
        pluck("X11") %>% 
        as.tibble() %>%
        slice(1)) ->
  inb_list

bind_rows(inb_list, .id = "reg_id") |>
  mutate(inbreeding = as.numeric(str_remove(value, "%INB"))) |>
  select(-value) ->
  inb_alt

write_csv(inb_alt, file = "../data/inb_alt.csv")

naab <- left_join(naab, cdn[, c("reg_id_alt", "inbreeding")], 
                  by = join_by(reg_id == reg_id_alt))

naab |>
  filter(is.na(inbreeding)) |>
  mutate(breed = substr(reg_id, 1, 2),
         country = substr(reg_id, 3, 5),
         id_number = substr(reg_id, 6, 17),
         id_number = str_replace(id_number, "^0+" ,""),
         url = paste0("https://www.cdn.ca/query/detail_ge.php?breed=", breed, 
                      "&country=", country, "&sex=M&regnum=", id_number)) ->
  bulls

bull_data <- list()
for(i in 1:dim(bulls)[1]){
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
bull_data <- bull_data
names(bull_data) <- bulls$reg_id


bull_data |> 
  map(~ .x %>% 
        pluck("X11") %>% 
        as.tibble() %>%
        slice(1)) ->
  inb_list

bind_rows(inb_list, .id = "reg_id") |>
  mutate(inbreeding = as.numeric(str_remove(value, "%INB"))) |>
  select(-value) ->
  inb_alt

inb_alt |>
  filter(!is.na(inbreeding)) ->
  inb_alt

write_csv(inb_alt, file = "../data/inb_alt.csv")