library(tidyverse)
library(statar)
library(tradestatistics)
library(haven)

exp <- ots_create_tidy_data(
  years = 2019,
  reporters = "all",
  commodities = "051110",
  table = "yrc")

exp |> 
  as_tibble() |>
  filter(trade_value_usd_exp > 0) |>
  arrange(desc(trade_value_usd_exp)) |>
  select(-c(commodity_code, section_name, section_code, year, 
            trade_value_usd_imp, reporter_iso, commodity_name)) |>
  mutate(perc = trade_value_usd_exp/sum(exp$trade_value_usd_exp)*100,
         cum = cumsum(perc)) ->
  table

table |>
  slice(1:10) ->
  table_1

table |>
  slice(11:49) |>
  summarise(trade_value_usd_exp = sum(trade_value_usd_exp),
            perc = sum(perc)) |>
  mutate(reporter_name = "Rest",
         cum = 100) |>
  relocate(reporter_name, .before = trade_value_usd_exp) ->
  table_11

table_1 <- rbind(table_1, table_11)

table_1 |>
  mutate(perc = round(perc, digits = 2)) |>
  ggplot(aes(x = reorder(reporter_name, perc), y = perc)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = sprintf("%1.1f%%", perc)), color = "black", size = 3.5)+
  theme_minimal() +
  coord_flip() +
  xlab("Country") + ylab("Share of total trade value") + 
  ggtitle("Bovine Semen Exports by country (2019)")

exp |> 
  as_tibble() |>
  filter(trade_value_usd_imp > 0) |>
  arrange(desc(trade_value_usd_imp)) |>
  select(-c(commodity_code, section_name, section_code, year, 
            trade_value_usd_exp, reporter_iso, commodity_name)) |>
  mutate(perc = trade_value_usd_imp/sum(exp$trade_value_usd_imp)*100,
         cum = cumsum(perc)) ->
  table_imp

table_imp |>
  slice(1:20) |>
  mutate(perc = round(perc, digits = 2)) |>
  ggplot(aes(x = reorder(reporter_name, perc), y = perc)) +
  geom_bar(stat = "identity", fill = "#f68060") +
  geom_text(aes(label = perc), color = "black", size = 3.5)+
  theme_minimal() +
  coord_flip() +
  xlab("Country") + ylab("Share of total trade value") + 
  ggtitle("Top 20 Bovine Semen Imports by country (2019)")

dairy <- read_csv("C:/Users/victo/Box/dairy_matching/data/dairy_market.csv")

dairy |>
  pivot_longer(-year, names_to = "variables", values_to = "values") |>
  filter(variables %in% c("production", "total_utilization")) |>
  mutate(variables = replace(variables, variables == "production", "Production"),
         variables = replace(variables, variables == "total_utilization", "Consumption")) |>
  ggplot(aes(x = year, y = values, group = variables)) + 
  geom_point(aes(color = variables)) + 
  geom_line(aes(color = variables)) + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Year") + ylab("Millions of pounds") + 
  scale_x_continuous(breaks = seq(1970, 2020, 5))

dairy |>
  select(c(year, milk_cow)) |>
  ggplot(aes(x = year, y = milk_cow)) + 
  geom_point(color = "darkred") + 
  geom_line(color = "darkred") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Year") + ylab("Pounds of milk per cow") + 
  scale_x_continuous(breaks = seq(1970, 2020, 5))
  
dairy |>
  pivot_longer(-year, names_to = "variables", values_to = "values") |>
  filter(variables %in% c("imports", "exports")) |>
  mutate(variables = replace(variables, variables == "imports", "Imports"),
         variables = replace(variables, variables == "exports", "Exports")) |>
  ggplot(aes(x = year, y = values, group = variables)) + 
  geom_point(aes(color = variables)) + 
  geom_line(aes(color = variables)) + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Year") + ylab("Millions of pounds") + 
  scale_x_continuous(breaks = seq(1970, 2020, 5))