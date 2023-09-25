library(tidyverse)
library(SPHSUgraphs)

library(statxplorer)
load_api_key("api_key.txt")

se_query <- fetch_table(read_file("data/uc_pp_la.json"))

uc_pp_la <- se_query$dfs$`People on Universal Credit`

populations <- read_csv("data/uk_la_pops.csv")

load("data/uc_pp_la_empl.rdata")

la_codes <- read_csv("data/la_codes.csv") |> 
  filter(!str_detect(LAD18CD, "^N")) |> 
  select(LAD18CD, LAD18NM)

library(readxl)
complete_la_rollout <- read_excel("data/complete_la_rollout.xlsx")

sumpops <- populations |> 
  select(-...26) |> 
  filter(age >= 16 & age < 65) |> 
  pivot_longer(starts_with("population"),
               names_to = "year",
               values_to = "pop",
               names_prefix = "population_") |> 
  group_by(ladcode20, laname20, year) |> 
  summarise(pop = sum(pop))

uc_totals <- uc_pp_la |> 
  mutate(la = str_extract(`National - Regional - LA - OAs`, "^[^/]*") |> str_trim(), .keep = "unused") |> 
  filter(la != "Total") |> 
  left_join(la_codes, by = join_by(
    la == LAD18NM
  )) |> 
  mutate(year = str_extract(Month, "\\d{4}$")) |> 
  rename(uc_tot = `People on Universal Credit`)


uc_totals |> 
  filter(year < 2021) |> 
  left_join(sumpops, by = join_by(
    year == year,
    la == laname20
    # LAD18CD == ladcode20
  )) |> 
  # filter(is.na(pop)) |>
  # ungroup() |>
  # select(la, LAD18CD, year) |>
  # unique()
  group_by(Month) |> 
  summarise(uc_tot = sum(uc_tot),
            pop = sum(pop, na.rm = TRUE)) |> 
  mutate(p_uc = uc_tot / pop,
         # Month = fct_reorder(factor(Month), my(Month))
         Month = my(Month)
         ) |> 
  arrange(Month) |> 
  ggplot(aes(Month, p_uc)) +
  geom_col(fill = sphsu_cols("University Blue")) +
  scale_y_continuous("Population receiving Universal Credit (%)", labels = scales::percent_format(1), expand = c(0, NA)) +
  theme_sphsu_light() 
  
dates <- uc_totals |> 
  ungroup() |> 
  select(Month) |> 
  unique() |> 
  mutate(month = my(Month))

p_uc_by_month <- uc_totals |> 
  filter(year < 2021) |> 
  left_join(sumpops, by = join_by(
    year == year,
    la == laname20
    # LAD18CD == ladcode20
  )) |> 
  mutate(p_uc_obs = uc_tot/ pop) |> 
  left_join(dates, by = "Month") |> 
  ungroup() |> 
  select(LAD18CD, ladcode20, la, month, pop, uc_tot, p_uc_obs)

write_csv(p_uc_by_month, file = "data/perc_uc_by_la_by_month.csv")


p_uc_with_dates <- p_uc_by_month |> 
  left_join(
    complete_la_rollout,
    by = join_by(
      LAD18CD == LAUA
    )
  ) |> 
  mutate(natural_migration = as.integer(`UC rollout`<= month))

p_uc_with_dates |> 
  group_by(natural_migration) |> 
  summarise(highest = max(p_uc_obs),
            lowest = min(p_uc_obs))

p_uc_with_dates |> 
  filter(natural_migration == 1) |> 
  group_by(la) |> 
  summarise(max_post = max(p_uc_obs)) |> 
  count(max_post > 0.1)

p_uc_with_dates |> 
  filter(natural_migration == 1) |> 
  count(p_uc_obs > 0.1)

library(gganimate)
library(plotly)

`-.gg` <- function(e1, e2) e2(e1)

p <- p_uc_by_month |> 
  ggplot(aes(month, p_uc_obs)) +
  geom_col(fill = sphsu_cols("University Blue")) +
  scale_y_continuous("Population receiving Universal Credit (%)", labels = scales::percent_format(1), expand = c(0, NA)) +
  theme_sphsu_light() +
  transition_states(la) +
  labs(title = "{closest_state}")

animate(p, nframes = 1000) + ease_aes('cubic-in-out')


p_uc_by_month |> 
  ggplot(aes(month, p_uc_obs, frame = la)) +
  # geom_col(fill = sphsu_cols("University Blue")) +
  geom_col(position = "identity", fill = sphsu_cols("University Blue")) +
  scale_y_continuous("Population receiving Universal Credit (%)", labels = scales::percent_format(1), expand = c(0, NA)) +
  theme_sphsu_light() - ggplotly
