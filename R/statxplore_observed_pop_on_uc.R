library(statxplorer)
library(plotly)
library(tidyverse)
library(lubridate)
library(SPHSUgraphs)
`-.gg` <- function(e1, e2) e2(e1)

theme_set(theme_sphsu_light())


# loading data from StatXplore api ----------------------------------------



load_api_key("api_key.txt")

n_uc <- fetch_table(filename = "data/uc_pp_n.json")

n_uc_pp <- n_uc$dfs$`People on Universal Credit`

n_uc_pp <- n_uc_pp |> 
  mutate(date = my(Month))

wa_pop <- read_csv("data/lfs_pop.csv", col_names = c("year_month", "pop"), 
                             skip = 8)

pop_uc <- wa_pop |>
  mutate(pop = pop * 1000) |> 
  separate(year_month, c("year", "month"), sep = " ", convert = TRUE) |> 
  filter(str_detect(month, "\\w{3}"),
         year > 2012) |> 
  mutate(date = ym(paste(year, month))) |> 
  inner_join(n_uc_pp, by = "date") |> 
  select(Month, date, n_uc = `People on Universal Credit`, pop)


pop_uc |> 
  mutate(prop_uc = n_uc / pop) |> 
  ggplot(aes(date, prop_uc)) +
  geom_col()

write_rds(pop_uc, "data/pop_on_uc.rds")
