library(statxplorer)
library(tidyverse)

load_api_key("api_key.txt")

uc_pp_la_cond <- fetch_table(filename = file.path("data", "uc_pp_la_cond.json"))

uc_pp_la_cond_n <- uc_pp_la_cond$dfs$`People on Universal Credit`

uc_pp_la_cond_n |> 
  select(cond = `Conditionality Regime`,
         la = `National - Regional - LA - OAs`,
         Month,
         uc = `People on Universal Credit`) |> 
  mutate(month = my(Month))

save(uc_pp_la_cond_n, file = "data/uc_pp_la_cond.rdata")
