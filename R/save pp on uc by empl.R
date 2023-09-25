library(statxplorer)
library(tidyverse)

load_api_key("api_key.txt")

uc_pp_la_empl <- fetch_table(filename = file.path("data", "uc_pp_la_empl.json"))

uc_pp_la_empl_n <- uc_pp_la_empl$dfs$`People on Universal Credit`

uc_pp_la_empl_n |> 
  select(empl = `Employment indicator`, 
         la = `National - Regional - LA - OAs`,
         Month,
         uc = `People on Universal Credit`) |> 
  mutate(month = my(Month))

save(uc_pp_la_empl_n, file = "data/uc_pp_la_empl.rdata")
