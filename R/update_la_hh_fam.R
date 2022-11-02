library(statxplorer)
library(tidyverse)

# loading data from StatXplore api ----------------------------------------



load_api_key("api_key.txt")

uc_hh_la_fam <- fetch_table(read_file(file.path("data", "uc_hh_la_family.json")))

uc_hh_la_fam <- uc_hh_la_fam$dfs$`Households on Universal Credit`

save(uc_hh_la_fam, file = "data/uc_hh_la_fam.rdata")
