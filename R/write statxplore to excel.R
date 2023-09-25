load(file = "data/uc_pp_la_cond.rdata")
load(file = "data/uc_pp_la_empl.rdata")
load(file = "data/uc_hh_la_fam.rdata")

library(openxlsx)


wb <- createWorkbook()

addWorksheet(wb, "UC claimant by employment")
addWorksheet(wb, "UC claimant by conditionality")
addWorksheet(wb, "UC houshold by family type")

uc_pp_la_empl_n |>
  select(LA = `National - Regional - LA - OAs`,
         Month,
         `Employment indicator`,
         `People on Universal Credit`) |>
  filter(`Employment indicator` != "Total") |> 
  writeData(wb = wb, sheet = "UC claimant by employment")

uc_pp_la_cond_n |>
  select(LA = `National - Regional - LA - OAs`,
         Month,
         `Conditionality Regime`,
         `People on Universal Credit`) |>
  filter(`Conditionality Regime` != "Total") |> 
  writeData(wb, "UC claimant by conditionality", x = _)

uc_hh_la_fam |> 
  select(LA = `National - Regional - LA - OAs`,
         Month,
         `Family Type`,
         `Households on Universal Credit`) |> 
  filter(`Family Type` != "Total") |> 
writeData(wb, "UC houshold by family type", x = _)

saveWorkbook(wb, "uc_claimant_data.xlsx", overwrite = TRUE)
