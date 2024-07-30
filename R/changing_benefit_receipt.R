library(statxplorer)
library(plotly)
library(tidyverse)
library(lubridate)
library(SPHSUgraphs)
library(scales)
`-.gg` <- function(e1, e2) e2(e1)

theme_set(theme_sphsu_light())


# loading data from StatXplore api ----------------------------------------



load_api_key("api_key.txt")

n_uc <- fetch_table(filename = "data/uc_pp_n.json")
n_jsa <- fetch_table(filename = "data/jsa_pp_n.json")
n_hb <- fetch_table(filename = "data/hb_hh_n.json")
n_hb2 <- fetch_table(filename = "data/hb_hh_n2.json")

library(readxl)
ctc_wtc_awards <- read_excel("data/ctc_wtc_awards.xlsx", 
                             sheet = "Table_1_2", skip = 2)

ctc_wtc_awards_23 <- read_excel("data/ctc_wtc_awards_23.xlsx", 
                             sheet = "Table_1_1", skip = 2)

n_uc_pp <- n_uc$dfs$`People on Universal Credit`
n_jsa_pp <- n_jsa$dfs$`Jobseekers Allowance`
n_hb_hh <- n_hb$dfs$`Housing Benefit Claimants` |> bind_rows(n_hb2$dfs$`Housing Benefit Claimants`)

n_tc_fu <- ctc_wtc_awards_23 |> 
  mutate(date = floor_date(dmy(Dates), "months")) |> 
  select(date, tax_credits = `Total in receipt (out-of-work and in-work families)`) |> 
  mutate(tax_credits = 1000 * tax_credits) |> 
  filter(!is.na(date))

# n_tc_fu <-
#   ctc_wtc_awards |> 
#   pivot_longer(-`Recipient families and entitlement by family type and profile position`,
#                names_to = "year",
#                values_to = "tax_credits") |> 
#     filter(`Recipient families and entitlement by family type and profile position` == "Total families") |> 
#     select(year, tax_credits) |> 
#   mutate(date = ymd(paste0(str_extract(year, "^\\d{4}"), "0501")),
#          tax_credits = if_else(
#            str_detect(tax_credits, "m"), 
#            str_extract(tax_credits, "\\d*") |> as.integer(),
#            as.integer(tax_credits)/1000) * 1000000)
  
n_uc_pp <- n_uc_pp |> 
  mutate(date = my(Month))

n_jsa_pp <- n_jsa_pp |> 
  mutate(date = my(Quarter))

n_hb_hh <- n_hb_hh |> 
  mutate(date = my(str_extract(Month, "\\w{3}-\\d{2}")))

n_uc_pp |> 
  filter(date > ymd("20140101")) |> 
  full_join(
    n_jsa_pp |> 
      filter(date > ymd("20140101")),
    by = join_by(date)
  ) |> 
  full_join(
    n_hb_hh,
    by = join_by(date)
  ) |>
  full_join(
    n_tc_fu,
    by = join_by(date)
  ) |> 
  filter(date >= ymd("20140101")) |> 
  select(date, 
         `Universal Credit Claimants` = `People on Universal Credit`, 
         `Jobseekers Allowance`, 
         `Housing Benefit Claimants`,
         `Working/Child Tax Credit Claimants` = tax_credits) |> 
  pivot_longer(-date,
               names_to = "Benefit", values_to = "Number of claimants") |> 
  mutate(Benefit = fct_inorder(Benefit)) |> 
  filter(!is.na(`Number of claimants`)) |> 
  ggplot(aes(date, `Number of claimants`, fill = Benefit, colour = Benefit)) +
  # geom_col(position = "dodge") +
  # geom_smooth(size = 2) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(
    values = sphsu_cols(c("pumpkin", "turquoise", "moss", "thistle"),
                        names = FALSE),
    aesthetics = c("fill", "colour")
    )

ggsave("graphs/benefits_rates.tiff", compression = "lzw", dpi = 900)
