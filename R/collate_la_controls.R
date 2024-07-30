library(readxl)
library(tidyverse)

complete_la_rollout <- read_excel("data/complete_la_rollout.xlsx")

lad_nuts_lookup <- read_csv("data/lad_nuts_lookup.csv")

new_old_lad_codes <- read_csv("data/perc_uc_by_la_by_month.csv", col_select = c("LAD18CD", "ladcode20", "la")) |> 
  unique()


lad_nuts_lookup <- lad_nuts_lookup |> 
  select(LAUA = LAD18CD,
         NUTS3 = NUTS318CD,
         NUTS2 = NUTS218CD,
         NUTS1 = NUTS118CD,
         laua_name = LAD18NM,
         nuts3_name = NUTS318NM,
         nuts2_name = NUTS218NM,
         nuts1_name = NUTS118NM) |> 
  unique()


missing_codes <- tibble(LAUA = c(
    "E06000048",
    "E06000058",
    "E06000059",
    "E06000060",
    "E07000097",
    "E07000101",
    "E07000244",
    "E07000245",
    "E07000246",
    "E08000020",
    "S12000015",
    "S12000024",
    "S12000049",
    "S12000050"
  )
)

# Only two of these are relevant
matchup_codes <- tribble(
  ~LAUA, ~match,
    # "E06000048", "E06000048",
    # "E06000058", "E06000028",
    # "E07000244", "E07000205",
    # "E07000097", "E07000242",								
    # "E07000101", "E07000243",								
    # "E08000020", "E08000037",
    "S12000015", "S12000047",
    "S12000024", "S12000048",
    # "S12000049", "S12000046",
    # "S12000050", "S12000044"
)

anti_join(missing_codes, complete_la_rollout)

anti_join(missing_codes, lad_nuts_lookup, by = join_by(LAUA))

anti_join(complete_la_rollout, lad_nuts_lookup) |> 
  arrange(`Local authority`)

all_la_codes <- complete_la_rollout |> 
  select(LAUA, `Local authority`) |> 
  left_join(lad_nuts_lookup, by = join_by(LAUA)) |> 
  filter(NUTS1 != "UKN")

all_la_codes <- all_la_codes |>
  filter(NUTS2 == "UKK2") |>
  mutate(NUTS3 = case_when(NUTS3 == "UKK21" ~ "UKK24",
                           NUTS3 == "UKK22" ~ "UKK25",
                           .default = NA_character_)) |>
  filter(!is.na(NUTS3)) |>
  bind_rows(all_la_codes, x = _)

# Import GVA ------------------------------------------------------------


gva_nuts <- read_excel("data/gva_nuts3_98_20.xlsx", sheet = "Table 2", skip = 1)

gva <- gva_nuts |> 
  pivot_longer(matches("\\d{4}"), names_to = "year", values_to = "GVA", names_transform = as.integer) |> 
  filter(ITL == "ITL3") |> 
  mutate(NUTS3 = paste0("UK", str_extract(`ITL code`, "(?<=TL).*"))) |> 
  select(NUTS3, region_name = `Region name`, year, GVA) |> 
  filter(!str_detect(NUTS3, "UKN"))


all_la_codes |> 
  anti_join(gva, join_by(NUTS3))

gva |> 
  anti_join(all_la_codes, join_by(NUTS3))

gva_codes <- all_la_codes |> 
  left_join(gva, join_by(NUTS3), relationship = "many-to-many")

## all GVA's working


# import spending -------------------------------------------------

## England - Education and culture spending - PLDR --------------------------

eng_culture <- read_csv("data/FIN_07_26L.csv")
eng_education <- read_csv("data/FIN_07_19L.csv")
eng_social <- read_csv("https://pldr.org/download/2o7dv/erl/FIN_07_21L.csv")

eng_spend <- eng_culture |> 
  left_join(eng_education, by = join_by(LTLA18CD, LTLA18NM, Year)) |> 
  left_join(eng_social, by = join_by(LTLA18CD, LTLA18NM, Year)) |> 
  select(LAUA = LTLA18CD,
         laua_name = LTLA18NM,
         year = Year,
         culture_pc = Cultural_Services_GrossExpen_PerCap,
         educ_pc = Education_Services_GrossExpen_PerCap,
         social_pc = Social_Care_Services_GrossExpen_PerCap)

all_la_codes |> 
  filter(NUTS1 != "UKM", NUTS1 != "UKL") |> 
  anti_join(eng_spend, join_by(LAUA))



## Wales - Education and culture spending - Wales ------------------------------

wal_raw <- list()

wal_raw[[1]] <- jsonlite::fromJSON(curl::curl("http://open.statswales.gov.wales/en-gb/dataset/lgfs0023"))

data_len <- length(wal_raw[[1]])
index <- 2

while (data_len == 3) {
  wal_raw[[index]] <- jsonlite::fromJSON(curl::curl(wal_raw[[index - 1]]$odata.nextLink))
  data_len <- length(wal_raw[[index]])
  index <- index + 1
}

wal_spend <- wal_raw |>
  map( ~ as_tibble(.x$value)) |>
  map(\(spend_fragment) {
    spend_fragment |>
      filter(Measure_ItemName_ENG == "£ per head") |>
      select(
        Service_ItemName_ENG,
        LAUA = Authority_AltCode1,
        year = Year_Code,
        laua_name = Authority_ItemName_ENG,
        Data
      ) |>
      mutate(Data = Data / 1000)
  }) |>
  reduce(bind_rows) |>
  pivot_wider(names_from = Service_ItemName_ENG, values_from = Data) |>
  mutate(year = as.integer(str_extract(year, "^20\\d{2}"))) |>
  select(year,
         LAUA,
         laua_name,
         culture_pc = `Libraries, culture, heritage, sport and recreation`,
         educ_pc = Education,
         social_pc = `Social services`) |>
  inner_join(lad_nuts_lookup |> select(LAUA, laua_name),
             by = join_by(LAUA, laua_name))


## Scotland - Scot Gov ---------------------------------------------------------

all_scot <- read_csv("data/combined_scot_services.csv") |> 
  filter(Year > 2010)

scot_education <- read_excel("data/Scot_educ_culture.xlsx", 
                             sheet = "Table 1 - Education",
                             skip = 5) |> 
  rename(laua_name = `Local Authority`) |>
  pivot_longer(
    -laua_name,
    names_to = "year",
    values_to = "educ_spend",
    names_transform = ~ as.integer(str_extract(.x, "^20\\d{2}"))
  ) |> 
  filter(laua_name != "Councils") |> 
  mutate(laua_name = str_replace(laua_name, "&", "and"))

scot_culture <- read_excel("data/Scot_educ_culture.xlsx", 
                             sheet = "Table 2 - Culture",
                             skip = 5) |> 
  rename(laua_name = `Local Authority`) |>
  pivot_longer(
    -laua_name,
    names_to = "year",
    values_to = "culture_spend",
    names_transform = ~ as.integer(str_extract(.x, "^20\\d{2}"))
  ) |> 
  filter(laua_name != "Councils") |> 
  mutate(laua_name = str_replace(laua_name, "&", "and"))

scot_social <- read_excel("data/Scot_social.xlsx", 
                             sheet = "Table 3 - Social Work",
                             skip = 5) |> 
  rename(laua_name = `Local Authority`) |>
  pivot_longer(
    -laua_name,
    names_to = "year",
    values_to = "social_spend",
    names_transform = ~ as.integer(str_extract(.x, "^20\\d{2}"))
  ) |> 
  filter(laua_name != "Councils") |> 
  mutate(laua_name = str_replace(laua_name, "&", "and"))

decode_lauas <- lad_nuts_lookup |> 
  select(laua_name, LAUA) |> 
  unique()

populations <- read_csv("data/uk_la_pops.csv")

sumpops <- populations |> 
  select(-...26) |> 
  filter(age >= 16 & age < 65) |> 
  pivot_longer(starts_with("population"),
               names_to = "year",
               values_to = "pop",
               names_prefix = "population_") |> 
  group_by(ladcode20, laname20, year) |> 
  summarise(pop = sum(pop)) |> 
  mutate(year = as.integer(year))

scot_spend <- all_scot |> 
  select(laua_name = LA_NAME,
         year = Year,
         social_spend = Social_Work_Services) |> 
  filter(laua_name != "Scotland", laua_name != "Councils") |> 
  mutate(laua_name = str_replace(laua_name, "&", "and") |> 
           str_replace("Edinburgh, City of", "City of Edinburgh"),
         laua_name = if_else(laua_name == "Eilean Siar", "Na h-Eileanan Siar", laua_name)) |> 
  left_join(scot_education, by = join_by(laua_name, year)) |> 
  left_join(scot_culture, by = join_by(laua_name, year)) |> 
  left_join(decode_lauas, by = join_by(laua_name)) |> 
  inner_join(sumpops, by = join_by(laua_name == laname20, year)) |> 
  mutate(
    educ_pc = educ_spend / pop,
    social_pc = social_spend / pop,
    culture_pc = culture_spend / pop) |> 
  select(LAUA, laua_name, year, culture_pc, educ_pc, social_pc)


la_spending <- bind_rows(
  eng_spend,
  wal_spend,
  scot_spend
) |> 
  filter(year > 2010, year < 2019)


# checking by LA ----------------------------------------------------------


library(plotly)

`-.gg` <- function(e1, e2) e2(e1)

bind_rows(
  eng_spend,
  wal_spend,
  scot_spend
) |> 
  filter(year > 2009, year < 2019) |> 
  mutate(country = str_extract(LAUA, "^\\w")) |> 
  pivot_longer(ends_with("_pc"), names_to = "spend", values_to = "per_capita") |> 
  ggplot(aes(year, per_capita, colour = country, text = laua_name, group = country)) +
  geom_point() +
  # geom_smooth(method = "lm") +
  xlim(2010, 2018) +
  facet_wrap(~spend, scales = "free_y") +
  scale_colour_manual(values = c(
    S = "blue",
    W = "green",
    E = "red"
  )) - ggplotly



# collating gva and spending ----------------------------------------------

# checking all available
gva_codes |>
  select(LAUA, laua_name, NUTS3, nuts3_name, year, GVA) |> 
  filter(year > 2010, year < 2019) |> 
  full_join(la_spending |> select(-laua_name), by = join_by(LAUA, year)) |> 
  filter(is.na(GVA) | is.na(educ_pc)) |>
  summarise(across(c(GVA, culture_pc, educ_pc), sum), .by = c(LAUA, laua_name)) |> View()

# copeland and scilly missing a year each in LA spend?

all_data <- gva_codes |>
  select(LAUA, laua_name, NUTS3, nuts3_name, year, GVA) |> 
  filter(year > 2010, year < 2019) |> 
  full_join(la_spending |> select(-laua_name), by = join_by(LAUA, year))

all_data <- all_data |> 
  filter(LAUA %in% matchup_codes$match) |> 
  rowwise() |> 
  mutate(LAUA = matchup_codes[matchup_codes$match == LAUA, "LAUA", drop = TRUE]) |> 
  bind_rows(all_data, x = _)

write_csv(all_data, "data/gva_la_spending_by_laua.csv")
