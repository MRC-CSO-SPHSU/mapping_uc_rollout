library(tidyverse)
library(readxl)
library(gt)

complete_la_rollout <- read_excel("data/complete_la_rollout.xlsx")

end_q <- 4 * (2019 - 2012) + 4

by_qs <- complete_la_rollout |> 
  filter(!str_detect(LAUA, "^N")) |> 
  arrange(`UC rollout`) |> 
  mutate(
    year = year(`UC rollout`),
    rollout_q = quarter(`UC rollout`) + 4 * (year - 2012),
    start_q = quarter(`UC rollout`, type = "date_first") |> month(label = TRUE),
    last_q = quarter(`UC rollout`, type = "date_last") |> month(label = TRUE),
    quarter_dur = glue::glue("{start_q}-{last_q}, {year}"),
    quarters_exposed = end_q - rollout_q
    )

by_qs |> 
  count(rollout_q, quarters_exposed) |> 
  group_by(quarters_exposed > 8) |> 
  summarise(tot = sum(n))

by_qs |> 
  count(rollout_q, quarters_exposed) |> 
  group_by(quarters_exposed >= 8) |> 
  summarise(tot = sum(n))

by_qs |> 
  count(rollout_q, quarters_exposed) |> 
  group_by(quarters_exposed >= 4) |> 
  summarise(tot = sum(n))


by_qs |>
  group_by(quarter_dur, quarters_exposed) |>
  summarise(
    `Local Authorities` = paste(`Local authority`, collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(-quarters_exposed) |>
  gt() |>
  cols_label(quarter_dur = "Quarter of UC natural migration", quarters_exposed = "Number of quarters exposed (by end of 2019)") |> 
  gtsave("quarters_LA_observed.docx")

by_qs |> 
  mutate(quarter_dur = fct_reorder(factor(quarter_dur), rollout_q) |> fct_relabel(~str_replace(.x, ", ", "\n"))) |> 
  ggplot(aes(quarter_dur)) +
  geom_bar(fill = sphsu_cols("University Blue", names = FALSE)) +
  scale_y_continuous("Number of LAs introducing UC in quarter", expand = expansion(mult = c(0, 0.05))) +
  theme_sphsu_light() +
  theme(panel.grid.major.x = element_blank()) +
  xlab("Quarter")

ggsave("graphs/rollout_by_quarter.png", dpi = 300, width = 200, height = 110, units = "mm")


read_table("
  Outcome	UC Covid
Life_Satisfaction	-0.66 -0.26
Happiness	-0.41 -0.15
Life_Worthwhile	-0.73 -0.13
Anxiety	+0.79	+0.26") |> 
  mutate(mult = UC/Covid)
