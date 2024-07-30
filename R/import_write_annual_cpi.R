library(tidyverse)


cpi_yearly <- read_csv(
  "data/series-170124.csv",
  skip = 8,
  col_names = c("year", "CPI"),
  col_types = c("int", "dbl")
) |>
  filter(!is.na(year))

write_csv(cpi_yearly, "data/cpi.csv")
