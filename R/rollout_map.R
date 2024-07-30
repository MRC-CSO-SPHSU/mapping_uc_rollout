library(tidyverse)
library(sf)
library(readxl)
library(SPHSUgraphs)
library(plotly)

# uk_las <- st_read("data/la_shapefiles/LAD_APR_2019_UK_BFC.shp")
uk_las <- st_read("data/la_shapefiles/Local_Authority_Districts_December_2011_GB_BFE.shp")
# uk_las <- st_read("data/la_shapefiles/LAD_MAY_2021_UK_BGC.shp")

uk_las_simple <- st_simplify(uk_las, dTolerance = 500)

complete_la_rollout <- read_excel("data/complete_la_rollout.xlsx")

matchup_codes <- tribble(
  ~LAUA, ~match,
  "E06000048", "E06000057",
  "E06000058", "E06000028",
  "E07000244", "E07000205",
  "E07000097", "E07000242",
  "E07000100", "E07000240",
  "E07000101", "E07000243",
  "E07000104", "E07000241",
  "E08000020", "E08000037",
  "S12000015", "S12000047",
  "S12000024", "S12000048",
  "S12000049", "S12000046",
  "S12000050", "S12000044"
)

all_la_codes <- complete_la_rollout |> 
  rowwise() |>
  filter(LAUA %in% matchup_codes$match) |> 
  mutate(
    LAUA = matchup_codes[matchup_codes$match == LAUA, "LAUA", drop = TRUE]
  ) |> 
  bind_rows(complete_la_rollout)


uk_las_simple |> 
  anti_join(all_la_codes,
            by = join_by(lad11cd ==  LAUA))

uk_las_simple |> 
  left_join(
    all_la_codes |> 
      mutate(year_rollout = factor(year(`UC rollout`), levels = 2015:2018)),
    by = join_by(lad11cd ==  LAUA)
  ) |> 
  ggplot() +
  geom_sf(aes(fill = year_rollout), colour = "white") +
  geom_sf(fill = NA, colour = "darkgrey") +
  coord_sf() +
  scale_fill_manual(name = "Year of UC\nnatural migration", values = sphsu_cols(c("University Blue", "Thistle", "Leaf", "Pumpkin"), names = FALSE)) +
  theme_void()

ggsave("graphs/map_UK_rollout_dates.tiff", height = 3500, width = 2500, dpi = 600, units = "px", compression = "lzw")
ggsave("graphs/map_UK_rollout_dates.png", height = 3500, width = 2500, dpi = 600, units = "px")

`-.gg` <- \(e, f) f(e)

uk_las_mpg <- uk_las_simple
uk_las_mpg$geometry <- st_cast(uk_las_simple$geometry, "MULTIPOLYGON")

{uk_las_mpg |> 
  left_join(
    all_la_codes |> 
      mutate(year_rollout = factor(year(`UC rollout`), levels = 2015:2018)),
    by = join_by(lad11cd ==  LAUA)
  ) |> 
  mutate(text = paste0(lad11nm, "\nUC rollout: ", format.Date(`UC rollout`, format = "%B %Y"))) |> 
  ggplot() +
  geom_sf(aes(fill = year_rollout, text = text), linewidth = 0.2) +
  coord_sf() +
  # scale_fill_discrete(name = "Year of UC\nnatural migration", type = RColorBrewer::brewer.pal(4, "Set3")) +
  scale_fill_manual(name = "Year of UC\nnatural migration", values = sphsu_cols(c("University Blue", "Thistle", "Leaf", "Pumpkin"), names = FALSE)) +
  theme_void() +
  theme(axis.line = element_blank(), legend.margin = margin(c(5, 5, 5, 5)))} |> 
  ggplotly(tooltip = "text", hoveron = "fills") |>
  config(displayModeBar = F) |> 
  layout(legend = list(x = 0.7, y = 0.5)) |> 
  htmlwidgets::saveWidget("graphs/uc_rollout_i.html", selfcontained = TRUE)


# hex it? -----------------------------------------------------------------

hex_dat <- parlitools::local_hex_map

st_crs(hex_dat) <- st_crs(hex_dat)

library(gganimate)
library(transformr)

hex_dat |> 
  left_join(all_la_codes, by = join_by(la_code == LAUA)) |> 
  mutate(year_rollout = factor(year(`UC rollout`), levels = 2015:2018)) |> 
  ggplot() +
  geom_sf(aes(fill = year_rollout)) +
  coord_sf() +
  scale_fill_manual(name = "Year of UC\nnatural migration", values = sphsu_cols(c("University Blue", "Thistle", "Leaf", "Pumpkin"), names = FALSE)) +
  theme_void()

ggsave("graphs/map_UK_rollout_dates_hex.tiff", height = 7000, width = 5000, dpi = 1200, units = "px", compression = "lzw")
ggsave("graphs/map_UK_rollout_dates_hex.png", height = 3500, width = 2500, dpi = 600, units = "px")

p <- hex_dat |>
  left_join(all_la_codes, by = join_by(la_code == LAUA)) |>
  mutate(year_rollout = factor(year(`UC rollout`), levels = 2015:2018)) |>
  mutate(
    time_point = dense_rank(`UC rollout`),
    month = paste(year(`UC rollout`), month(`UC rollout`, label = TRUE)) |> fct_reorder(`UC rollout`)
  ) |>
  nest(dat = -la_code) |>
  mutate(dat = map(dat, \(dat) cross_join(dat, tibble(lag = seq(
    0.9, 0, -0.1
  )))),
  dat = map(dat, \(dat) mutate(dat, time_point = time_point + lag))) |>
  unnest(dat) |>
  ggplot() +
  geom_sf(data = hex_dat, fill = "#eeeeee") +
  geom_sf(aes(
    fill = year_rollout,
    geometry = geometry,
    alpha = lag
  )) +
  scale_alpha_continuous("", range = c(0, 0.5), guide = "none") +
  coord_sf() +
  scale_fill_manual(name = "Year of UC\nnatural migration", values = sphsu_cols(c(
    "University Blue", "Thistle", "Leaf", "Pumpkin"
  ), names = FALSE)) +
  theme_void() +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  # labs(title = "Natural Migration: {closest_state}") +
  ggtitle("Natural Migration: {current_frame}") +
  ease_aes() +
  transition_manual(month, cumulative = TRUE)
transition_states(month)
transition_time(`UC rollout`)
transition_manual(time_point, cumulative = TRUE)

animate(p, renderer = ffmpeg_renderer(), nframes = 30, fps = 2, res = 200, width = 1000, height = 1000)
animate(p, renderer = magick_renderer(), nframes = 30, fps = 2, device = "svg")
anim_save("graphs/anim_rollout_hex.mp3")
anim_save("graphs/anim_rollout_hex_15s.mp4")

{hex_dat |> 
    left_join(all_la_codes, by = join_by(la_code == LAUA)) |> 
    mutate(year_rollout = factor(year(`UC rollout`), levels = 2015:2018)) |> 
    mutate(text = paste0(`Local authority`, "\nUC rollout: ", format.Date(`UC rollout`, format = "%B %Y"))) |> 
    ggplot() +
    geom_sf(aes(fill = year_rollout, text = text)) +
    coord_sf() +
    scale_fill_manual(name = "Year of UC\nnatural migration", values = sphsu_cols(c("University Blue", "Thistle", "Leaf", "Pumpkin"), names = FALSE)) +
    theme_void() +
    theme(axis.line = element_blank(), legend.margin = margin(c(5, 5, 5, 5)))} |> 
  ggplotly(tooltip = "text", hoveron = "fills") |>
  config(displayModeBar = F) |> 
  layout(legend = list(x = 0.7, y = 0.5)) |> 
  htmlwidgets::saveWidget("graphs/uc_rollout_hex_i.html", selfcontained = TRUE)
