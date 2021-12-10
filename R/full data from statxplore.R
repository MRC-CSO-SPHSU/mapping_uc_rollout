library(statxplorer)
library(plotly)
library(tidyverse)
library(lubridate)
library(SPHSUgraphs)
`-.gg` <- function(e1, e2) e2(e1)

theme_set(theme_sphsu_light())


# loading data from StatXplore api ----------------------------------------



load_api_key("api_key.txt")


tables <- list()

files <- dir("data") %>% 
  str_subset(".*json")

for (file in files) {
  varname <- str_extract(file, ".*(?=\\.json)")
  if (!(varname %in% names(tables))) {
    tables[[varname]] <- fetch_table(read_file(file.path("data", file)))
  }
  
}

# graph to check all
tables$uc_hh_jcp$dfs[[1]] %>% 
  select(-`Payment Indicator`, jcp = `Jobcentre Plus`, n_h = `Households on Universal Credit`) %>% 
  mutate(Month = dmy(paste(1, Month))) %>% 
  filter(jcp != "Total") %>% 
  group_by(jcp) %>% 
  mutate(rel_n = n_h/max(n_h)) %>% 
  ggplot(aes(Month, n_h, group = jcp)) +
  geom_line(size = 0.2, colour = "grey") +
  geom_vline(xintercept = ymd("2016/05/01")) -
  ggplotly

# Hounslow and Croydon as pilots before launch of full site?


uc_hh_jcp_ts <- tables$uc_hh_jcp$dfs[[1]] %>% 
  select(-`Payment Indicator`, jcp = `Jobcentre Plus`, n_h = `Households on Universal Credit`) %>% 
  mutate(Month = dmy(paste(1, Month)),
         jcp = str_to_title(str_replace_all(jcp, "-", " "))) %>% 
  filter(jcp != "Total") 



# checking for present rollout dates --------------------------------------
# Read table of dates from website
source("R/start dates from gov website.R", echo=TRUE)


# jcps in statxplore but not matching with start dates
xplore_unmatched <- uc_hh_jcp_ts %>% 
  select(jcp) %>% 
  unique() %>% 
  anti_join(jcp_rollout_dates, by = "jcp") %>% 
  arrange(jcp) %>% 
  pull(jcp)


# jcps with start dates but not matching xplore
gov_unmatched <- uc_hh_jcp_ts %>% 
  select(jcp) %>% 
  unique() %>% 
  anti_join(jcp_rollout_dates,. , by = "jcp") %>% 
  arrange(jcp) %>% 
  pull(jcp)

length(gov_unmatched) <- length(xplore_unmatched)

fuzzyjoin::stringdist_join(
  tibble(gov_unmatched),
  tibble(xplore_unmatched),
  by = c("gov_unmatched" = "xplore_unmatched"),
  mode = "inner",
  method = "lcs",
  max_dist = 10,
  distance_col = "dist"
) %>% View()


# all current matches
uc_hh_jcp_ts %>% 
  select(jcp) %>% 
  unique() %>% 
  mutate(id = "xplore") %>% 
  full_join(jcp_rollout_dates %>% select(jcp, `Local authority`) %>% mutate(id = "gov"), by = "jcp") %>% 
  arrange(jcp) %>% 
  # filter(is.na(id.x) | is.na(id.y)) %>%
  View()



# manual arranging --------------------------------------------------------

tibble(
       gov = gov_unmatched,
       match = "",
  xplore = xplore_unmatched
  ) %>%
  mutate(gov = replace_na(gov, "")) #%>% 
  # write_csv("matching_jcps.csv")

keys <- read_csv("matching_jcps_in.csv")

start_dates_la <- keys %>% select(gov, match) %>% 
  right_join(jcp_rollout_dates, by = c("gov" = "jcp")) %>% 
  mutate(jcp = if_else(is.na(match), gov, match)) %>% 
  select(jcp, `Local authority`, full_date)


# joining compete dates ---------------------------------------------------

uc_hh_jcp_starts <- uc_hh_jcp_ts %>% 
  left_join(start_dates_la, by = "jcp")

dates <- uc_hh_jcp_starts %>% 
  select(full_date) %>% 
  unique() %>% 
  filter(!is.na(full_date)) %>% 
  arrange(full_date)

uc_hh_jcp_starts %>% 
  ggplot(aes(Month, n_h, group = jcp)) +
  geom_line(colour = "grey", size = 0.5) -
  ggplotly


{
  temp_dates <- dates %>%
    head(6)
  
  temp_dates %>% 
    mutate(date_df = map(full_date, function(date) {
      uc_hh_jcp_starts %>%
        filter(full_date <= date) %>%
        mutate(displ = if_else(full_date == date, "y", "n")) %>%
        select(jcp, Month, n_h, displ, launch_date = full_date)
    })) %>%
    # filter(full_date > min(full_date)) %>%
    unnest(date_df) %>%
    add_row(tibble(
      full_date = ymd("2015-11-01"), displ = "n"
    )) %>%
    mutate(tooltip = paste0(
      "Job centre: ", jcp,
      ", ", format(Month, "%B %Y"),
      "\nNumber of households on UC: ", format(n_h, big.mark = ",", scientific = FALSE),
      "\nDate UC available from: ", format(full_date, "%B %Y")
    )) %>% 
    ggplot(aes(
      Month,
      n_h,
      group = jcp,
      frame = factor(full_date),
      colour = displ,
      text = tooltip
    )) +
    geom_line(size = 0.5) +
    scale_colour_manual(values = c("y" = "black", "n" = "grey")) +
    geom_vline(data = temp_dates, aes(xintercept = as.numeric(full_date), frame = factor(full_date)),
               colour = "grey", linetype = "dashed") +
    theme(legend.position = "none") +
    scale_y_continuous("Number of households on universal credit", labels = scales::number_format(big.mark = ","))
} %>%
    ggplotly(tooltip = "text") %>% 
  animation_opts(redraw = FALSE, transition = 0) %>% 
  animation_slider(
    currentvalue = list(
      prefix = "Date of rollout: "
    )
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    xaxis = list(fixedrange = TRUE),
    xaxis2 = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE),
    yaxis2 = list(fixedrange = TRUE)
  )

rollout_pltly <- last_plot()

htmlwidgets::saveWidget(rollout_pltly, file = "hosting/public/index.html")

format(500000)