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
  str_subset("^uc.*json")

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


# Jobseekers claimants as baseline?
js_by_la <- fetch_table(read_file(file.path("data", "js_pp_la.json")))

js_by_la$dfs[[1]] %>% 
  select(`Local authority` = 1, Quarter, n_p_js = 3) %>% 
  mutate(Month = dmy(paste0("1-", Quarter)), .keep = "unused") %>% 
  nest(-`Local authority`) %>% 
  sample_n(10) %>% 
  unnest(data) %>% 
  ggplot(aes(Month, -n_p_js, colour = `Local authority`)) +
  geom_line()

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
  temp_dates <- dates
  
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



# by LA -------------------------------------------------------------------
library(gganimate)

anim <- start_dates_la %>% 
  # head(10) %>%
  group_by(`Local authority`) %>%
  summarise(start = min(full_date), end = max(full_date)) %>%
  ggplot(aes(xmin = start, xmax = end, ymin = 0, ymax = 1, group = `Local authority`)) + 
  geom_rect(aes(fill = `Local authority`), colour = "black", alpha = 0.6) +
  theme(legend.position = "none") +
  geom_text(aes(y = 0.5, x = end, label = str_wrap(`Local authority`, 10)), hjust = 0, nudge_x = 10) +
  scale_x_date(expand = expansion(add = c(10, 100))) +
  transition_states(`Local authority`, transition_length = 1, state_length = 2)
  


la_rollout_periods <- start_dates_la %>%
  group_by(`Local authority`) %>%
  summarise(start = min(full_date), end = max(full_date)) %>%
  arrange(start) %>%
  mutate(LA = paste(
    str_pad(as.integer(as_factor(`Local authority`)), 3, pad = "0"), "-", `Local authority`
  ))


uc_hh_by_la <- la_rollout_periods %>%
    mutate(date_df = map(`Local authority`, function(la) {
      uc_hh_jcp_starts %>%
        filter(`Local authority` == la) %>%
        select(-`Local authority`)
    })) %>%
    unnest(date_df) %>% 
  mutate(tooltip = paste0(
    "Job centre: ", jcp,
    ", ", format(Month, "%B %Y"),
    "\nNumber of households on UC: ", format(n_h, big.mark = ",", scientific = FALSE),
    "\nDate UC available from: ", format(full_date, "%B %Y")
  ))


{
    la_rollout_periods %>%
    ggplot(aes(frame = as_factor(`Local authority`))) +
    geom_rect(
      aes(
        xmin = start,
        xmax = end,
        ymin  = 0,
        ymax = 50000,
        text = `Local authority`
      ),
      colour = "black",
      fill = "lightblue",
      alpha = 0.5
    ) +
    coord_cartesian(ylim = c(0, max(uc_hh_jcp_starts$n_h))) +
    geom_line(data = uc_hh_by_la, aes(Month, n_h, text = tooltip, group  = jcp)) +
    scale_y_continuous("Number of households on universal credit",
                       labels = scales::number_format(big.mark = ","),
                       expand = expansion(mult = c(0, NA)))
  } %>%
  ggplotly(tooltip = "text") %>%
  animation_opts(redraw = TRUE, transition = 0) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Local authority: "
    )
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    xaxis = list(fixedrange = TRUE),
    xaxis2 = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE),
    yaxis2 = list(fixedrange = TRUE)
  )
  

rollout_byla <- last_plot()

htmlwidgets::saveWidget(rollout_byla, file = "hosting/public/by_la/index.html")






# for >1 month ------------------------------------------------------------

la_rollout_periods_1pm <- start_dates_la %>%
  group_by(`Local authority`) %>%
  summarise(start = min(full_date), end = max(full_date)) %>%
  mutate(t = end-start) %>% 
  filter(t>0) %>% 
  arrange(start) %>%
  mutate(LA = paste(
    str_pad(as.integer(as_factor(`Local authority`)), 3, pad = "0"), "-", `Local authority`
  ))


uc_hh_by_la_1pm <- la_rollout_periods_1pm %>%
    mutate(date_df = map(`Local authority`, function(la) {
      uc_hh_jcp_starts %>%
        filter(`Local authority` == la) %>%
        select(-`Local authority`)
    })) %>%
    unnest(date_df) %>% 
  mutate(tooltip = paste0(
    "Job centre: ", jcp,
    ", ", format(Month, "%B %Y"),
    "\nNumber of households on UC: ", format(n_h, big.mark = ",", scientific = FALSE),
    "\nDate UC available from: ", format(full_date, "%B %Y")
  ))


{
    la_rollout_periods_1pm %>%
    ggplot(aes(frame = as_factor(`Local authority`))) +
    geom_rect(
      aes(
        xmin = start,
        xmax = end,
        ymin  = 0,
        ymax = 50000,
        text = `Local authority`
      ),
      colour = "black",
      fill = "lightblue",
      alpha = 0.5
    ) +
    geom_line(data = uc_hh_by_la_1pm, aes(Month, n_h, text = tooltip, group  = jcp)) +
    coord_cartesian(ylim = c(0, max(uc_hh_jcp_starts$n_h))) +
    scale_y_continuous("Number of households on universal credit",
                       labels = scales::number_format(big.mark = ","),
                       expand = expansion(mult = c(0, NA)))
  } %>%
  ggplotly(tooltip = "text") %>%
  animation_opts(redraw = TRUE, transition = 0) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Local authority: "
    )
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    xaxis = list(fixedrange = TRUE),
    xaxis2 = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE),
    yaxis2 = list(fixedrange = TRUE)
  )
  

rollout_byla_1pm <- last_plot()

htmlwidgets::saveWidget(rollout_byla_1pm, file = "hosting/public/by_la_1pm/index.html")



# People on UC by employment status ---------------------------------------

uc_pp_jcp_empl_ts <- tables$uc_pp_jcp_empl$dfs[[1]] %>% 
  select(jcp = `Jobcentre Plus`, Month, n_p = `People on Universal Credit`, emp = `Employment indicator`) %>% 
  mutate(Month = dmy(paste(1, Month)),
         jcp = str_to_title(str_replace_all(jcp, "-", " "))) %>% 
  filter(jcp != "Total") %>% 
  right_join(jcp_rollout_dates, by = c("jcp"))

{
  uc_pp_jcp_empl_ts %>%
    group_by(`Local authority`, Month, emp) %>%
    summarise(n_p = sum(n_p), .groups = "drop_last") %>%
    # filter(`Local authority` == "Aberdeen City Council"| `Local authority` == "Allerdale Borough Council") %>%
    arrange(desc(Month)) %>% 
    filter(emp != "Total") %>%
    mutate(
      tooltip = paste0(
        "Local authority: ", `Local authority`,
        "\nMonth: ", Month,
        "\nNumber of individuals ", str_to_lower(emp), ": ", n_p
      ),
      n_p = if_else(emp == "Not in employment", sum(n_p), n_p),
    ) %>%
    group_by(`Local authority`) %>% 
    mutate(p_p = n_p/max(n_p)) %>% 
    ungroup() %>% 
    ggplot(
      aes(frame = as_factor(`Local authority`))
      ) +
    geom_rect(
      data = la_rollout_periods, # %>%
        # filter(`Local authority` == "Aberdeen City Council"| `Local authority` == "Allerdale Borough Council"),
      aes(
        xmin = start,
        xmax = end,
        ymin  = 0,
        ymax = 1,
        text = paste0(`Local authority`, " rollout period\n(", start, "-", end, ")")
      ),
      colour = "black",
      fill = "lightblue",
      alpha = 0.5
    ) +
    geom_area(aes(
      Month,
      p_p,
      fill = fct_rev(emp),
      group = interaction(`Local authority`, emp),
      text = tooltip
    ),
    position = "identity") +
    scale_fill_sphsu(palette = "hot", name = "Employment status") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_y_continuous(
      "Proportion of max individuals on universal credit",
      labels = scales::percent,
      expand = expansion(mult = c(0, NA))
    )
  } %>%
  ggplotly(tooltip = "text") %>%
  animation_opts(redraw = TRUE, transition = 0) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Local authority: "
    )
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    xaxis = list(fixedrange = TRUE),
    xaxis2 = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE),
    yaxis2 = list(fixedrange = TRUE),
    legend = list(x = 0.1, y = 0.9)
  )


rollout_by_la_emp <- last_plot()

htmlwidgets::saveWidget(rollout_by_la_emp, file = "hosting/public/by_la_emp/index.html")
