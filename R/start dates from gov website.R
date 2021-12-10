library(tidyverse)
library(rvest)

uc_rollout_page <- read_html("https://web.archive.org/web/20201007162803/https://www.gov.uk/government/publications/universal-credit-transition-to-full-service/universal-credit-transition-rollout-schedule-march-2018-to-december-2018")

rollout_tables <- uc_rollout_page %>% 
  html_elements(".govspeak table") %>% 
  html_table()

uc_rollout_untidy <- uc_rollout_page %>% 
  html_elements(".govspeak h2, .govspeak h2 + table, .govspeak h3, .govspeak h3 + table") %>% 
  {ifelse(!is.na(html_attr(., "id")), 
    html_text(.), html_table(.))} %>% 
  map(~ tibble(.x) %>% rename_with(~"full_date", ends_with("x"))) %>%
  reduce(bind_rows) %>% 
  fill(full_date) %>% 
  filter(!is.na(`Local authority`))

jcp_rollout_dates <- uc_rollout_untidy %>% 
  mutate(jcp = str_split(`Jobcentre area`, " JCP\\**")) %>% 
  select(-3) %>% 
  unnest(jcp) %>% 
  mutate(jcp = str_trim(str_remove(jcp, "\\(.*\\)")),
         full_date = dmy(paste(1, full_date)),
         jcp = str_to_title(str_replace_all(jcp, "-", " ")),
         jcp = str_replace(jcp, "St$", "Street"),
         jcp = case_when(
           jcp == "Kings Lynn" ~ "King's Lynn",
           jcp == "Caldicott" ~ "Caldicot",
           jcp == "Wolverhampton Molineaux House" ~ "Wolverhampton Molineux House",
           jcp == "Mussleburgh" ~ "Musselburgh",
           jcp == "Marylebone" ~ "St Marylebone",
           jcp == "Dundee" ~ "Dundee City",
           jcp == "Canvey" ~ "Canvey Island",
           jcp == "Chapeltown" ~ "Sheffield Chapeltown",
           jcp == "Aberdeen" ~ "Aberdeen Ebury House",
           jcp == "Newport" & `Local authority` == "Isle of Wight Council" ~ "Newport Isle Of Wight",
           jcp == "Newport" & `Local authority` == "Newport City Council" ~ "Newport Gwent",
           TRUE ~ jcp
         )) %>% 
  filter(jcp != "")
