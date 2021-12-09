library(readxl)
library(tidyverse)
library(lubridate)
library(nlme)

# read in and tidy data ---------------------------------------------------

UC_households_LA_Month_2015_2021 <- read_excel("data/UC_households_LA_Month_2015_2021.xlsx", 
                                               skip = 9,n_max = 366)

uc_households_tidy <- UC_households_LA_Month_2015_2021 %>% 
  select(local_authority = ...2, everything(), -Month) %>% 
  filter(!str_detect(local_authority, "^National")) %>% 
  mutate(across(-1, as.numeric)) %>% 
  pivot_longer(-1, names_to = "m_y", values_to = "n_households") %>% 
  filter(!is.na(n_households)) %>% 
  group_by(local_authority) %>% 
  mutate(m_y = dmy(paste("1", m_y)),
         rel_n = n_households/max(n_households)) %>%
  arrange(m_y) %>%
  # ungroup() %>% 
  mutate(month = as.numeric(as.factor(m_y)))


# visualisations ----------------------------------------------------------

uc_households_tidy %>% 
    filter(m_y < ymd("2020-02-01")) %>% 
  nest() %>% 
  ungroup() %>% 
  slice_sample(n = 10) %>%
  unnest(cols = data) %>% 
  mutate(month = as.numeric(as.factor(m_y))) %>% 
  ggplot(aes(m_y, rel_n, colour = local_authority)) +
  # geom_vline(xintercept = ymd("2020-03-01"), colour = "red", linetype = "dashed") +
  geom_line(size = 1)

uc_households_tidy %>% 
  filter(local_authority == "South Lanarkshire")  %>% 
  ggplot(aes(m_y, rel_n)) +
  geom_vline(xintercept = ymd("2020-03-01"), colour = "red", linetype = "dashed") +
  geom_line(size = 1)


# testing fitting segmented regression model ------------------------------

test_data <- uc_households_tidy %>%
  filter(local_authority == "South Lanarkshire")  %>%
  mutate(d1 = sample(subset(month, subset = month < median(month)), 1),
         d2 = sample(subset(month, subset = month > d1), 1),
         d3 = sample(subset(month, subset = month > d2), 1)
         ) %>%
  rowwise() %>%
  mutate(r1 = max(0, month - d1),
         r2 = max(0, month - d2),
         r3 = max(0, month - d3)) %>% 
  ungroup()

model1 <- test_data %>%
  lm(rel_n ~ month + r1 + r2 + r3, data = .)


test_data %>%
  mutate(pred = predict(model1)) %>% 
  ggplot(aes(month)) +
  geom_point(aes(y = rel_n)) +
  geom_line(aes(y = pred))


# fitting a test function -------------------------------------------------


test_paramaters <- function(d, la) {
  
  model1 <- uc_households_tidy %>% 
  filter(local_authority == la)  %>%
  rowwise() %>%
  mutate(covid = ifelse(month > 56, 1, 0),
         r1 = max(0, month - d[1]),
         r2 = max(0, month - d[2]),
         r3 = max(0, month - d[3])
         ) %>% 
  ungroup() %>% 
  gls(rel_n ~ month + covid + r1 + r2 + r3, method = "ML", data = .)
    
  
  # mean(model1$residuals^2)
  logLik(model1)
  
}

test_paramaters(c(5, 15, 52), la = "North Lanarkshire")
test_paramaters(c(10, 22, 44), la = "North Lanarkshire")
test_paramaters(c(27, 57, 60), la = "North Lanarkshire")


opt1a <- optim(c(30, 40, 60), la = "South Lanarkshire",
      method = "SANN",
      control = list(maxit = 1000, fnscale = -1),
      test_paramaters)  # outputs d1 ~ 27, d2 ~ 34, d3 ~ 49

opt1b <- optim(opt1a$par, la = "South Lanarkshire", test_paramaters, control = list(fnscale = -1), method = "BFGS")
opt1_par <- round(sort(opt1b$par), 0)

uc_households_tidy %>%
  filter(local_authority == "South Lanarkshire")  %>%
  rowwise() %>%
  mutate(
    r1 = max(0, month - opt1_par[1]),
    r2 = max(0, month - opt1_par[2]),
    r3 = max(0, month - opt1_par[3])
  ) %>%
  ungroup() %>%
  mutate(covid = ifelse(month > 56, 1, 0))  %T>% {
    gls(rel_n ~ month + covid + r1 + r2 + r3,
        method = "ML",
        data = .) ->> model1
    print(summary(model1))
  } %>% 
mutate(
  pred = predict(model1),
  period = case_when(
    r1 == 0 ~ "base",
    r2 == 0 ~ paste0("slope 1 - from month ", opt1_par[1]),
    r3 == 0 ~ paste0("slope 2 - from month ", opt1_par[2]),
    TRUE ~ paste0("slope 3 - from month ", opt1_par[3])
  )
) %>%
  ggplot(aes(month)) +
  geom_line(aes(y = pred, colour = period, group = 1), size = 2) +
  geom_point(aes(y = rel_n), shape = 3) +
  scale_colour_brewer(palette = "Set1")



# * Test with 4 slope changes ---------------------------------------------

test_paramaters_4 <- function(d, la) {
  
  model1 <- uc_households_tidy %>% 
    filter(local_authority == la)  %>%
    rowwise() %>%
    mutate(covid = ifelse(month > 56, 1, 0),
           r1 = max(0, month - d[1]),
           r2 = max(0, month - d[2]),
           r3 = max(0, month - d[3]),
           r4 = max(0, month - d[4])
    ) %>% 
    ungroup() %>% 
    gls(rel_n ~ month + covid + r1 + r2 + r3 +r4, method = "ML", data = .)
  
  
  # mean(model1$residuals^2)
  logLik(model1)
  
}

opt2a <- optim(c(20, 30, 40, 50), la = "South Lanarkshire",method = "SANN", control = list(
  maxit = 1000,
   fnscale = -1
), test_paramaters_4) # best fit 27 34 43 49

opt2b <- optim(opt2a$par, la = "South Lanarkshire", test_paramaters_4, control = list(fnscale = -1), method = "BFGS")
opt2_par <- round(sort(opt2b$par), 0)

uc_households_tidy %>%
  filter(local_authority == "South Lanarkshire")  %>%
  rowwise() %>%
  mutate(
    r1 = max(0, month - opt2_par[1]),
    r2 = max(0, month - opt2_par[2]),
    r3 = max(0, month - opt2_par[3]),
    r4 = max(0, month - opt2_par[4]),
  ) %>%
  ungroup() %>%
  mutate(covid = ifelse(month > 56, 1, 0))  %T>%  {
    gls(rel_n ~ month + covid + r1 + r2 + r3 + r4,
        method = "ML",
        data = .) ->> model2
    print(summary(model2))
  } %>%
  mutate(
    pred = predict(model2),
    period = case_when(
      r1 == 0 ~ "base",
      r2 == 0 ~ paste0("slope 1 - from month ", 1 + opt2_par[1]),
      r3 == 0 ~ paste0("slope 2 - from month ", 1 + opt2_par[2]),
      r4 == 0 ~ paste0("slope 3 - from month ", 1 + opt2_par[3]),
      TRUE ~ paste0("slope 4 - from month ", 1 + opt2_par[4])
    )
  ) %>%
  ggplot(aes(month)) +
  geom_line(aes(y = pred, colour = period, group = 1), size = 2) +
  geom_point(aes(y = rel_n), shape = 3) +
  scale_colour_brewer(palette = "Set1")

opt1_par
opt2_par

anova(model1, model2)
summary(model1)
summary(model2)

# * Test with 1 slope change before 2020 ---------------------------------------------

uc_households_pre_c <- uc_households_tidy %>% 
    filter(m_y < ymd("2020-02-01"))

test_paramaters_2 <- function(d, la) {
  
  model1 <- uc_households_pre_c %>% 
    filter(local_authority == la)  %>%
    rowwise() %>%
    mutate(r = max(0, month - d[1])) %>% 
    ungroup() %>% 
    gls(rel_n ~ poly(month, 2) + poly(r, 2), method = "ML", data = .)
  
  logLik(model1)
  
}

opt2a <- optim(20, la = "South Lanarkshire", method = "SANN", control = list(
  maxit = 1000,
   fnscale = -1
), test_paramaters_2) # best fit 25
opt1_par <- opt2a$par


opt2b <- optim(opt2a$par, la = "South Lanarkshire", test_paramaters_2, control = list(fnscale = -1), method = "BFGS")
opt2_par <- round(sort(opt2b$par), 0)

uc_households_pre_c %>%
  filter(local_authority == "South Lanarkshire")  %>%
  rowwise() %>%
  mutate(
    r = max(0, month - opt2_par)
  ) %>%
  ungroup() %>%
  mutate(covid = ifelse(month > 56, 1, 0))  %T>%  {
    gls(rel_n ~ poly(month, 2) + poly(r, 2),
        method = "ML",
        data = .) ->> model2
    print(summary(model2))
  } %>%
  mutate(
    pred = predict(model2),
    period = case_when(
      r == 0 ~ "base",
      TRUE ~ paste0("slope  - from month ", 1 + opt2_par)
    )
  ) %>%
  ggplot(aes(month)) +
  geom_line(aes(y = pred, colour = period, group = 1), size = 2) +
  geom_point(aes(y = rel_n), shape = 3) +
  scale_colour_brewer(palette = "Set1")

opt1_par
opt2_par

anova(model1, model2)
summary(model1)
summary(model2)


# Across all LAs - define function ---------------------------------------------
uc_households_pre_c <- uc_households_tidy %>% 
    filter(m_y < ymd("2020-02-01"))

library(furrr)

plan(multicore)

test_params <- function(d, df = df_la) {
  
  dat <- df
  dat %>%
    rowwise() %>%
    mutate(r = max(0, month - d)) %>%
    gls(rel_n ~ poly(month, 2) + poly(r, 2),
        method = "ML",
        data = .) -> md_out
  
  logLik(md_out)
}

detect_month <-
  function(df_la = uc_households_pre_c) {

    opt1 <-
      optim(
        30,
        df = df_la,
        method = "Brent",
        lower = 3, upper = max(df_la$month) - 2,
        control = list(maxit = 1000,
                       fnscale = -1),
        fn = test_params
      )
    
    # opt2 <- optim(opt1$par, df = df_la, fn = test_params, control = list(fnscale = -1), method = "BFGS")
    round(sort(opt1$par), 0)
  }


test_out <- uc_households_pre_c %>% 
  group_by(local_authority) %>% 
  nest() %>% 
  head(20) %>% 
  mutate(start_month = future_map_dbl(data, detect_month))

test_out %>% 
  unnest(data) %>% 
  ggplot(aes(month, n_households)) +
  geom_point(shape = 3) +
  geom_vline(aes(xintercept = start_month)) +
  stat_summary_2d(aes(xmin = after_stat(x), xmax = Inf, ymin = 0, ymax = Inf, z = start_month),
                  geom = "rect",
                  fun = function(df) tibble(x = max(z), y = 1, ymin = 0, ymax = 2)) +
  facet_wrap(~ local_authority)


test_out %>% 
  ungroup() %>% 
  sample_n(1) %>% 
  unnest(data) %>% 
  ggplot(aes(month, n_households)) +
  geom_rect(aes(xmin = start_month, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.01) +
  geom_point(shape = 3)

test_out %>% 
  filter(local_authority == "Hartlepool") %>% 
  unnest(data) %>% 
  rowwise() %>%
  mutate(
    r = max(0, month - start_month)
  ) %>%
  ungroup() %T>%  {
    gls(rel_n ~ poly(month, 2) + poly(r, 2),
        method = "ML",
        data = .) ->> model2
    print(summary(model2))
  } %>%
  mutate(
    pred = predict(model2),
    period = case_when(
      r == 0 ~ "base",
      TRUE ~ paste0("slope  - from month ", 1 + start_month)
    )
  ) %>%
  ggplot(aes(month)) +
  geom_line(aes(y = pred, colour = period, group = 1), size = 2) +
  geom_point(aes(y = rel_n), shape = 3) +
  scale_colour_brewer(palette = "Set1")


test_out %>% 
  filter(local_authority == "Hartlepool") %>% 
  unnest(data) %>% 
  rowwise() %>%
  mutate(
    r = max(0, month - start_month),
    r2 = max(0, month - 17)
  ) %>%
  ungroup() %>%  {
    gls(rel_n ~ poly(month, 2) + poly(r, 2),
        method = "ML",
        data = .) ->> model2
    print(summary(model2))

    gls(rel_n ~ poly(month, 2) + poly(r2, 2),
        method = "ML",
        data = .) ->> model3
    print(summary(model3))
  }

anova(model2, model3)


# running on all ----------------------------------------------------------

predicted_months <- uc_households_pre_c %>% 
  group_by(local_authority) %>% 
  nest() %>% 
  mutate(start_month = future_map_dbl(data, detect_month))

predicted_months %>%
  ungroup() %>% 
  sample_n(20) %>% 
  unnest(data) %>% 
  ggplot(aes(month, rel_n)) +
  geom_point(shape = 3) +
  geom_vline(aes(xintercept = start_month)) +
  facet_wrap(~ local_authority)
