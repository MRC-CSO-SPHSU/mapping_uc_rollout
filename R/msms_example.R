library(tidyverse)
df <- tribble(~a0, ~z1, ~a1, ~y, ~n,
0, 0, 0, 87.288, 209271,
0, 0, 1, 112.107, 93779,
0, 1, 0, 119.654, 60657,
0, 1, 1, 144.842, 136293,
1, 0, 0, 105.282, 134781,
1, 0, 1, 130.184, 60789,
1, 1, 0, 137.720, 93903,
1, 1, 1, 162.832, 210527)


mod1 <- glm(a0 ~ 1, data = df, family = binomial, weights = n)

mod1

mod2 <- glm(a1 ~ 1, data = df, family = binomial, weights = n)

mod2

mod3 <- glm(a1 ~ z1, data = df, family = binomial, weights = n)

mod3

df |> 
  mutate(p1 = predict(mod1, type = "response"),
         p2 = predict(mod2, type = "response"),
         p3 = predict(mod3, type = "response"),
         sw = case_when(
           a0 == 1 & a1 == 1 ~ (p1/p1) * (p2/p3),
           a0 == 1 & a1 == 0 ~ (p1/p1) * ((1 - p2)/(1 - p3)),
           a0 == 0 & a1 == 1 ~ ((1 - p1)/(1 - p1)) * (p2/p3),
           a0 == 0 & a1 == 0 ~ ((1 - p1)/(1 - p1)) * ((1 - p2)/(1 - p3))
         ),
         pn = n * sw) %$%
    lm(y ~ a0*a1, weights = pn) |> 
  summary()


library(magrittr)
mtcars %$% 
  glm(am ~ cyl, family = binomial) |> 
  predict(type = "response")


# Try a hairy example? ----------------------------------------------------


pop <- tibble(id = 1:100,
              uc = rep(0:1, each = 50),
              benefit = rnorm(100, 100, 20) + uc * rnorm(100, -20, 5))

pop %$%
  lm(benefit ~ uc) |> 
  summary()

pop |> 
  mutate(mean_ben = mean(benefit),
         sd_ben = sd(benefit),
         p_ben = pnorm(benefit, mean = mean_ben, sd = sd_ben)) |> 
  group_by(uc) |> 
  mutate(p_ben_exp = pnorm(benefit, mean = mean(benefit), sd = sd(benefit)),
         ipw = p_ben/p_ben_exp) |> 
  ungroup() %$%
  lm(benefit ~ uc, weights = ipw) |> 
  summary()
