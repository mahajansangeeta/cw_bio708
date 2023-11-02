#' ---
#' title: "Scriptweek_11_2"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "10/31/23"
#' ---

library(tidyverse)
df_mussel <- read_csv(here::here("data_raw/data_mussel.csv"))
print(df_mussel)

# calculate the proportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

# plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_p = seq(-10, 10, length = 100),
                  p = exp(logit_p) / (1 + exp(logit_p)))

df_test %>% 
  ggplot(aes(x = logit_p,
             y = p)) +
  geom_point() +
  geom_line() +
  labs(y = "x",
       x = "logit(p)")  # logit(p)= log(p/1-p)

#use binomial glm
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

summary(m_binom)


# make prediction
df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100))

# y_binom is inv.logit-transformed because predict() returns values in logit-scale
y_binom <- predict(m_binom, newdata = df_pred) %>% boot::inv.logit()

df_pred <- df_pred %>% 
  mutate(y_binom)

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_binom)) +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")


#y~ D(theta)
#y(y = variable)~D(D = type of distribution)theta(theta = parameter eg mean, lambda, probabolity success)
