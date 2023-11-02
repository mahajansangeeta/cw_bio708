#' ---
#' title: "Scriptweek_11"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "10/26/23"
#' ---

library(tidyverse)
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)


# Generalized Linear Model -------------------------------------------------

m_normal <- lm(count ~ nitrate,
               df_count)

summary(m_normal)

# figure with normal distribution

# extract estimates
alpha <- coef(m_normal)[1] # intercept
beta <- coef(m_normal)[2] # slope

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)


#using Poisson distribution

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
summary(m_pois)


# parameter estimates and their SEs
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se

print(z_value)

# estimate Pr(>|z|) using a standardized normal distribution
p_value <- (1 - pnorm(abs(z_value), mean = 0, sd = 1)) * 2
print(p_value)

# figure of Poisson distribution

# make predictions
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

# y_hat is exponentiated because predict() returns values in log-scale
y_normal <- predict(m_normal, newdata = df_pred)
y_hat <- predict(m_pois, newdata = df_pred) %>% exp()

df_pred <- df_pred %>% 
  mutate(y_normal,
         y_hat)

# figure with Poisson distribution
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_normal),
            linetype = "dashed") +
  geom_line(data = df_pred,
            aes(y = y_hat),
            color = "salmon")


