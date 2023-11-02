#'---
#'title:  "Screeptweek_6_2_probabilities_laboratory"
#'date : 09_21_2023
#'author : sangeeta mahajan
#'---
#Laboratory

# normal distribution
library(tidyverse)
library(ggplot2)
x_g <- rnorm(n = 50, mean = 10, sd = 0.8)
print(x_g)


#calculate mean and sd here
mu_x <- mean(x_g)
sd_x <- sd(x_g)

#probability density

pd_x <- dnorm(x_g, mean = mu_x, sd = sd_x)
df_x <- data.frame(df_x = rnorm(n = 50, mean = 10, sd = 0.8))

tibble(y = pd_x, x = x_g) %>% 
  ggplot(aes(x = x_g, y = y))+
  geom_line()+
  labs(y="Probability density")
  
  


#calculate mean and sd here

x_g_min <- floor(min(x_g))
x_g_max <- ceiling(max(x_g))
#p_x <- pnorm(x_g)
bin_x <- seq(floor(min(x_g)), ceiling(max(x_g)), by=1)
print(bin_x)

df_x <- data.frame(x_g = rnorm(n=50, mean=10, sd=0.8))
p <- NULL # empty object for probability
for (i in 1:(length(bin_x) - 1)) {
  p[i] <- pnorm(bin_x[i+1], mean = mu_x, sd = sd_x) - pnorm(bin_x[i], mean = mu_x, sd = sd_x)
}

df_x_prob <- tibble(p = p,
                    bin_x = bin_x[-length(bin_x)] + 0.5) %>% 
  mutate(freq = p * nrow(df_x))

df_x %>% 
  ggplot(aes(x = x_g)) +
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_point(data = df_x_prob,
             aes(y = freq,
                 x = bin_x),
             color="salmon") +
  geom_line(data = df_x_prob,
            aes(y = freq,
                x = bin_x),
            color="salmon")




#poisson distribution
z <- rpois(1000, lambda = 340)
lambda_hat <- mean(z)

#calculate probability mass
z_at <- seq(min(z), max(z), by = 1)
pm_z <- dpois(z_at, lambda = lambda_hat)
df_z_at <- tibble(z_at = z_at, pm_z = pm_z) %>% 
  mutate(freq = pm_z * length(z))

#figure
df_z <- tibble(z = z)

df_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram(binwidth = 1) +
  geom_point(data = df_z_at,
             aes(x = z_at, y = freq)) +
  geom_line(data = df_z_at,
            aes(x = z_at, y = freq))
print(df_z)
  




  