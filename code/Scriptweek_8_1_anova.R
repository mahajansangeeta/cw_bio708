#' ---
#' title: "Scriptweek_8_1_anova"
#' author: Sangeeta Mahajan
#' output: html_document
#' date: "10/03/2023"
#' ---
  
library(tidyverse)

df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

# figure
# geom_violin() - function for violin plots
# geom_jitter() - jittered points

df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency


# estimate overall mean
mu <- mean(df_anova$length)


# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group
print(df_g)


# anova in R

fit <- aov(formula = length ~ lake,
          data = df_anova)

# behind Anova

mu <- mean(df_anova$length)

x_a <- df_anova %>% 
  filter(lake == "a") %>% 
  pull(length)

x_b <- df_anova %>% 
  filter(lake == "b") %>% 
  pull(length)

x_c <- df_anova %>% 
  filter(lake == "c") %>% 
  pull(length)

mu_a <- mean(x_a)
mu_b <- mean(x_b)
mu_c <- mean(x_c)

# variability

ss_ga<- (mu_a - mu)^2*length(x_a)
ss_gb<- (mu_b - mu)^2*length(x_b)
ss_gc<- (mu_c - mu)^2*length(x_c)

ss_g<- ss_ga + ss_gb + ss_gc # estimate overall mean
mu <- mean(df_anova$length)

df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarise(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group


ss_g <- df_g %>% 
  mutate(ss = dev_g * n) %>% 
  pull(ss) %>% 
  sum()

# within group variability

dev_ia <- (x_a - mu_a)^2
dev_ib <- (x_b - mu_b)^2
dev_ic <- (x_c - mu_c)^2

ss_w <- sum(dev_ia) + sum(dev_ib) + sum(dev_ic)

df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

# convert variability into SD

var_g <- ss_g / (3 - 1)
var_w <- ss_w / (150 - 3)

# get F value
f_value <- var_g / var_w


# f distribution ----------------------------------------------------------

x <- seq(0, 10, by = 0.1)
y <- df(x, df1 = 3 - 1, df2 = 150 - 3)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "salmon",
             linetype = "dashed")

p_value <- 1 - pf(f_value, df1 = 3 - 1, df2 = 150 - 3)



  
             







