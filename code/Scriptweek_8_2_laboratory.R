#' ---
#' title: "Scriptweek_8_2_laboratory"
#' author: Sangeeta Mahajan
#' date: "10/05/23"
#' output: html_document
#' ---

library(tidyverse)
data(PlantGrowth)
df_pg <- as_tibble(PlantGrowth)

# estimate overall mean

pg_mu <- mean(PlantGrowth$weight)

df_pg_mu <- df_pg %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight),
            sd_g = sd(weight),
            sq_dev_g = (mu_g - pg_mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_pg_mu)


# figure ----------------------------------------------------------------

df_pg  %>% 
  ggplot(aes(x = group,
             y = weight))+
  geom_jitter(width = 0.1,
              height = 0,
              alpha = 0.25) +
  geom_segment(data = df_pg_mu,
               aes(x = group,
                   xend = group,
                   y = mu_g - sd_g,
                   yend = mu_g + sd_g)) +
  geom_violin(draw_quantiles = aes(0.25, 0.5, 0.75),
              alpha = 0.2) +
  geom_point(data = df_pg_mu,
             aes(x = group,
             y = mu_g),
             size = 3) +
  labs(x = "Group",
       y = "Weight")
  

# ANOVA to examine differences in weight among the different groups
# # anova in R
  
fit <- aov(formula = weight ~ group,
             data = df_pg)


# ANOVA by hand ---------------------------------------------------------
# pull out group weights

x_ctrl <- df_pg %>% 
  filter(group == "ctrl") %>% 
  pull(weight)

x_trt1 <- df_pg %>% 
  filter(group == "trt1") %>% 
  pull(weight)

x_trt2 <- df_pg %>% 
  filter(group == "trt2") %>% 
  pull(weight)

# group means

mu_ctrl <- mean(x_ctrl)
mu_trt1 <- mean(x_trt1)
mu_trt2 <- mean(x_trt2)


# between group variability ---------------------------------------------

sqs_ctrl<- (mu_ctrl - pg_mu)^2*length(x_ctrl)
sqs_trt1<- (mu_trt1 - pg_mu)^2*length(x_trt1)
sqs_trt2<- (mu_trt2 - pg_mu)^2*length(x_trt2)

# overall mean
sqs_pg <- sqs_ctrl + sqs_trt1 + sqs_trt2

sqs_pg <- df_pg_mu %>% 
  mutate(ss = sq_dev_g * n) %>% 
  pull(ss) %>% 
  sum()


# within group variability ----------------------------------------------

dev_ictrl <- (x_ctrl - mu_ctrl)^2
dev_itrt1 <- (x_trt1 - mu_trt1)^2
dev_itrt2 <- (x_trt2 - mu_trt2)^2

sqs_w <- sum(dev_ictrl) + sum(dev_itrt1) + sum(dev_itrt2)

# convert variability into SD

var_pg <- sqs_pg / (3 - 1)
var_w <- sqs_w / (30 - 3)

# get F value

f_value <- var_pg / var_w

# f distribution ----------------------------------------------------------

x <- seq(0, 100, by = 1)
y <- df(x, df1 = 3 - 1, df2 = 30 - 3)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "salmon",
             linetype = "dashed")

# p-value

p_value <- 1 - pf(f_value, df1 = 3 - 1, df2 = 30 - 3)
print(p_value)


# # exercise 3 ------------------------------------------------------------

x <- seq(0, 100, by = 1)
y <- df(x, df1 = 5 - 1, df2 = 30 - 5)
p_value <- 1 - pf(f_value, df1 = 5 - 1, df2 = 30 - 5)
print(p_value) # p_value 0.00494376

x <- seq(0, 100, by = 1)
y <- df(x, df1 = 3 - 1, df2 = 15 - 3)
p_value <- 1 - pf(f_value, df1 = 3 - 1, df2 = 15 - 3)
print(p_value) # p_value 0.02865951

x <- seq(0, 100, by = 1)
y <- df(x, df1 = 4 - 1, df2 = 20 - 4)
p_value <- 1 - pf(f_value, df1 = 4 - 1, df2 = 20 - 4)
print(p_value) # p_value 0.01384048






## exercise 3 ####
# try different numbers of df1 and df2
# 1) we have 5 groups with 30 measures
# df1 = 5 - 1
# df2 = 30 - 5
# use pf() and 
f_value = 4.846

1 - pf(4.846, df1 = 4, df2 = 25)

# 2) we have 3 groups with 15 measures

1 - pf(4.846, df1 = 2, df2 = 12)

# 3) we have 4 groups with 20 measures

1 - pf(4.846, df1 = 3, df2 = 16)

# visual for different f distributions
nx <- 1000
x <- seq(0, 10, length = nx)
df_y <- tibble(y = c(df(x, df1 = 4, df2 = 25),
                     df(x, df1 = 2, df2 = 12),
                     df(x, df1 = 3, df2 = 16))) %>% 
  mutate(x = rep(x, 3),
         df1 = rep(c(4, 2, 3), each = nx),
         df2 = rep(c(25, 12, 16), each = nx),
         df = paste(df1, df2, sep = ","))

df_y %>% 
  ggplot(aes(y = y,
             x = x,
             color = df)) +
  geom_line()