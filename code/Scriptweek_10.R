#' ---
#' title: "Scriptweek_10"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "10/19/23"
#' ---


library(tidyverse)

# Two-Group Case ----------------------------------------------------------
getwd()
df_fl <- read_csv("C:/1Sangeeta/1UNCG/github/cw_bio708/data_raw/data_fish_length.csv")
print(df_fl)

# group means
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

# mu_a: should be identical to intercept
v_mu[1]

# mu_b - mu_a: should be identical to slope
v_mu[2] - v_mu[1]

# in lm(), letters are automatically converted to 0/1 binary variable.
# alphabetically ordered (in this case, a = 0, b = 1)
m <- lm(length ~ lake,
        data = df_fl)

summary(m)

# apply t-test 

lake_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

lake_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

lake_t <- t.test(lake_b, lake_a , var.equal = TRUE)

# ANOVA data set -----------------------------------------------------------

df_anova <- read_csv("C:/1Sangeeta/1UNCG/github/cw_bio708/data_raw/data_fish_length_anova.csv")
print(df_anova)

# group means
v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

print(c(v_mu[1], # mu_a: should be identical to intercept
        v_mu[2] - v_mu[1], # mu_b - mu_a: should be identical to the slope for lake b
        v_mu[3] - v_mu[1])) # mu_c - mu_a: should be identical to the slope for lake c

# lm() output
m <- lm(length ~ lake,
        data = df_anova)

summary(m)

# aov output

m_aov <- aov(length ~ lake,
             data = df_anova)

summary(m_aov) # compare lm and aov outputs


# Combine Multiple Types of Variables -------------------------------------

# convert the data format to tibble
iris <- as_tibble(iris)
print(iris)

distinct(iris, Species)

# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)


# # create a data frame for prediction ------------------------------------

# variable names must be identical to the original data frame for analysis
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

# make prediction based on supplied values of explanatory variables
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) # redefine y values for lines; x and color are inherited from ggplot()






