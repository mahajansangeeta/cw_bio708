#' ---
#' title: "Scriptweek_11_laboratory"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "11/02/23"
#' ---

library(tidyverse)

# GLM exercise ------------------------------------------------------------
getwd()
df_fish <- read_csv(here::here("data_raw/data_vpart.csv"))


# species richness --------------------------------------------------------

# species type 
m <- glm(n_sp ~ distance + cat_area + hull_area,
               data = df_fish,
               family = "poisson")
summary(m)

# check mean variance relationship

mean(df_fish$n_sp)
var(df_fish$n_sp)

coef(m)


## small exercise for scaling

x1 <- rnorm(100, mean = 0, sd = 5)
x2 <- rnorm(100, mean =0, sd = 10)

sd(x1); sd(x2)

z1 <- x1/sd(x1)
z2 <- x2/sd(x2)

sd(z1); sd(z2)

## centralize the data

y1 <- rnorm(100, mean = 10, sd = 5)
y2 <- rnorm(100, mean = 2, sd = 5)

mean(y1); mean(y2)
sd(y1); sd(y2)

mean(scale(y1)); mean(scale(y2))
sd(scale(y1)); sd(scale(y2))
#centralizing is subtracting mean
#standardizing is dividing by sd
#scaling is both and needs to be done before using variables with different units


# scaling -----------------------------------------------------------------

#approach 1
m_scale <- glm(n_sp ~ scale(distance) + scale(cat_area) + scale(hull_area),
         data = df_fish,
         family = "poisson")
summary(m_scale)


#approach 2
df_fish_scale <- 
