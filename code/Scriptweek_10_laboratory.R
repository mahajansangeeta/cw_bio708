#' ---
#' title: "Scriptweek_10_laboratory"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "10/24/23"
#' ---

library(tidyverse)
iris <- as_tibble(iris)

print(iris)
### apply filter function separately
df1 <- iris %>% 
  filter(Species == "setosa")

m1 <- lm(Sepal.Width ~ Petal.Width,
         data = df1)

summary(m1)

df2 <- iris %>% 
  filter(Species == "versicolor")

m2 <- lm(Sepal.Width ~ Petal.Width,
         data = df2)

summary(m2)

df3 <- iris %>% 
  filter(Species == "virginica")

m3 <- lm(Sepal.Width ~ Petal.Width,
         data = df3)

summary(m3)

### skip typing species names
sp <- unique(iris$Species)

df1 <- iris %>% 
  filter(Species == sp[1])

df2 <- iris %>% 
  filter(Species == sp[2])

df3 <- iris %>% 
  filter(Species == sp[3])

### lapply
sp <- unique(iris$Species)

list0 <- lapply(X = 1:3, function(i) {
  # i will be substituted by what's in `X`...1, 2, 3
  df_temp <- iris %>% 
    filter(Species == sp[i])
  
  m <- lm(Sepal.Width ~ Petal.Width,
          data = df_temp)
  return(m)
})

list0 <- lapply(X = sp, function(i) {
  # i will be substituted by what's in `X`...setota, versicolor, virginica
  df_temp <- iris %>% 
    filter(Species == i)
  
  m <- lm(Sepal.Width ~ Petal.Width,
          data = df_temp)
  return(m)
})

## 6.4.2 Multiple explanatory variables
### add the second predictor to lm()
### lm(y ~ x1 + x2, ...)

x1 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = df1)

summary(m1)
summary(x1)

x2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = df2)

summary(m2)
summary(x2)

x3 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = df3)

summary(m3)
summary(x3)














