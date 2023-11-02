#' ---
#' title: "Scriptweek_9_laboratory"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "10/17/23"
#' ---

library(tidyverse)
df_iris <- as_tibble(iris)
df_setosa <- df_iris[1:50, ] %>%  #OR filter(species=="setosa")
  subset(select = - Species)
  
df_versicolor <- df_iris[51:100, ] %>% #OR filter(species=="versicolor")
  subset(select = - Species)

df_virginica <- df_iris[101:150, ] %>%  #OR filter(species=="virginica")
  subset(select = - Species)

### OR lapply unique()function
#lapply(x = 1:3, function(i){
#list0 <-iris %>% 
#  filter(species == sp[i])})


### skip typing species names
#unique(iris$Sepal.Length)
#df_setosa <- filter(Species==sp[1]) and so on


# regression --------------------------------------------------------------

# for setosa

setosa_lm <- lm(data = df_setosa, formula = Sepal.Width ~ Petal.Width)
alpha_setosa <- coef(setosa_lm)[1]
beta_setosa <- coef(setosa_lm)[2]
summary(setosa_lm)

#figure

df_setosa %>% 
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_setosa,
              slope = beta_setosa) 

#for versicolor

versicolor_lm <- lm(data = df_versicolor, formula = Sepal.Width ~ Petal.Width)
alpha_versicolor <- coef(versicolor_lm)[1]
beta_versicolor <- coef(versicolor_lm)[2]
summary(versicolor_lm)

#figure

df_versicolor %>% 
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_versicolor,
              slope = beta_versicolor)

# for virginica

virginica_lm <- lm(data = df_virginica, formula = Sepal.Width ~ Petal.Width)
alpha_virginica <- coef(virginica_lm)[1]
beta_virginica <- coef(virginica_lm)[2]
summary(virginica_lm)

#figure

df_virginica %>% 
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_virginica,
              slope = beta_virginica)


# Adding Petal.Length as additional explanatory variable ------------------

# for setosa

setosa_lm1 <- lm(data = df_setosa, formula = Sepal.Width ~ Petal.Width + Petal.Length)
alpha1_setosa <- coef(setosa_lm1)[1]
beta1_setosa <- coef(setosa_lm1)[2]
summary(setosa_lm1)


#for versicolor

versicolor_lm1 <- lm(data = df_versicolor, formula = Sepal.Width ~ Petal.Width + Petal.Length)
alpha1_versicolor <- coef(versicolor_lm1)[1]
beta1_versicolor <- coef(versicolor_lm1)[2]
summary(versicolor_lm1)

#for virginica

virginica_lm1 <- lm(data = df_virginica, formula = Sepal.Width ~ Petal.Width + Petal.Length)
alpha1_virginica <- coef(virginica_lm1)[1]
beta1_virginica <- coef(virginica_lm1)[2]
summary(virginica_lm1)
