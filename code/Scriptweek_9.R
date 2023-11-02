#' ---
#' title: "Scriptweek_9"
#' author: Sangeeta Mahajan
#' output: html_document
#' date:  "10/12/23"
#' ---

library(tidyverse)
getwd()
df_algae <- read_csv("C:/1Sangeeta/1UNCG/github/cw_bio708/data_raw/data_algae.csv")
print(df_algae)

# figure ------------------------------------------------------------------

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

# fit linear regression to biomass - conductivity -------------------------
# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

#  extracts estimated coefficients ----------------------------------------
# coef() extracts estimated coefficients
# e.g., coef(m)[1] is (Intercept)

alpha <- coef(m)[1] # 1 means intercept
beta <- coef(m)[2]  # 2 means slope

# figure ------------------------------------------------------------------

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line
#get residuals

y_hat <- alpha + beta*df_algae$conductivity #OR
epsilon <- df_algae$biomass - y_hat
df_algae %>% 
  mutate(y_hat = alpha + beta*conductivity, epsilon = biomass - y_hat)
sd(epsilon)

# create matrix X
v_x <- df_algae %>% pull(conductivity)
X <- cbind(1, v_x)

# create a vector of y
Y <- df_algae %>% pull(biomass)

# %*%: matrix multiplication
# t(): transpose a matrix
# solve(): computes the inverse matrix
theta <- solve(t(X) %*% X) %*% t(X) %*% Y
print(theta)

m <- lm(biomass ~ conductivity,
        data = df_algae)

coef(m)

# extract coefficients
theta <- coef(m)

# extract standard errors
se <- sqrt(diag(vcov(m)))

# t-value
t_value <- theta / se
print(t_value)

# calculate p value -------------------------------------------------------
# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)

print(p_beta)

resid(m) # residuals in R

# eps - stands for epsilon
eps <- resid(m)
head(eps)

# pull vector data
v_x <- df_algae %>% pull(conductivity)
v_y <- df_algae %>% pull(biomass)

# theta[1] = alpha
# theta[2] = beta
error <- v_y - (theta[1] + theta[2] * v_x)

# cbind() combines vectors column-wise
# head() retrieves the first 6 rows
head(cbind(eps, error))

# round values at 5 decimal
eps <- round(eps, 5)
error <- round(error, 5)

# eps == error return "TRUE" or "FALSE"
# all() returns TRUE if all elements meet eps == error
all(eps == error)

# add error column
df_algae <- df_algae %>% 
  mutate(eps = eps)

# visualization of the errors ---------------------------------------------

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed")

# residual variance
ss <- sum(resid(m)^2)

# null variance
ss_0 <- sum((v_y - mean(v_y))^2)


#  coefficient of determination -------------------------------------------

r2 <- 1 - ss / ss_0

print(r2)

summary(m)
