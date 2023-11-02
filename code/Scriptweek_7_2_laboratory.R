#'---
#'job title : "Scriptweek_7_2_laboratory"
#'Author : Sangeeta Mahajan
#'Date : 09/28/2023
#'---
 
library(tidyverse)

# Influence of Sample Size

xs <- rnorm(10, mean = 10, sd = 5)

ys <- rnorm(10, mean = 12, sd = 5)

xl <- rnorm(100, mean = 10, sd = 5)

yl <- rnorm(100, mean = 12, sd = 5)

#    Welch’s t-test

#  xs vs. ys


t.test(xs, ys, var.equal = FALSE)

#  xl vs. yl

t.test(xl, yl, var.equal = FALSE)

#  Difference and Uncertainty

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

#  tibble
a_b <- tibble(a1, a2 ,b1, b2) 


#  Welch's test 
t.test(a1, a2, var.equal = FALSE)

t.test(b1, b2, var.equal = FALSE)


  
a_b <- tibble(a1, a2 ,b1, b2) %>% 
  pivot_longer(cols=c("a1","a2","b1","b2"),
               names_to  = "groups",
               values_to  = "values") %>% 
  group_by(groups) %>% 
  summarize(mu_v = mean(values),
            sd_var = sd(values))
                      
print(a_b)   

a_b %>% 
  ggplot(aes(x = groups,
             y = mu_v)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = a_b, # switch data frame
               aes(x = groups,
                   xend = groups,
                   y = mu_v - sd_var,
                   yend = mu_v + sd_var)) +
  geom_point(data = a_b, # switch data frame
             aes(x = groups,
                 y = mu_v, col="red", fill=mu_v),
             size = 3) +
  labs(x = "Groups", # x label
       y = "Values") # y label


