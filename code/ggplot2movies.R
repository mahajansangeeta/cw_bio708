
library(ggplot2movies)
data('movies')
#ex 1a
library(tidyverse)
movies_id<- movies %>% 
  mutate(ID=1:58788)
print(movies_id)
# ex 1b
movies_2<- movies %>%
  filter(year>=2000)
# ex 1c
movies_3<- movies %>%
  select(title,year,budget,length,rating,votes)
# ex 1d
movies_4<- movies %>%
  rename(length_in_minutes=length)
# ex 2
movies_5<- movies %>%
  group_by(year) %>%
  summarize(average_budget=mean(budget,na.rm=TRUE))
# ex 3
dat = tibble(id = 1:10,
             x = rnorm(10),
             y = rnorm(10))
dat_new<- dat %>%
  pivot_longer(cols = c("x","y"),               
               names_to = "names",               
               values_to = "value",
               values_drop_na=TRUE)
# ex 4
movies_6<- movies %>%
  filter(year>=1990) %>% 
  select(title, year, budget, length, rating,votes,mpaa,Action,Drama) %>%
  group_by(mpaa,Action) %>%
  summarize(average_rating=mean(rating,na.rm = TRUE))   