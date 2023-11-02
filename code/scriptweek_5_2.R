library(tidyverse)
setwd("C:/1Sangeeta/1UNCG/github/cw_bio708")
df_h<-read.csv("code/data_raw/data_plant_height.csv")
print(df_h)
mu_df_50<-NULL
mu_df_100<-NULL
var_df_50<-NULL
var_df_100<-NULL
for (i in 1:100) {
  df_50<-df_h %>% 
    sample_n(size=50)
  df_100<-df_h %>% 
    sample_n(size=100)
  mu_df_50[i]<-mean(df_50$height)
  mu_df_100[i]<-mean(df_100$height)
  var_df_50[i]<-var(df_50$height)
  var_df_100[i]<-var(df_100$height)
}
df_tibble<-tibble(mu_df_50,mu_df_100,var_df_50,var_df_100)
library(patchwork)
mu_pop<-mean(df_h$height)
var_pop<-var(df_h$height)
mu_50_g<-df_tibble %>% 
  ggplot(aes(x=mu_df_50))+
  geom_histogram(color="blue")+
  geom_vline(xintercept=mu_pop,color="red")
mu_100_g<-df_tibble %>% 
  ggplot(aes(x=mu_df_100))+
  geom_histogram(color="blue")+
  geom_vline(xintercept=mu_pop,color="red")
var_50_g<-df_tibble %>% 
  ggplot(aes(x=var_df_50))+
  geom_histogram(color="blue")+
  geom_vline(xintercept=var_pop,color="red")
var_100_g<-df_tibble %>% 
  ggplot(aes(x=var_df_100))+
  geom_histogram(color="blue")+
  geom_vline(xintercept=var_pop,color="red")
print(mu_50_g/mu_100_g|var_50_g/var_100_g)
