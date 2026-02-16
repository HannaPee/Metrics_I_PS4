#################################
########## Metrics PS4 ##########
#################################

#load packages#
library(dplyr)
library(ggplot2)
library(readr)
library(xtable)

############# Q2 ###############

### Generate data ###

#Create i variables # 
df_i <- tibble(
  i =1:280,
  alpha =  rnorm(280, mean = 0, sd = 1),
  ever_treated = ifelse(i <= 145, 1, 0))

#Create t-variables# 

df_t <- tibble(
  t = 1:8, 
  delta = rnorm(8, mean = 0, sd = 1))


# Create a panel # 

df_panel <- merge(df_i, df_t)

# create D variable # 
  
for (tau in 1:8) {
  df_panel <- df_panel %>%
    mutate(!!paste0("D", tau) := ever_treated * (t == tau))
}
 
#Create epsilon and y y variable # 

df_panel <- df_panel %>%
  mutate(
    epsilon = rnorm(2240, mean = 1, sd = 1), 
    y = alpha + delta + 1*D5 + 2*D6 + 3*D7 + 4*D8 + epsilon)


### a ### 










