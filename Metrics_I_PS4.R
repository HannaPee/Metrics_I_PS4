#################################
########## Metrics PS4 ##########
#################################

#load packages#
library(dplyr)
library(ggplot2)
library(readr)
library(xtable)
library(fixest)
library(broom)


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
 
#Create epsilon and y variable # 

df_panel <- df_panel %>%
  mutate(
    epsilon = rnorm(2240, mean = 1, sd = 1), 
    y = alpha + delta + 1*D5 + 2*D6 + 3*D7 + 4*D8 + epsilon)


### a ### 

# Estimate model # 

model_1 <- feols(
  y ~ D1 + D2 + D3 + D5 + D6 + D7 + D8 | i + t,
  data = df_panel,
  vcov = ~ i
)

# Summary #

summary(model_1)
coef(model_1)
vcov(model_1)

# Result to overleaf #
etable(model_1, tex = TRUE, 
       title = "Effect of Treatment on Outcome $y$",
       label = "tab:main_results", 
       file = "regression_1.tex")


# Plot the results # 

result_df <- tidy(model_1, conf.int = TRUE)
result_df <-bind_rows(result_df, 
                     tibble(term = "D4", 
                            estimate = 0, 
                            std.error = NA, 
                            conf.low = NA, 
                            conf.high = NA) )
result_df$time <- as.numeric(gsub("D", "", result_df$term))


plot_1 <- ggplot(result_df, aes(x = time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time", y = "Estimate") +
  theme_minimal()

print(plot_1)


ggsave(
  filename = "DiD_estimates.pdf",
  plot     = plot_1,
  width    = 8,
  height   = 6
)


##### b #######

# Estimate model 2 # 

model_2 <- feols(
  y ~ D5 + D6 + D7 + D8 | i + t,
  data = df_panel,
  vcov = ~ i
)

# Result to overleaf #
etable(model_2, tex = TRUE, 
       title = "Model 2 results",
       label = "tab:main_results", 
       file = "regression_2.tex")


# Plot the results # 

result_2_df <- tidy(model_2, conf.int = TRUE)
result_2_df <-bind_rows(result_2_df, 
                      tibble(term = "D4", 
                             estimate = 0, 
                             std.error = NA, 
                             conf.low = NA, 
                             conf.high = NA) )
result_2_df$time <- as.numeric(gsub("D", "", result_2_df$term))


plot_2 <- ggplot(result_2_df, aes(x = time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time", y = "Estimate") +
  theme_minimal()

print(plot_2)


ggsave(
  filename = "DiD_estimates_2.pdf",
  plot     = plot_2,
  width    = 8,
  height   = 6
)


###### c #####

# Define a new variable # 

df_panel <- df_panel %>% mutate (D_post = ever_treated*(t >=5))


# Estimate model 3 # 

model_3 <- feols(
  y ~ D_post | i + t,
  data = df_panel,
  vcov = ~ i
)

# Result to overleaf #
etable(model_3, tex = TRUE, 
       title = "Model 3 results",
       label = "tab:main_results", 
       file = "regression_3.tex")





