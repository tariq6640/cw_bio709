pacman::p_load(tidyverse,
               ggeffects,
               mgcv)
#pracice data
link <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_water_temp.csv"

(df_wt_raw <- read_csv(link))

##check data format
sapply(df_wt_raw, class)

df_wt <- df_wt_raw %>% 
  mutate(date = as.Date(date_time,
                        format = "%m/%d/%Y"),
year = year(date),
month = month(date)) %>%
  filter(year == 2022,
         between(month, left = 3, right = 10))

df_wt_daily <- df_wt %>%
  group_by(date, site) %>% 
  summarize(temp = mean(temp, na.rm = TRUE) %>% round(3), 
            groups = "drop")
#visualize
df_wt_daily %>% 
  ggplot(aes(
    x = date, 
    y = temp,
    color = site)) +
  geom_point(alpha = 0.25) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Water Temperature",
    color = "Wetland Type"
  )

##convert data type
df_wt_daily <- df_wt_daily %>% 
  mutate(j_date = yday(date),
         site = factor(site)
  )

##linear modeling approach
m_glm <- glm(
  temp ~ j_date + site,
  data = df_wt_daily,family = "gaussian")
summary(m_glm)

# Generate model predictions across all Julian days and wetland sites

df_pred <- ggpredict(m_glm,
                     terms = c(
                       "j_date [all]",  # Use all observed values of Julian day
                       "site [all]"     # Generate predictions for all levels of the factor 'site'
                     )
) %>% 
  # Rename the default columns to match the original dataset
  rename(site = group,  # 'group' from ggpredict() corresponds to the factor variable 'site'
         j_date = x     # 'x' from ggpredict() corresponds to the predictor 'j_date'
  )

# Plot daily water temperature and overlay model predictions

df_wt_daily %>% 
  ggplot(aes(
    x = j_date,   # Julian day on x-axis
    y = temp,     # Observed daily temperature on y-axis
    color = site  # Color points by wetland type (factor)
  )) +
  geom_point(alpha = 0.25) +
  # Overlay predicted values from the model
  # df_pred contains predictions from ggpredict()
  # aes(y = predicted) maps the model's predicted temperature to y
  geom_line(data = df_pred,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",         # x-axis label
       y = "Water Temperature",   # y-axis label
       color = "Wetland Type"     # Legend title for site color
  )

# Generate model predictions across all Julian days and wetland sites
library(dplyr)
df_pred <- ggpredict(m_glm,
                     terms = c(
                       "j_date [all]",  # Use all observed values of Julian day
                       "site [all]"     # Generate predictions for all levels of the factor 'site'
                     )
) %>% 
  # Rename the default columns to match the original dataset
  rename(site = group,  # 'group' from ggpredict() corresponds to the factor variable 'site'
         j_date = x     # 'x' from ggpredict() corresponds to the predictor 'j_date'
  )

# Plot daily water temperature and overlay model predictions
df_wt_daily %>% 
  ggplot(aes(
    x = j_date,   # Julian day on x-axis
    y = temp,     # Observed daily temperature on y-axis
    color = site  # Color points by wetland type (factor)
  )) +
  geom_point(alpha = 0.25) +
  # Overlay predicted values from the model
  # df_pred contains predictions from ggpredict()
  # aes(y = predicted) maps the model's predicted temperature to y
  geom_line(data = df_pred,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",         # x-axis label
       y = "Water Temperature",   # y-axis label
       color = "Wetland Type"     # Legend title for site color
  )

library(mgcv)
m_gam <- gam(temp ~ site + s(j_date),
             data = df_wt_daily,
             family = "gaussian")

summary(m_gam)
df_pred_gam <- ggpredict(m_gam,
                         terms = c(
                           "j_date [all]", 
                           "site [all]")
) %>% 
  rename(site = group,
         j_date = x)

df_wt_daily %>% 
  ggplot(aes(
    x = j_date,
    y = temp, 
    color = site
  )) +
  geom_point(alpha = 0.25) +
  # Overlay predicted values from the GAM
  geom_line(data = df_pred_gam,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",         # x-axis label
       y = "Water Temperature",   # y-axis label
       color = "Wetland Type"     # Legend title for site color
  )

install.packages("ggeffects")
