pacman::p_load(tidyverse,
               forecast,
               lterdatasampler,
               daymetr,
               glarma)
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_ts_anormaly.csv"
(df_ts <- read_csv(url))

# Plot time-series anomalies
df_ts %>% 
  ggplot(aes(x = year,          
             y = anormaly)) +  
  geom_line() +                  
  geom_point() +                 
  theme_bw() +                   
  labs(
    x = "Year",                     
    y = "Anomaly"                   
  )
# Simple linear model
m_lm <- lm(anormaly ~ year, data = df_ts)

#  summary
summary(m_lm)

#Visualization
df_ts %>% 
  ggplot(aes(x = year, 
             y = anormaly)) +  
  geom_line(linetype = "dotted") +     
  geom_point(alpha = 0.25) +           
  geom_abline(intercept = coef(m_lm)[1],  
              slope = coef(m_lm)[2]) +
  theme_bw()  

y <- NULL
y[1] <- 0
for (i in 1:99){
y[i+1] <- y[i] +rnorm(1, mean = 0, sd = 1)
}

tibble(y = y,
       x = 1:length(y)) %>%
  ggplot(aes(x=x,
             y=y))+
  geom_point()+geom_line()

#Lake heuron data
df_huron <- tibble(
  year = time(LakeHuron),                # Extracts the time component (years) from the LakeHuron ts object
  water_level = as.numeric(LakeHuron)    # Converts LakeHuron values to numeric (from ts class)
) %>% 
  arrange(year)                           # Ensures the data is ordered by year

# Plot Lake Huron time series with a linear trend
df_huron %>% 
  ggplot(aes(x = year, y = water_level)) +
  geom_point(alpha = 0.25) +       # Semi-transparent points
  geom_line(linetype = "dotted") + # Dotted line connecting points
  geom_smooth(method = "lm",       # Linear trend line
              color = "black",
              linewidth = 0.5) +
  theme_bw() +
  labs(x = "Year", y = "Water Level")
##AUTOREGRESSIVE MODEL
(m_ar1 <- Arima(
  df_huron$water_level,       
  order = c(1, 0, 0)          
))

#FITTED VALUES
df_huron_ar1 <- df_huron %>% 
  mutate(fit = fitted(m_ar1) %>%   
           as.numeric()) 

df_huron_ar1 %>% 
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +        
  
  geom_line(aes(x = year, 
                y = fit),           
            
            color = "steelblue") +
  theme_bw() 

m_ma1 <- Arima(
  df_huron$water_level,       # The time series data we want to model
  order = c(0, 0, 1)          # ARIMA model orders: c(p, d, q)
)

##ARMA
m_arma <- Arima(
  df_huron$water_level,       # The time series data we want to model
  order = c(1, 0, 1)          # ARIMA model orders: c(p, d, q)
)


##ARIMA MODEL
(m_arima <- Arima(df_huron$water_level,
                  order = c(1, 1, 0)))

##Model selection

auto.arima(
  df_huron$water_level, # data
  stepwise = FALSE,  
  ic = "aic" 
)  

#ARIMAX MODEL
data("ntl_icecover") 
ntl_icecover
df_ice <- ntl_icecover %>% 
  as_tibble() %>% 
  filter(between(year, 1980, 2014), 
         lakeid == "Lake Mendota") %>% 
  arrange(year)

list_mendota <- download_daymet(
  site = "Lake_Mendota",   # Arbitrary name you assign to this site
  lat = 43.1,              # Latitude of the lake
  lon = -89.4,             # Longitude of the lake
  start = 1980,            # Start year
  end = 2024,              # End year
  internal = TRUE          # Return the data as an R object rather than saving to disk
)

df_temp <- list_mendota$data %>% 
as_tibble() %>%                  
  janitor::clean_names() %>%       
  mutate(
    
    date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
    
    month = month(date)
  ) %>% 
  arrange(year, yday) %>% 
  group_by(year) %>% 
  summarize(temp_min = round(mean(tmin_deg_c), 2))

df_ice <- df_ice %>% 
  left_join(df_temp, by = "year")

#Do
obj_arima <-auto.arima(
  df_ice$ice_duration, 
  xreg = df_ice$temp_min, 
  stepwise = FALSE 
)
confint(obj_arima, level = 0.95)

df_ice %>%
  ggplot(aes(x=temp_min,
             y=ice_duration))+geom_point()


##Lab

library(lterdatasampler)
knz_bison
# 1. Explore the structure of the knz_bison dataset.
#    - Inspect variable types and missing values.
#    - Reformat variables as needed for analysis.

names(knz_bison)

knz_bison %>% 
  summarize(across(everything(), ~ sum(is.na(.)))) %>% 
  t() 

summary(knz_bison)


knz_bison_clean <- knz_bison %>% 
  mutate(
    animal_sex = factor(animal_sex),
    rec_year   = as.integer(rec_year)   
  )

str(knz_bison_clean)

#weight
knz_bison_clean %>% 
  ggplot(aes(x = animal_weight)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  theme_bw() +
  labs(title = "Distribution of Bison Body Mass",
       x = "Weight (kg)", y = "Count")

#weight over time
knz_bison_clean %>% 
  ggplot(aes(x = rec_year, y = animal_weight)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  labs(title = "Long-term Trend in Bison Body Mass",
       x = "Year", y = "Weight (kg)")
# 2. Subset the data to include observations from 1994–2012.

knz_bison_94_12 <- knz_bison %>%
  filter(rec_year >= 1994 & rec_year <= 2012)
nrow(knz_bison_94_12)

knz_bison_94_12 %>%
  count(rec_year)

# 3. Calculate the average body mass for female and male bison
#    for each year in the selected time period.


bison_mean_mass <- knz_bison_94_12 %>%
  group_by(rec_year, animal_sex) %>%       
  summarize(
    mean_mass = mean(animal_weight, na.rm = TRUE),
    n = n(),                               
    .groups = "drop"
  )
bison_mean_mass

# 4. Obtain climate data from the daymetr dataset.
#    - Identify relevant climate variables (e.g., temperature,
#      precipitation).
#    - Associate climate data with knz_bison by year.
#    - Coordinates: Lat 39.09300	Lon -96.57500

library(daymetr)
library(lterdatasampler)


konza_climate <- download_daymet(
  site     = "Konza_Prairie",
  lat      = 39.09300,
  lon      = -96.57500,
  start    = 1994,
  end      = 2012,
  internal = TRUE
)

#Convert tibble
df_climate <- konza_climate$data %>%
  as_tibble() %>%
  janitor::clean_names()



df_climate <- konza_climate$data %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
    year = year(date)
  )

names(df_climate)



df_climate_yearly <- df_climate %>%
  group_by(year) %>%
  summarize(
    mean_tmin = mean(tmin_deg_c, na.rm = TRUE),
    mean_tmax = mean(tmax_deg_c, na.rm = TRUE),
    sum_prcp  = sum(prcp_mm_day, na.rm = TRUE),  
    .groups = "drop"
  )
df_climate_yearly


bison_climate <- knz_bison_94_12 %>%
  left_join(df_climate_yearly, by = c("rec_year" = "year"))
bison_climate



# 5. Perform a time-series analysis to examine whether selected
#    climate variables influence annual bison body mass.
#    - Consider temporal autocorrelation and lag effects.
#    - Model males and females separately

# Annual mean mass by sex
annual_mass <- knz_bison_94_12 %>%
  group_by(rec_year, animal_sex) %>%
  summarize(
    mean_mass = mean(animal_weight, na.rm = TRUE),
    .groups = "drop"
  )

male_data <- bison_climate %>% filter(animal_sex == "male")
female_data <- bison_climate %>% filter(animal_sex == "female")

#time series
knz_bison_94_12 <- knz_bison %>%
  filter(rec_year >= 1994, rec_year <= 2012)
annual_mass <- knz_bison_94_12 %>%
  group_by(rec_year, animal_sex) %>%
  summarize(
    mean_mass = mean(animal_weight, na.rm = TRUE),
    .groups = "drop"
  )

table(annual_mass$animal_sex)
bison_climate <- annual_mass %>%
  left_join(df_climate_yearly, by = c("rec_year" = "year"))
bison_climate
bison_climate <- bison_climate %>% drop_na(mean_tmax, mean_tmin, sum_prcp)
male_data   <- bison_climate %>% filter(animal_sex == "M")
female_data <- bison_climate %>% filter(animal_sex == "F")

nrow(male_data)
nrow(female_data)

male_ts <- ts(
  male_data$mean_mass,
  start = min(male_data$rec_year),
  end   = max(male_data$rec_year),
  frequency = 1
)

female_ts <- ts(
  female_data$mean_mass,
  start = min(female_data$rec_year),
  end   = max(female_data$rec_year),
  frequency = 1
)

male_climate <- cbind(
  tmax = male_data$mean_tmax,
  tmin = male_data$mean_tmin,
  prcp = male_data$sum_prcp
)

female_climate <- cbind(
  tmax = female_data$mean_tmax,
  tmin = female_data$mean_tmin,
  prcp = female_data$sum_prcp
)

acf(male_ts, main = "Male bison: ACF of body mass")
pacf(male_ts, main = "Male bison: PACF of body mass")

acf(female_ts, main = "Female bison: ACF of body mass")
pacf(female_ts, main = "Female bison: PACF of body mass")

#Male arimax
male_arimax <- auto.arima(
  male_ts,
  xreg = male_climate,
  stepwise = FALSE,
  approximation = FALSE
)

summary(male_arimax)

#Female arimax
female_arimax <- auto.arima(
  female_ts,
  xreg = female_climate,
  stepwise = FALSE,
  approximation = FALSE
)

summary(female_arimax)

#lag effects
male_data <- male_data %>%
  mutate(
    lag_tmax = dplyr::lag(mean_tmax, 1),
    lag_tmin = dplyr::lag(mean_tmin, 1),
    lag_prcp = dplyr::lag(sum_prcp, 1)
  )

female_data <- female_data %>%
  mutate(
    lag_tmax = dplyr::lag(mean_tmax, 1),
    lag_tmin = dplyr::lag(mean_tmin, 1),
    lag_prcp = dplyr::lag(sum_prcp, 1)
  )

#Fit lag effects
male_ts_lag <- ts(male_data$mean_mass[-1],
                  start = min(male_data$rec_year) + 1,
                  frequency = 1)

male_arimax_lag <- auto.arima(
  male_ts_lag,
  xreg = cbind(
    lag_tmax = male_data$lag_tmax[-1],
    lag_tmin = male_data$lag_tmin[-1],
    lag_prcp = male_data$lag_prcp[-1]
  ),
  stepwise = FALSE
)

summary(male_arimax_lag)

checkresiduals(male_arimax)
checkresiduals(female_arimax)


