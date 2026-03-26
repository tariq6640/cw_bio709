library(tidyverse)

# Q1. Visualize seasonal patterns in emergence flux at both sites
#     (e.g., plot emergence vs. day of year, with separate lines or colors for each site).
#     [1 point]

link1 <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_insect_emergence.rds"
df_emg <- readRDS(url(link1, "rb"))

ggplot(df_emg, aes(x = t, y = emergence, color = site)) +
  geom_line(alpha = 0.7, linewidth = 1) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Seasonal Patterns of Aquatic Insect Emergence",
    x = "Day of Year",
    y = "Emergence flux (g/day)",
    color = "Site"
  ) +
  theme_classic(base_size = 14)

# Q2. Test whether emergence flux differs significantly between the two sites,
#     while appropriately accounting for seasonal variation
#     [4 points]

library(mgcv)

# General additive model (A simple t‑test is invalid 
#because emergence varies strongly across seasons)
#Model A: shared seasonal pattern
m1 <- gam(emergence ~ site + s(t, k = 30), data = df_emg)

summary(m1)
anova(m1)

#Model B: Each site has its own seasonal curve
m2 <- gam(emergence ~ site + s(t, by = site, k = 30), data = df_emg)

summary(m2)
anova(m2)

#Model comparison
AIC(m1, m2)
anova(m1, m2, test = "F")

#Dataset 2

link2 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/data_lake_invert.rds"
df_inv <- readRDS(url(link2, "rb"))

# Q1. Create a scatter plot of macrophyte production ('prod', y-axis)
#     versus water conductivity ('cond', x-axis), with points colored by lake identity.
#     [1 point]

ggplot(df_inv, aes(x = cond, y = prod, color = lake)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    x = "Conductivity",
    y = "Macrophyte Production",
    color = "Lake"
  ) +
  theme_classic(base_size = 14)

# Q2. Create a scatter plot of raw invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

ggplot(df_inv, aes(x = prod, y = hb, color = lake)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    x = "Macrophyte Production",
    y = "Invertebrate Biomass",
    color = "Lake"
  ) +
  theme_classic(base_size = 14)

# Q3. Create a scatter plot of "log-transformed" invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

ggplot(df_inv, aes(x = prod, y = log(hb), color = lake)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(x = "Macrophyte Production (g/month)",
    y = "log(Invertebrate Biomass)",
    color = "Lake"
  ) +
  theme_classic(base_size = 14)

# Q4. Test hypothesis (a) by modeling macrophyte production while
#     statistically controlling for potential confounding variables ('substrate', 'lake').
#     [3 points]

#For this I will use linear model as the prodcution look continous

library(lme4)
library(lmerTest)

m_a <- lmer(prod ~ cond + substrate + (1 | lake), data = df_inv)

summary(m_a)

#Explaination: Signifcant effect of Cond so it support hypothesis A while
#no signficant effect of substrate so doesnot support hypothesis a

# Q5. Test hypotheses (a–c) simultaneously using a unified modeling framework.
#     Based on the resulting statistical tests, determine whether the overarching
#     hypothesis (a–c, combined) is supported or rejected.
#     - Use appropriate probability distributions.
#     - Use variable transformation if appropriate given the data.
#     [4 points]

##I test (a)cond - prod, (b)prod - hb, (c) prod - s
##According to these pathways I will do piecewise SEM

install.packages("piecewiseSEM")
library(piecewiseSEM)

#Model (a): conductivity - production

m_prod <- lmer(prod ~ cond + substrate + (1 | lake), data = df_inv)

#Model (b): production → invertebrate biomass (log-transformed)

m_hb <- lmer(log(hb) ~ prod + substrate + (1 | lake), data = df_inv)

#Model (c): production → richness
library(glmmTMB)

m_s <- glmmTMB(s ~ prod + substrate + (1 | lake), 
               family = poisson(),
               data = df_inv)
#One SEM
sem_model <- psem(m_prod, m_hb,m_s)

summary(sem_model)

#Dataset 3

link3 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/nutrient.rds"
nutrient <- readRDS(url(link3, "rb"))

print(trees)

# Q1. Visualize relationships among tree diameter ('Girth'), height ('Height'),
#     and timber volume ('Volume') (e.g., using scatterplot matrix or pairwise scatter plots).
#     [1 point]



pairs(trees,
      main = "Pairwise Scatterplots of Tree Diameter, Height, and Timber Volume",
      pch = 19, col = "red")

#Optional (Just checking)

install.packages("GGally")
library(GGally)

ggpairs(trees) +
  theme_bw()

# Q2. Perform an appropriate ordination or dimension reduction method to 
#     summarize these three variables into fewer composite axes.
#     Then, identify and retain axes that explain meaningful variation in the original variables
#     [3 points]

trees_scaled <- scale(trees)

pca_res <- prcomp(trees_scaled, center = TRUE, scale. = TRUE)
summary(pca_res)
pca_res$rotation  

plot(pca_res, type = "l", main = "Scree Plot")

#Explaination: PC1 usually explains >90% of variation because Girth, 
#Height, and Volume are strongly positively correlated

biplot(pca_res, main = "PCA Biplot")

# Q3. If justified, test whether the retained axis (or axes) is significantly 
#     related to "nutrient"; 
#     skip regression if the ordination does not support meaningful interpretation.
#     [1 point]

trees$PC1 <- pca_res$x[, 1]
str(nutrient)
m_nutrient <- lm(PC1 ~ nutrient, data = trees)

summary(m_nutrient)

#Explaination: Nutrient levels influence tree size

#Dataset 4
df_nile <- dplyr::tibble(
  year = time(Nile), # observation year
  discharge = as.numeric(Nile) # discharge
)

df_sunspot <- dplyr::tibble(
  year = time(sunspot.year), # observation year
  sunspots = as.numeric(sunspot.year) # the number of sunspots
)

# Q1. Create a combined data frame aligning the observation years
#     (i.e., only include years present in both datasets)
#     [1 point]

library(dplyr)

df_combined <- inner_join(df_nile, df_sunspot, by = "year")

df_combined

# Q2. Test whether the number of sunspots is significantly related to Nile's discharge
#     [4 points]

#Simple regression approach
model_simple <- lm(discharge ~ sunspots, data = df_combined)
summary(model_simple)

#Corelation test approach
cor.test(df_combined$discharge, df_combined$sunspots)

#To aviod spurious coorelations

df_combined <- df_combined %>%
  mutate(
    d_discharge = c(NA, diff(discharge)),
    d_sunspots  = c(NA, diff(sunspots))
  ) %>%
  drop_na()

model_diff <- lm(d_discharge ~ d_sunspots, data = df_combined)
summary(model_diff)

#Hence there is no significant effect I want to try time time series plot and 
#ARIMA

#Time Series plots


ggplot(df_nile, aes(x = year, y = discharge)) +
  geom_line(color = "red", linewidth = 1) +
  labs(x = "Year",
       y = "Discharge") +
  theme_classic(base_size = 14)

# Plotting sunspot counts
ggplot(df_sunspot, aes(x = year, y = sunspots)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(x = "Year",
       y = "Sunspots") +
  theme_classic(base_size = 14)

#Lets check after scaling
df_scaled <- df_combined %>%
  mutate(
    discharge_z = scale(discharge)[,1],
    sunspots_z  = scale(sunspots)[,1]
  )

ggplot(df_scaled, aes(x = year)) +
  geom_line(aes(y = discharge_z, color = "Nile discharge"), linewidth = 1) +
  geom_line(aes(y = sunspots_z, color = "Sunspots"), linewidth = 1) +
  labs(y = "Standardized value (z-score)",
       x = "Year",
       color = "Variable") +
  scale_color_manual(values = c("Nile discharge" = "red",
                                "Sunspots" = "steelblue")) +
  theme_classic(base_size = 14)

#ARIMA MODEL

#Nile discharge
library(forecast)

ts_nile <- ts(df_nile$discharge, start = min(df_nile$year))

# Auto ARIMA
fit_nile <- auto.arima(ts_nile)
fit_nile
summary(fit_nile)
autoplot(fit_nile)

#SUnspot ARIMA
ts_sun <- ts(df_sunspot$sunspots, start = min(df_sunspot$year))

fit_sun <- auto.arima(ts_sun)
fit_sun
summary(fit_sun)
autoplot(fit_sun)

#ARIMA REgression

df_reg <- df_combined

ts_dis <- ts(df_reg$discharge, start = min(df_reg$year))
ts_sun <- df_reg$sunspots  

#Fitting
fit_arimax <- auto.arima(ts_dis, xreg = ts_sun)

fit_arimax
summary(fit_arimax)
checkresiduals(fit_arimax)
#Conclusion: Sunspots do nOt significantly influence nile river discharge.