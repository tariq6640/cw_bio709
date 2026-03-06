pacman::p_load(tidyverse,
               GGally,
               piecewiseSEM)
library(MASS)  
data("keeley")

(df_keeley <- keeley %>% 
    as_tibble())

#PSEM but all normally distributed
#DEFINE MODELS

m1 <- lm(abiotic ~ distance, data = df_keeley)
m2 <- lm(hetero ~ distance, data = df_keeley)
m3 <- lm(firesev ~ age, data = df_keeley)
m4 <- lm(cover ~ firesev, data = df_keeley)
m5 <- lm(rich ~ cover + abiotic + hetero, data = df_keeley)

#Piecewise SEM
sem_model <- psem(m1, m2, m3, m4, m5)

# Evaluate
summary(sem_model, .progressBar = FALSE)

#deifne individuals models 

m1 <- lm(abiotic ~ distance, data = df_keeley)      
m2 <- lm(hetero ~ distance, data = df_keeley)       
m3 <- lm(firesev ~ age, data = df_keeley)           

# m4 now includes a direct effect of hetero on cover (added path)
m4 <- lm(cover ~ firesev + hetero, data = df_keeley)  
m5 <- MASS::glm.nb(rich ~ cover + abiotic + hetero + distance, 
                   data = df_keeley) 
sem_model <- psem(m1, m2, m3, m4, m5)
# Evaluate model
summary(sem_model, .progressBar = FALSE)
plot(sem_model)
data("shipley")

df_shipley <- shipley %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  drop_na(growth)

df_shipley %>% 
  group_by(site) %>% 
  summarize(n_tree = n_distinct(tree))

# visualization
df_shipley %>% 
  ggpairs(
    columns = c("dd", 
                "date",
                "growth",
                "live"),  
    progress = FALSE      
  ) +
  theme_bw()    
library(glmmTMB)
# Model 1: date depends on dd, with random intercepts for site and tree
m1 <- glmmTMB(date ~ dd + (1 | site) + (1 | tree), 
              data = df_shipley,
              family = "gaussian")

# Model 2: growth depends on date, same random effects
m2 <- glmmTMB(growth ~ date + (1 | site) + (1 | tree), 
              data = df_shipley,
              family = "gaussian")

# Model 3: live (binary) depends on growth, logistic mixed model
m3 <- glmmTMB(live ~ growth + (1 | site) + (1 | tree), 
              data = df_shipley, 
              family = "binomial")

# Combine models into a piecewise SEM
sem_glmm <- psem(m1, m2, m3)

summary(sem_glmm, .progressBar = FALSE)


# lab ---------------------------------------------------------------------

library(piecewiseSEM)
data("meadows")

# =========================================
# EXERCISE: Piecewise SEM with Meadows Data
# =========================================
#
# ------------------------------------------------------------
# Dataset: meadows (from piecewiseSEM package)
# Variables:
#   grazed - 0 = ungrazed, 1 = grazed
#   mass   - plant biomass (g/m²)
#   elev   - plot elevation above sea level
#   rich   - plant species richness per m²
# ------------------------------------------------------------
#
# 1. Explore the dataset (structure, summary, plots).


data("meadows")

df_meadows <- as_tibble(meadows)
df_meadows
glimpse(df_meadows)
summary(df_meadows)
library(GGally)

df_meadows %>% 
  ggpairs(columns = c("grazed", "mass", "elev", "rich"))

df_meadows %>%
  ggplot(aes(x = factor(grazed), y = rich)) +
  geom_boxplot() +
  labs(x = "Grazing treatment", y = "Species richness") +
  theme_bw()


df_meadows %>% 
  ggplot(aes(x = mass)) +
  geom_histogram(bins = 20) +
  theme_bw()

#Relationship
df_meadows %>%
  ggplot(aes(elev, rich, color = factor(grazed))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# 2. Develop a conceptual model: decide which variables influence others.
#    - Consider direct and indirect effects.
#    - Think about grazing as a disturbance factor.

# Upstream mediator
m1_mass <- lm(mass ~ grazed + elev, data = df_meadows)

# Try Poisson for the richness node
m2_rich_pois <- glm(rich ~ mass + elev + grazed, data = df_meadows, family = poisson())


m2_rich_nb <- MASS::glm.nb(rich ~ mass + elev + grazed, data = df_meadows)
richness_model <- m2_rich_nb
sem_family <- "Negative Binomial (glm.nb)"

richness_model <- m2_rich_pois
sem_family <- "Poisson"


# 3. Fit component models for each hypothesized relationship.
sem_obj <- psem(
  m1_mass,        
  richness_model  
)

# 4. Combine models into a piecewise SEM using psem().

# Full SEM summary
s <- summary(sem_obj, .progressBar = FALSE)


# Path table with standardized effects (robust to minor name differences)
coef_tab_raw <- piecewiseSEM::coefs(sem_obj, standardize = "scale")
std_col <- dplyr::case_when(
  "Std.Est" %in% names(coef_tab_raw) ~ "Std.Est",
  "Std.Estimate" %in% names(coef_tab_raw) ~ "Std.Estimate",
  TRUE ~ NA_character_
)
keep <- c("Response","Predictor","Estimate","SE","Crit.Value","P.Value", std_col)
keep <- keep[!is.na(keep) & keep %in% names(coef_tab_raw)]
coef_tab <- dplyr::select(coef_tab_raw, dplyr::all_of(keep))
print(coef_tab, row.names = FALSE)

# Variance explained per component 
s$R2

# Global fit
s$Cstat

# Upstream model reused
m1_mass <- lm(mass ~ grazed + elev, data = df_meadows)

# Model A: with direct grazing effect
m2_with    <- MASS::glm.nb(rich ~ mass + elev + grazed, data = df_meadows)
sem_with   <- psem(m1_mass, m2_with)
s_with     <- summary(sem_with, .progressBar = FALSE)

# Model B: without direct grazing effect
m2_without <- MASS::glm.nb(rich ~ mass + elev, data = df_meadows)
sem_without <- psem(m1_mass, m2_without)
s_without   <- summary(sem_without, .progressBar = FALSE)

# Compare global fit and IC
s_with$Cstat;    s_with$IC
s_without$Cstat; s_without$IC