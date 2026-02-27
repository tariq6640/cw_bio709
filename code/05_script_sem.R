pacman::p_load(tidyverse,
               GGally,
               vegan,
               lavaan,
               lavaanPlot)
# Specify the URL of the raw CSV file on GitHub
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_foodweb.csv"

# Read the CSV file into a tibble
(df_fw <- read_csv(url))

#visualization
df_fw %>% 
  select(-plot_id) %>%
  ggpairs(progress = FALSE
  ) +
  theme_bw()

#write a path diagram
m1 <- '
  # regression of herbivore biomass on plant variables
  mass_herbiv ~ mass_plant + cv_h_plant
  # regression of predator biomass on herbivore biomass
  mass_pred ~ mass_herbiv
'

(fit1 <- sem(model = m1,
             data = df_fw))
summary(fit1)
summary(fit1, standardize = TRUE)
lavaanPlot(model = fit1, coefs = TRUE, stand = TRUE)

m2 <- 'mass_herbiv ~ mass_plant + cv_h_plant
mass_pred ~ mass_herbiv + cv_h_plant'
(fit2 <- sem(model = m2,
             data = df_fw))

#model comparison
anova(fit1, fit2)

#structure equation model
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_herbivory.csv"

(df_herbv <- read_csv(url))

#visualize
df_herbv %>% 
  ggpairs(
    progress = FALSE,
    columns = c("soil_n",
                "sla",
                "cn_ratio",
                "per_lignin")
  ) +
  theme_bw()

m_sem <- '
# latent variable
  palatability =~ sla + cn_ratio + per_lignin
  
# regression
  palatability ~ soil_n
  herbivory ~ palatability
'

(fit_sem <- sem(m_sem,
                data = df_herbv))

summary(fit_sem, standardize = TRUE)

## Lab
library(piecewiseSEM)
data("keeley")
# ---- Setup (runs first) ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, GGally, lavaan, lavaanPlot)

# Define once at top so all chunks can use it
load_keeley <- function() {
  if (requireNamespace("piecewiseSEM", quietly = TRUE)) {
    piecewiseSEM::keeley |> tibble::as_tibble()
  } else {
    message("piecewiseSEM not installed; using CSV mirror with same columns.")
    readr::read_csv(
      "https://byrneslab.net/classes/lavaan_materials/Keeley_rawdata_select4.csv",
      show_col_types = FALSE
    ) |>
      mutate(
        elev = as.integer(elev),
        age  = as.integer(age),
        rich = as.integer(rich)
      )
  }
}



(df_keeley <- load_keeley())
vars_fig22 <- c("distance", "abiotic",
                "hetero", "age", "firesev", "cover", "rich")

df_keeley %>%
  select(all_of(vars_fig22)) %>%
  ggpairs(progress = FALSE) +
  theme_bw()

#2

m_fig22 <- '
  abiotic ~ distance
  hetero  ~ distance
  firesev ~ age
  cover   ~ firesev
  rich    ~ cover + abiotic + hetero
'

fit_fig22 <- sem(model = m_fig22,
                 data  = df_keeley,
                 estimator = "ML",
                 meanstructure = TRUE)


summary(fit_fig22,
        fit.measures = TRUE,
        standardized = TRUE)


lavaanPlot(model = fit_fig22,
           coefs = TRUE,
           stand = TRUE,
           covs  = FALSE,
           stars = c("regress","latent","covs"))

#3


m_alt <- '
  abiotic ~ distance
  hetero  ~ distance
  firesev ~ age
  cover   ~ firesev + hetero + age
  rich    ~ cover + abiotic + hetero + distance
'


(fit_alt <- sem(model = m_alt,
                data  = df_keeley,
                estimator = "ML",
                meanstructure = TRUE))

summary(fit_alt, fit.measures = TRUE, standardized = TRUE)


lavaanPlot(model = fit_alt,
           coefs = TRUE,
           stand = TRUE,
           covs  = FALSE,
           stars = c("regress","latent","covs"))

#4

vars_fig22 <- c("distance", "abiotic", "hetero", 
                "age", "firesev", "cover", "rich")
df_keeley %>% select(all_of(vars_fig22)) %>% 
  GGally::ggpairs(progress = FALSE) + theme_bw()

m_pub <- '
  abiotic ~ distance
  hetero  ~ distance
  firesev ~ age
  cover   ~ firesev
  rich    ~ cover + abiotic + hetero
'


fit_pub <- sem(model = m_pub,
               data  = df_keeley,
               estimator = "ML",
               meanstructure = TRUE)

#Alternative model
# Add: cover ~ hetero + age; rich ~ distance


m_alt <- '
  abiotic ~ distance
  hetero  ~ distance
  firesev ~ age
  cover   ~ firesev + hetero + age
  rich    ~ cover + abiotic + hetero + distance
'


fit_alt <- sem(model = m_alt,
               data  = df_keeley,
               estimator = "ML",
               meanstructure = TRUE)
print(anova(fit_pub, fit_alt))




