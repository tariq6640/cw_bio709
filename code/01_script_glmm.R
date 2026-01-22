
# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               lme4,
               glmmTMB)
##call sample data
data(Owls)
df_owl_raw <- as_tibble(Owls)
df_owl <- df_owl_raw %>%
  janitor::clean_names() %>%
  mutate(across(.cols = where(is.factor),
                .fns = str_to_lower))
df_owl
 
##visulaization

df_owl %>%
  ggplot(aes(x = food_treatment,
             y = neg_per_chick)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = 0.25) + 
  theme_bw()
##ignoring nest effects
MASS::glm.nb(sibling_negotiation ~ food_treatment + offset(log(brood_size)),
             data = df_owl)

##add nest effects as fixed effects
MASS::glm.nb(sibling_negotiation ~ food_treatment + nest + offset(log(brood_size)),
             data = df_owl)
v_g9 <- unique(df_owl$nest)[1:9]
df_owl %>%
  filter(nest %in% v_g9) %>% 
  ggplot(aes(x = food_treatment,
             y = neg_per_chick)) +
  geom_jitter(alpha = 0.25, 
              width = 0.1) +
  facet_wrap(facets =~ nest,
             ncol = 3,
             nrow = 3) +
  theme_bw() 

## use glmmTMB to include random effects
##regular glm - y~x
##with random effect - y ~x (1|group)
m_ri <-glmmTMB(
  sibling_negotiation ~food_treatment + offset(log(brood_size)) + 
    (1 | nest) +      
    offset(log(brood_size)),
  data = df_owl,
  family = nbinom2())
m_ri

## get random intercept values
head(coef(m_ri)$cond$nest)
# ------------------------------------------------------------
# 1. Global (population-level) intercept on the response scale
# ------------------------------------------------------------
# The model uses a log link (negative binomial),
# so the intercept is on the log scale.
# We exponentiate it to return to the original response scale.
g0 <- exp(fixef(m_ri)$cond[1])


# ------------------------------------------------------------
# 2. Select a subset of nests to visualize
# ------------------------------------------------------------
# Plotting all 27 nests would be visually overwhelming,
# so we randomly select 9 nests for this example.
set.seed(123)  # ensures reproducibility
v_g9_ran <- sample(unique(df_owl$nest),
                   size = 9)


# ------------------------------------------------------------
# 3. Extract nest-specific coefficients (random intercept model)
# ------------------------------------------------------------
# coef(m_ri) returns the sum of fixed + random effects for each nest.
# These values are still on the log scale.
df_g9 <- coef(m_ri)$cond$nest %>% 
  as_tibble(rownames = "nest") %>%      # convert to tibble and keep nest ID
  filter(nest %in% v_g9_ran) %>%        # keep only the selected nests
  rename(
    log_g = `(Intercept)`,              # nest-specific intercept (log scale)
    b = food_treatmentsatiated          # fixed slope for food treatment
  ) %>% 
  mutate(
    g = exp(log_g),                     # intercept on response scale
    s = exp(log_g + b)                  # predicted value under satiated treatment
  )


# ------------------------------------------------------------
# 4. Create the figure
# ------------------------------------------------------------
df_owl %>% 
  filter(nest %in% v_g9_ran) %>%        # plot only the selected nests
  ggplot(aes(x = food_treatment,
             y = neg_per_chick)) +
  # Raw data points (jittered to reduce overlap)
  geom_jitter(width = 0.1,
              alpha = 0.5) +
  # Dashed horizontal lines:
  # nest-specific intercepts (baseline differences among nests)
  geom_hline(data = df_g9,
             aes(yintercept = g),
             alpha = 0.5,
             linetype = "dashed") +
  # Solid line segments:
  # predicted change from unfed to satiated treatment
  # using a common (fixed) slope across nests
  geom_segment(data = df_g9,
               aes(y = g,
                   yend = s,
                   x = 1,
                   xend = 2),
               linewidth = 0.5,
               linetype = "solid") +
  # Solid blue horizontal line:
  # global (population-level) intercept
  geom_hline(yintercept = g0,
             alpha = 0.5,
             linewidth = 1,
             linetype = "solid",
             color = "steelblue") +
  # Facet by nest to emphasize group-level structure
  facet_wrap(facets =~ nest,
             nrow = 3,
             ncol = 3) +
  theme_bw()


# Excercise ---------------------------------------------------------------

# Q1 – Visualization:
#
# Goal:
#   Examine the relationship between parasite load (ticks) at the brood level and height above sea level.
#
# Key considerations:
# - Calculate average tick counts for each brood
# - Plot mean ticks vs. height
# - Color points by sampling year

# Packages
library(ggplot2)


# Load the built-in sleep dataset
data(sleep)

# Convert variables to factors (important for plotting)
sleep <- sleep %>%
  mutate(
    ID = factor(ID),
    group = factor(group, levels = c("1", "2"),
                   labels = c("Drug 1", "Drug 2"))
  )

# Paired visualization with facets
ggplot(sleep, aes(x = group, y = extra, group = ID, color = ID)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  facet_wrap(~ ID, ncol = 5) +
  labs(
    title = "Increase in Sleep Duration After Drug Administration",
    subtitle = "Paired responses for each subject",
    x = "Drug Group",
    y = "Increase in Sleep (hours)",
    color = "Subject ID"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0)
  )

# Q2 – Model development:
#
# Goal:
#   Develop a model to examine the relationship between parasite load (ticks) at the brood level and height above sea level.
#
# Key considerations:
#   - Response variable (TICKS) is count
#   - HEIGHT represents the variable of interest
#   - BROOD represents a grouping factor of repeated measurements
#   - YEAR represents another grouping factor of repeated measurements










