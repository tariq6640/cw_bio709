pacman::p_load(tidyverse,
               GGally,
               vegan)
# iris dataset 
df_iris <- iris %>% 
  as_tibble() %>%               
  janitor::clean_names()  

df_iris %>%
  ggpairs(
    progress = FALSE,
    columns = c("sepal_length", 
                "sepal_width",
                "petal_length",
                "petal_width"),
    aes(
      color = species, 
      alpha = 0.5 
    )
  ) +
  theme_bw() 
# Extract only the petal measurements from the iris dataset
df_petal <- df_iris %>% 
  
  select(starts_with("petal_")) 

# Perform Principal Component Analysis (PCA) on the selected petal data
obj_pca <- prcomp(
  x = df_petal,    
  center = TRUE,   
  scale = TRUE     
)


print(obj_pca)
summary(obj_pca)
# Combine the original iris dataset with PCA scores
df_pca <- bind_cols(
  df_iris,              
  as_tibble(obj_pca$x)) 
df_pca %>% 
  ggplot(aes(
    x = species,   
    y = PC1,       
    color = species 
  )) +
  geom_boxplot() +
  labs(x = "Species",
       y = "PC1")

# Exercise ----------------------------------------------------------------


# 1. Using all four morphological variables
#    (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#    perform a PCA.


# Load data
data(iris)

# Keep only numeric columns
X <- iris[, 1:4]  

# Run PCA on standardized variables
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# Variance explained
summary(pca)

# Base R scree plot
plot(pca, type = "l", main = "Scree Plot: Variance Explained")

var_expl <- pca$sdev^2
prop_var <- var_expl / sum(var_expl)
round(prop_var, 3)

library(ggplot2)

scores <- as.data.frame(pca$x)
scores$Species <- iris$Species

ggplot(scores, aes(PC1, PC2, color = Species)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(type = "norm", linetype = 2) +
  labs(title = "Iris PCA: PC1 vs PC2",
       x = paste0("PC1 (", round(100*prop_var[1],1), "%)"),
       y = paste0("PC2 (", round(100*prop_var[2],1), "%)")) +
  theme_minimal()

loadings <- as.data.frame(pca$rotation)
round(loadings, 3)

## call sample data from vegan package
data(dune)
## the first 3 species
head(dune[, 1:3])
# Take the 'dune' dataset from vegan
dune %>% 
  as_tibble() %>%         
  select(1:3) %>%         
  ggpairs() +             
  theme_bw()   

#calculate he distance between units
m_bray <- vegdist(dune,       
                  method = "bray")
m_bray
obj_nmds <- metaMDS(
  m_bray,   
  k = 2)

#metaMDS(comm = dune, distance = "bray",
#k = 2)

obj_nmds

#vusualize nmds
data(dune.env)
head(dune.env)
df_nmds <- dune.env %>%           
  as_tibble() %>%                  
  bind_cols(obj_nmds$points) %>%   
  janitor::clean_names()

df_nmds %>% 
  ggplot(aes(
    x = mds1,           
    y = mds2,          
    color = use         
  )) + geom_point(size = 3) + stat_ellipse(level = 0.95,linetype = 2) + 
  theme_bw() +  labs(color = "Land-use intensity",x = "NMDS1",                   
                     y = "NMDS2") 

adonis2(
  m_bray ~ use,   
  data = df_nmds  
)

#Excercise

data("BCI", "BCI.env")
BCI %>% 
  as_tibble() %>%         
  select(1:3) %>%         
  ggpairs() +             
  theme_bw()   

#calculate he distance between units
m_bray <- vegdist(BCI,       
                  method = "bray")
m_bray
obj_nmds <- metaMDS(
  m_bray,   
  k = 2)


obj_nmds

#vusualize nmds
data(BCI.env)
head(BCI.env)
df_nmds <- BCI.env %>%           
  as_tibble() %>%                  
  bind_cols(obj_nmds$points) %>%   
  janitor::clean_names()
head(BCI.env)
df_nmds %>% 
  ggplot(aes(
    x = mds1,           
    y = mds2,          
    color = habitat         
  )) + geom_point(size = 3)

adonis2(
  m_bray ~ Habitat,   
  data = BCI.env 
)

