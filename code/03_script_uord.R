#' DESCRIPTION:
#' Script for Unconstrained Ordination

# in-class ----------------------------------------------------------------

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: PCA using the iris dataset
# ============================================================

# In this exercise, you will perform a Principal Component
# Analysis (PCA) using all morphological measurements in the
# iris dataset and visualize multivariate trait patterns
# among species.

# 1. Using all four morphological variables
#    (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#    perform a PCA.

# 2. Visualize flower morphology in PC axes whose cumulative
#    contribution exceeds 90%; color points by species.

# 3. Which morphological traits contribute most strongly to
#    the first and second principal components? How?

# ============================================================
# EXERCISE: NMDS using the BCI dataset
# ============================================================

# In this exercise, you will perform a Non-metric Multidimensional
# Scaling (NMDS) using the BCI tree community dataset and explore
# patterns in species composition among sites.

data("BCI")
data("BCI.env")

# 1. Using the BCI dataset, calculate a dissimilarity matrix
#    (e.g., Bray-Curtis) and perform NMDS.

# 2. Visualize sites in NMDS space.
#    - How are sites positioned relative to each other?
#    - Color or shape points by environmental groups or site
#      characteristics of your choice.

# 3. Perform PERMANOVA to examine if communities are grouped
#    by the environmental variable you selected.
