#' DESCRIPTION:
#' Script for piecewise SEM

# in-class ----------------------------------------------------------------


# lab ---------------------------------------------------------------------

# =========================================
# Student Exercise: Piecewise SEM with Meadows Data
# =========================================
# Dataset: meadows (from piecewiseSEM package)
# Variables:
#   grazed - 0 = ungrazed, 1 = grazed
#   mass   - plant biomass (g/m²)
#   elev   - plot elevation above sea level
#   rich   - plant species richness per m²
#
# Instructions:
# 1. Explore the dataset (structure, summary, plots).
# 2. Develop a conceptual model: decide which variables influence others.
#    - Consider direct and indirect effects.
#    - Think about grazing as a disturbance factor.
# 3. Fit component models (e.g., lm) for each hypothesized relationship.
# 4. Combine models into a piecewise SEM using psem().
# 5. Evaluate the SEM: path coefficients, significance, variance explained.
# 6. Optional: try alternative models if your model deveiate from the expectation.
#
# Deliverables:
# - Code for component models and combined SEM
# - Conceptual SEM diagram
# - Short reasoning about your SEM results
