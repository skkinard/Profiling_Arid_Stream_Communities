# 02_E_PCA
# Sean Kinard
# 03-30-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by importing a dataset (sp17_site_x_env.csv) containing environmental data. It selects seven a-priori environmental predictors for evaluation with community diversity metrics and community composition ordinations. These predictors include annual precipitation (AP), flood disturbance regime (flash.index), drought disturbance regime (LFPP), biogenic pollutants (NH4.), osmotic stressors (log.cond), and stream morphology (Rosgen.Index). Next, the script conducts Principal Component Analysis (PCA) on the selected environmental predictors using the prcomp() function. It generates a summary of the PCA results, including the importance of each principal component, and creates a diagnostic scree plot and a PCA plot using the ggbiplot() function. The script then generates correlation tables between the environmental predictors and the first two principal components. These correlation tables are exported to Excel files using the write_xlsx() function.

# In summary, the script performs PCA on selected environmental predictors, visualizes the PCA results, calculates correlations between predictors and principal components, and exports the results for further analysis and reporting.

# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# Load and Install Packages
library(ggplot2)
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
library(tidyverse)

# Import Data File
emat <- read.csv("sp17_data_files\\sp17_site_x_env.csv", fileEncoding="UTF-8-BOM")

# Selecting a-priori environmental predictors
# 7 variables to evaluate with community diversity metrics and community composition ordinations
# Precipitation: Annual precipitation (30-year average)
# Flood disturbance regime: flash index
# Drought disturbance regime: Low Flow Pulse Percent  
# Biogenic Pollutants: NH4+
# Osmotic stressors: Conductivity
# Canopy effects: Canopy Coverage
# Stream morphology: Rosgen index

(emat_ap <- select(emat, Site.Name, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

# -----------------------------------------------------------------------------
# PCA
# -----------------------------------------------------------------------------
mypr <- prcomp(emat_ap[,-1], scale = TRUE)
summary(mypr)
mypr_importance <- as.data.frame(summary(mypr)$importance)
mypr_importance$metric <- rownames(mypr_importance)
(mypr_importance <- mypr_importance[,c(8,1:7)])

# diagnostic scree plot
plot(mypr, type = "l")

# PCA plot
ggbiplot(mypr, obs.scale=1, var.scale=1,
         groups = emat_ap$AP) +
  scale_colour_viridis_c(direction=-1) +
  theme_classic() +
 theme(text = element_text(size = 18)) +
 theme(axis.text = element_text(size = 18))

# generating tables
m_env <- cbind(emat_ap[,-1], mypr$x[,1:2])
# correlations with PCA1
Pcor <- as.data.frame(cor(m_env[,1:6],m_env[,7:8]))
Pcor$variable <- rownames(Pcor)
(Pcor <- Pcor[,c(3,1,2)])

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------
# library(writexl)
# write_xlsx(as.data.frame(mypr_importance),"sp17_r_scripts\\ap_PCA_importance.xlsx")
# write_xlsx(as.data.frame(Pcor),"sp17_r_scripts\\ap_PCA_correlations.xlsx")

# -----------------------------------------------------------------------------
# End 02_E_PCA





