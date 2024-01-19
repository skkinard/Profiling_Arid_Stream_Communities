# Sean Kinard
# 03-30-2021
# Spring 17 Texas Coastal Prairie

# PCA to identify patterns in variation among environmental variables

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - 

# Load and Install Packages
library(ggplot2)
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
library(tidyverse)

# Import Data File
emat <- read.csv("sp17_data_files\\sp17_site_x_env.csv", fileEncoding="UTF-8-BOM")

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -

# Selecting a-priori environmental predictors
# 7 variables to evaluate with community diversity metrics and community composition ordinations
# Precipitation: Annual precipitation (30-year average)
# Flood disturbance regime: flash index
# Drought disturbance regime: Low Flow Pulse Percent  
# Biogenic Pollutants: NH4+
# Osmotic stressors: Conductivity
# Canopy effects: Canopy Coverage
# Stream morphology: Rosgen index

(emat_ap <- select(emat, Site.Name, AP, flash.index, LFPP, NH4., log.cond, canopy, Rosgen.Index))

# PCA and plots
mypr <- prcomp(emat_ap[,-1], scale = TRUE)
summary(mypr)
mypr_importance <- as.data.frame(summary(mypr)$importance)
mypr_importance$metric <- rownames(mypr_importance)
(mypr_importance <- mypr_importance[,c(8,1:7)])

# diagnostic scree plot
plot(mypr, type = "l")

# PCA plot
ggbiplot(mypr, obs.scale=1, var.scale=1,
         groups = emat_ap$AP,
         labels = emat_ap$Site.Name) +
  scale_colour_viridis_c(direction=-1) +
  theme_classic() +
 theme(text = element_text(size = 16)) +
 theme(axis.text = element_text(size = 16))

# generating tables
m_env <- cbind(emat_ap[,-1], mypr$x[,1:2])
# correlations with PCA1
Pcor <- as.data.frame(cor(m_env[,1:7],m_env[,8:9]))
Pcor$variable <- rownames(Pcor)
(Pcor <- Pcor[,c(3,1,2)])

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -

# export tables to excel

# library(writexl)
# write_xlsx(as.data.frame(mypr_importance),"sp17_r_scripts\\ap_PCA_importance.xlsx")
# write_xlsx(as.data.frame(Pcor),"sp17_r_scripts\\ap_PCA_correlations.xlsx")

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -





