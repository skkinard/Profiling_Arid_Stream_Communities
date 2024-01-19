# Sean Kinard
# 3-30-2021
# Spring 17 Texas Coastal Prairie

# Fish diversity linear regressions & aggregate plot lm&RDA at end of script

# Load Packages
library(nlme)
library(multcomp)
library(vegan)
library(cluster)
library("car")
library("MuMIn")
library("readr")
library(xlsx)
library(pvclust)
library(tidyverse)
library(car)
library(MASS)

# Import Data Files
env <- read.csv("sp17_data_files\\sp17_site_x_env.csv", fileEncoding="UTF-8-BOM")
invert <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, canopy, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterinvert <- merge(env,invert, by = "STAID")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# :::::::::::::::::::::::::::::::::::::::::::::::::::::
# Multivariate Regression analysis
# Invertebrate
# PCA selected environmental variables
# scale environmental predictors
msterinvert[,2:8] <- scale(msterinvert[,2:8])

full.model.invertebrate <- lm(shannon~ AP + flash.index + LFPP + NH4. + log.cond + canopy + Rosgen.Index, data=msterinvert)
summary(full.model.invertebrate)
vif(full.model.invertebrate)

# Exhuastive multivariable regression
options(na.action = "na.fail")
dredge(full.model.invertebrate)
options(na.action = "na.omit")

# exported top ten model outputs and AICc to excel sheet

# 3 models with delta AICc < 2.
# 2nd-ranked model only contained random effects

im1 <- lm(shannon~ LFPP, data=msterinvert)
im3 <- lm(shannon ~ NH4. + LFPP, data = msterinvert)

model_name <- c("im1", "im3")
d <- list(summary(im1), summary(im3))


# 7/10 top AICc models contained one or fewer environmental predictors. Previous univariate regressions indicate no statistically significant correlations

# 1/3 of the top AICc ranked multivariation regression models has a p value less than 0.05 and has an adjusted r^2 of 0.519 (-LFPP + NH4)

# Positive predictors of invertebrate diversity include NH4 and canopy
# Negative predictors of invertebrate diversity include LFPP and AP

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

