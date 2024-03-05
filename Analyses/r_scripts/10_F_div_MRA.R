# 10_F_div_MRA
# Sean Kinard
# 07-19-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by loading two datasets: one containing fish diversity estimates and another containing environmental data. It then proceeds to clean up the data frames to include only the a-priori selected variables and merges them to ensure matching rows (sites).
# 
# Following data preparation, the script conducts multivariate regression analysis to explore the relationship between fish diversity measures and environmental variables. It fits a full model regression with species richness (shannon) as the dependent variable and several environmental predictors (AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index). The summary of the full model regression is provided, including estimates, standard errors, t-values, and p-values for each predictor. Additionally, the script calculates the Variance Inflation Factor (VIF) to check for multicollinearity among predictors.
# 
# The script then performs an exhaustive multivariable regression analysis using the dredge function and selects models with a delta AICc less than 2. It identifies multiple regression models with low AICc values, indicating that precipitation (AP) is a positive predictor while low flow pulse percent (LFPP) is a negative predictor of fish diversity.
# 
# Finally, the script exports the results of the multivariate regression analysis into a CSV file for further examination and reporting.
# 
# In summary, the script employs multivariate regression techniques to investigate the relationship between fish diversity and environmental variables, providing insights into the predictors of fish diversity in the studied sites.

# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

# Fish multiple regressions with environmental predictors

# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")
library(nlme)
library(multcomp)
library("car")
library("MuMIn")
library("readr")
library(car)
library(MASS)

fish <- read_csv("sp17_data_files/fish_diversity_estimates.csv")

# Load environmental data 
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
# Merge dataframes to ensure matching rows (sites) 
msterfish <- merge(env,fish, by = "STAID")

# -----------------------------------------------------------------------------
# Multivariate Regression analysis
# Fish
# a priori selected environmental variables
# scale environmental predictors
msterfish[,3:27] <- scale(msterfish[,3:27])

# Species Richness
full.model.fish <- lm(shannon ~ AP + flash.index + LFPP + NH4. + log.cond + Rosgen.Index , data=msterfish)
summary(full.model.fish)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)    3.8263     0.3409  11.224  0.00152 **
#   AP             1.7330     1.1625   1.491  0.23281   
# flash.index    0.3584     0.5195   0.690  0.53986   
# LFPP          -0.9359     0.6642  -1.409  0.25360   
# NH4.           0.5434     0.9012   0.603  0.58903   
# log.cond       0.6678     1.0309   0.648  0.56328   
# Rosgen.Index   0.5138     0.3857   1.332  0.27500   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.078 on 3 degrees of freedom
# Multiple R-squared:  0.8239,	Adjusted R-squared:  0.4717 
# F-statistic:  2.34 on 6 and 3 DF,  p-value: 0.2593

vif(full.model.fish) # VIF is okay, but warrants some concern regarding Precipitation (10.4) along with conductivity (8.2)

# Exhuastive multivariable regression
options(na.action = "na.fail")
dredge_fish <- dredge(full.model.fish)
options(na.action = "na.omit")
dredge_fish <- as.data.frame(dredge_fish[1:10,])

# delta < 2 multivariate regression models:
dredge_fish_rich[c(which(dredge_fish_rich$delta < 2)),]

fm1 <- lm(shannon ~ AP , data=msterfish)
summary(fm1)
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.8263     0.3238  11.817 2.41e-06 ***
#  AP            1.1260     0.3413   3.299   0.0109 *  
#
# Residual standard error: 1.024 on 8 degrees of freedom
# Multiple R-squared:  0.5764,	Adjusted R-squared:  0.5234 
# F-statistic: 10.88 on 1 and 8 DF,  p-value: 0.01087

fm2 <- lm(shannon ~ AP + LFPP, data = msterfish)
summary(fm2)
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.8263     0.2904  13.177 3.39e-06 ***
#   AP            1.0410     0.3101   3.357   0.0121 *  
#   LFPP         -0.5323     0.3101  -1.717   0.1297   
#
# Residual standard error: 0.9182 on 7 degrees of freedom
# Multiple R-squared:  0.7019,	Adjusted R-squared:  0.6167 
# F-statistic: 8.241 on 2 and 7 DF,  p-value: 0.01446

# Results: Multiple regression models with AICc < 2 indicate that precipitation is a positive predictor while low flow pulse percent is a negative predictor of fish diversity.

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------
# Export fish richness multiple regression outputs
write_csv(dredge_fish, "sp17_data_files\\dredge_fish.csv")

# -----------------------------------------------------------------------------
# End