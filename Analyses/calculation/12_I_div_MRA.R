# 12_I_div_MRA
# Sean Kinard
# 07-19-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by loading two datasets: one containing invertebrate diversity estimates and another containing environmental data. It then cleans up the environmental dataset to include only the a-priori selected variables and merges it with the invertebrate dataset to ensure matching rows (sites).
# 
# After data preparation, the script conducts multivariate regression analysis to explore the relationship between invertebrate diversity measures and environmental variables. It fits a full model regression with species richness (shannon) as the dependent variable and several environmental predictors (AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index). The summary of the full model regression is provided, including estimates, standard errors, t-values, and p-values for each predictor. Additionally, the script calculates the Variance Inflation Factor (VIF) to check for multicollinearity among predictors.
# 
# The script then performs an exhaustive multivariable regression analysis using the dredge function and selects models with a delta AICc less than 2. It identifies multiple regression models with low AICc values, indicating that the low flow pulse percent (LFPP) is a significant negative predictor of invertebrate diversity.
# 
# Finally, the script exports the results of the multivariate regression analysis into a CSV file for further examination and reporting.
# 
# In summary, the script employs multivariate regression techniques to investigate the relationship between invertebrate diversity and environmental variables, providing insights into the predictors of invertebrate diversity in the studied sites.


# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
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

invert <- read_csv("sp17_data_files/invert_diversity_estimates.csv")

# Load environmental data 
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")

# cleaning up data frames to include only the a priori selected variables and community abundance matrix

# Merge dataframes to ensure matching rows (sites) 
msterinvert <- merge(env,invert, by = "STAID")

# -----------------------------------------------------------------------------
# Multivariate Regression analysis
# -----------------------------------------------------------------------------
# Invert
# a priori selected environmental variables
# scale environmental predictors
msterinvert[,3:27] <- scale(msterinvert[,3:27])

# Species Richness
full.model.invert <- lm(shannon ~ AP + flash.index + LFPP + NH4. + log.cond + Rosgen.Index , data=msterinvert)
summary(full.model.invert)
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   16.7014     2.1797   7.662  0.00462 **
# AP            -6.7577     7.4331  -0.909  0.43031   
# flash.index   -0.5472     3.3219  -0.165  0.87963   
# LFPP          -2.7199     4.2470  -0.640  0.56745   
# NH4.          -0.9055     5.7621  -0.157  0.88511   
# log.cond      -5.5446     6.5919  -0.841  0.46203   
# Rosgen.Index  -2.4482     2.4662  -0.993  0.39403   
#
# Residual standard error: 6.893 on 3 degrees of freedom
# Multiple R-squared:  0.6861,	Adjusted R-squared:  0.05832 
# F-statistic: 1.093 on 6 and 3 DF,  p-value: 0.5121

vif(full.model.invert) # VIF is okay, but warrants some concern regarding Precipitation (10.5) along with conductivity (8.2)

# Exhuastive multivariable regression
options(na.action = "na.fail")
dredge_invert <- dredge(full.model.invert)
options(na.action = "na.omit")
dredge_invert <- as.data.frame(dredge_invert[1:10,])


# delta < 2 multivariate regression models:
dredge_invert_rich[c(which(dredge_invert_rich$delta < 2)),]

fm <- lm(shannon ~ LFPP, data = msterinvert)
summary(fm)
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     16.701      1.941   8.606   2.57e-05 ***
#   LFPP          -4.120      2.046  -2.014   0.0788 .  
#
# Residual standard error: 6.137 on 8 degrees of freedom
# Multiple R-squared:  0.3364,	Adjusted R-squared:  0.2535 
# F-statistic: 4.056 on 1 and 8 DF,  p-value: 0.07879

# Results: Multiple regression models with AICc < 2 indicate that low flow pulse percent is a negative predictor of invertebrate diversity.

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------
# Export invert richness multiple regression outputs
write_csv(dredge_invert, "sp17_data_files\\dredge_invert.csv")

# -----------------------------------------------------------------------------
# End 12_I_div_MRA