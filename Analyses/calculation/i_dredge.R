# Sean Kinard
# 07-19-2021
# Spring 17 Texas Coastal Prairie

# Invert multiple regressions with environmental predictors

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

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# :::::::::::::::::::::::::::::::::::::::::::::::::::::
# Multivariate Regression analysis
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

# Export invert richness multiple regression outputs
write_csv(dredge_invert, "sp17_data_files\\dredge_invert.csv")

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

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
