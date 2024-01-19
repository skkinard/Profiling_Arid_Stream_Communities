# Sean Kinard
# 03-30-2021
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
fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
emat <- read.csv("sp17_data_files\\sp17_site_x_env.csv", fileEncoding="UTF-8-BOM")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(emat, STAID, AP, flash.index, LFPP, NH4., log.cond, canopy, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterfish <- merge(env,fmat, by = "STAID")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# :::::::::::::::::::::::::::::::::::::::::::::::::::::
# Multivariate Regression analysis
# Fish
# a priori selected environmental variables
# scale environmental predictors
msterfish[,2:8] <- scale(msterfish[,2:8])

full.model.fish <- lm(shannon~ AP + flash.index + LFPP + NH4. + log.cond + canopy + Rosgen.Index , data=msterfish)
summary(full.model.fish)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   1.261048   0.101568  12.416  0.00642 **
#   AP            0.392638   0.402968   0.974  0.43264   
# flash.index   0.006972   0.218168   0.032  0.97741   
# LFPP         -0.262315   0.217824  -1.204  0.35167   
# NH4.          0.060313   0.271008   0.223  0.84455   
# log.cond      0.116788   0.328844   0.355  0.75643   
# canopy       -0.052908   0.233836  -0.226  0.84202   
# Rosgen.Index  0.094603   0.141608   0.668  0.57287   
# ---

# Residual standard error: 0.3212 on 2 degrees of freedom
# Multiple R-squared:  0.8848,	Adjusted R-squared:  0.4818 
# F-statistic: 2.195 on 7 and 2 DF,  p-value: 0.3483

vif(full.model.fish) # Indicates low variance inflation due to colinearities among environmental predictors

# Exhuastive multivariable regression

options(na.action = "na.fail")
ms1 <- dredge(full.model.fish)
options(na.action = "na.omit")
ms1

#average models with delta.aicc < 4
summary(model.avg(ms1, subset = delta < 10))

# exported top ten model outputs and AICc to excel sheet

# delta < 2 multivariate regression models:
fm1 <- lm(shannon~ AP + LFPP, data=msterfish)
fm2 <- lm(shannon ~ AP, data = msterfish)

model_name <- c("fm1", "fm2")
d <- list(summary(fm1), summary(fm2))

# make data frame for model outputs
model_name 
var1 <- rep(0, length(d))
var2 <- rep(0, length(d))
estimate <- rep(0,length(d))
estimate2 <- rep(0,length(d))
adjusted_r2 <- rep(0,length(d))
f.stat <- rep(0,length(d))
df <- rep(0,length(d))
p.value <- rep(0,length(d))
table.lm <- data.frame(model_name, var1, estimate,var2, estimate2, adjusted_r2, f.stat, df, p.value)

for(j in 1:length(d)){
  table.lm$var1[j] <- noquote(rownames(d[[j]]$coefficients)[2])
  table.lm$var2[j] <- noquote(rownames(d[[j]]$coefficients)[3])
  table.lm$estimate[j] <- d[[j]]$coefficients[2,1]
  table.lm$estimate2[j] <- d[[j]]$coefficients[3,1]
  table.lm$df[j] <- paste(d[[j]]$df[1], d[j]$df[2])
  table.lm$adjusted_r2[j] <- d[[j]]$adj.r.squared
  table.lm$f.stat[j] <- d[[j]]$fstatistic[1]
  table.lm$p.value[j] <- d[[j]]$coefficients[2,4]
  table.lm$p.value[j] <- pf(d[[j]]$fstatistic[1], 
                            d[[j]]$fstatistic[2], 
                            d[[j]]$fstatistic[3],
                            lower.tail=FALSE) 
}
table.lm

# write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\fish_top_dredge_stats.xlsx")

# The top 4 multivariate regressions predicting fish shannon diversity have correlations ranging from 0.63 to 0.77 with associated p values less than .05.

# one of the models contains 3 variables:
fm5 <- lm(shannon ~ AP + LFPP + Rosgen.Index, data = msterfish)
summary(fm5)

#   Coefficients  Estimate   Std. Error t value Pr(>|t|)    
#   (Intercept)   1.26105    0.06298    20.022  1.01e-06 ***
#   AP            0.30138    0.06767     4.453  0.00431 ** 
#   LFPP         -0.23384    0.06914    -3.382  0.01482 *  
#   Rosgen.Index  0.10252    0.06844     1.498  0.18475    
# ---
# Residual standard error: 0.1992 on 6 degrees of freedom
# Multiple R-squared:  0.8672,	Adjusted R-squared:  0.8007 
# F-statistic: 13.05 on 3 and 6 DF,  p-value: 0.004866

# Precipitation is a predictor in the top multivariate models and has the largest estimated slope compared to all other scaled environmental variables in the exhaustive multivariate regression.

# One multivariate regression utilizes 2 water quality parameters (conductivity and NH4).

# Positive predictors of fish diversity include AP and Rosgen Index
# Negative predictors of fish diversity include LFPP, canopy, NH4., log.cond

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #









