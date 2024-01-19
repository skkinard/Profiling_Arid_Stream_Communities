# multivariate regression analysis
# Sean Kinard
# 12-17-2020


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load programs and datasets
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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

# Set Work Directory
setwd("C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats")

# Source stat programs
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')
source('sp17_r_source_files/correctedLMEAnova.R', encoding = 'UTF-8')
source('sp17_r_source_files/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('sp17_r_source_files/multcompUtilities.R', encoding = 'UTF-8')
source('sp17_r_source_files/profilePlot.R', encoding = 'UTF-8')
source('sp17_r_source_files/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

# Import Data Files
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")

# Merge Community x site with env x site
msterinvert <- merge(imat,emat, by = "STAID")
row.names(msterinvert) <- msterinvert$staid
msterinvert <- Filter(is.numeric, msterinvert)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::
# Multivariate Regression analysis
# Invertebrate
# PCA selected environmental variables
# scale environmental predictors
msterinvert[,101:124] <- scale(msterinvert[,101:124])

full.model.invertebrate <- lm(shannon~ AP + flash.index + LFPP + NH4. + log.cond + canopy + Rosgen.Index, data=msterinvert)
summary(full.model.invertebrate)
vif(full.model.invertebrate)

# Exhuastive multivariable regression
options(na.action = "na.fail")
dredge(full.model.invertebrate)
options(na.action = "na.omit")

# exported top ten model outputs and AICc to excel sheet

# models ranked 1st through 9th contain 0-1 predictor variables.
# The tenth AICc ranked model contains 2 variables:

im1 <- lm(shannon~ LFPP + NH4., data=msterinvert)
im2 <- lm(shannon~ canopy + LFPP, data=msterinvert)
im3 <- lm(shannon ~ AP + LFPP, data = msterinvert)

model_name <- c("im1", "im2", "im3")
d <- list(summary(im1), summary(im2), summary(im3))
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
write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\invertebrate_multivariate_regression_stats.xlsx")

# 7/10 top AICc models contained one or fewer environmental predictors. Previous univariate regressions indicate no statistically significant correlations

# 1/3 of the top AICc ranked multivariation regression models has a p value less than 0.05 and has an adjusted r^2 of 0.519 (-LFPP + NH4)

# Positive predictors of invertebrate diversity include NH4 and canopy
# Negative predictors of invertebrate diversity include LFPP and AP


