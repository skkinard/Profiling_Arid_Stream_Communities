# 18_E_regression
# Sean Kinard
# 2-15-2020
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by visualizing the correlations between different environmental variables using the aug.pairs function, which likely generates a matrix of scatterplots with correlations displayed.
# 
# Next, the script performs Multivariate Regression Analysis (MRA) using additive linear regression models to identify multivariate relationships between the flash index and several environmental predictors (LFPP, HFPP, Av.Flow, Rip.forest, AP, DRAIN_SQKM). Two separate models (full.model.flash and full.model.LFPP) are created and analyzed using the lm function. Variance inflation factors (VIFs) are calculated to check for multicollinearity among the predictors.
# 
# The script then conducts exhaustive multivariable regression analysis using the dredge function to explore different model combinations for explaining variation in the flash index and LFPP. The results of these analyses are displayed and evaluated.
# 
# Following the multivariable regression analyses, the script performs linear regressions to assess the relationships between individual environmental predictors and the flash index or LFPP. The p-values and coefficients of determination (R-squared) are reported for each regression model.
# 
# The script further investigates whether watershed factors that covary with flow metrics also covary with precipitation. Linear regression models are fitted to explore the relationship between drainage area (DRAIN_SQKM) and precipitation (AP). Scatterplots with regression lines are generated to visualize these relationships.
# 
# Finally, the script visualizes the relationships between various environmental variables (DRAIN_SQKM, Av.Flow, LFPP, HFPP, AP) using scatterplots with regression lines (geom_smooth). Each plot includes annotations indicating the regression equation, R-squared value, and significance level (alpha). These plots provide insights into the relationships between different environmental variables.


# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
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
library(gridExtra)

# Source stat programs
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')
source('sp17_r_source_files/correctedLMEAnova.R', encoding = 'UTF-8')
source('sp17_r_source_files/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('sp17_r_source_files/multcompUtilities.R', encoding = 'UTF-8')
source('sp17_r_source_files/profilePlot.R', encoding = 'UTF-8')
source('sp17_r_source_files/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

# Load Data
env <- read.csv("sp17_data_files/sp17_site_x_env.csv", encoding = 'UTF-8')
env <- merge(env,drainage, by = STAID)
(env <- select(env, flash.index, LFPP, HFPP, Av.Flow, Rip.forest, AP, DRAIN_SQKM))
env.scale <- as.data.frame(scale(env))

# -----------------------------------------------------------------------------
# Visualize Correlations
# -----------------------------------------------------------------------------
aug.pairs(env.scale, na.action=na.omit)

# -----------------------------------------------------------------------------
# MRA: Identify Multivariate relationships using addivitive lm
# -----------------------------------------------------------------------------
full.model.flash <- lm(flash.index ~ LFPP+ HFPP+ Av.Flow+ Rip.forest+ AP+ DRAIN_SQKM, data=env.scale)
summary(full.model.flash)
# Coefficients:
#           Estimate     Std. Error t value Pr(>|t|)
#(Intercept) -1.680e-18  2.478e-01   0.000    1.000
#LFPP        -6.137e-01  3.490e-01  -1.758    0.177
#HFPP        -4.776e-01  3.332e-01  -1.434    0.247
#Av.Flow     -8.594e-01  5.034e-01  -1.707    0.186
#Rip.forest   1.537e-01  3.277e-01   0.469    0.671
#AP          -5.061e-02  4.544e-01  -0.111    0.918
#DRAIN_SQKM  -1.370e-01  5.620e-01  -0.244    0.823

#Residual standard error: 0.7835 on 3 degrees of freedom
#Multiple R-squared:  0.7954,	Adjusted R-squared:  0.3862 
#F-statistic: 1.944 on 6 and 3 DF,  p-value: 0.3128

vif(full.model.flash) # Indicates low variance inflation due to colinearities among environmental predictors

# Exhuastive multivariable regression

options(na.action = "na.fail")
ms1 <- dredge(full.model.flash)
options(na.action = "na.omit")
ms1

full.model.LFPP <- lm(LFPP ~ flash.index+ HFPP+ Av.Flow+ Rip.forest+ AP+ DRAIN_SQKM, data=env.scale)
summary(full.model.LFPP)
# Coefficients:
#             Estimate  Std. Error t value Pr(>|t|)
#(Intercept)  1.914e-17  2.876e-01   0.000    1.000
#flash.index -8.270e-01  4.703e-01  -1.758    0.177
#HFPP        -6.206e-01  3.516e-01  -1.765    0.176
#Av.Flow     -8.903e-01  6.396e-01  -1.392    0.258
#Rip.forest  -1.109e-01  3.889e-01  -0.285    0.794
#AP          -3.593e-02  5.282e-01  -0.068    0.950
#DRAIN_SQKM  -1.869e-01  6.500e-01  -0.288    0.792
#
#Residual standard error: 0.9095 on 3 degrees of freedom
#Multiple R-squared:  0.7243,	Adjusted R-squared:  0.1729 
#F-statistic: 1.313 on 6 and 3 DF,  p-value: 0.4445

vif(full.model.LFPP) # Indicates low variance inflation due to colinearities among environmental predictors

# Exhuastive multivariable regression

options(na.action = "na.fail")
ms2 <- dredge(full.model.LFPP)
options(na.action = "na.omit")
ms2

# Linear Regressions
flash_drainage <- lm(formula= flash.index ~ DRAIN_SQKM, data = env)
summary(flash_drainage) # p-value: 0.1975

flash_meanflow <- lm(formula= flash.index ~ Av.Flow, data = env)
summary(flash_meanflow) # p-value: 0.06793

# Linear Regressions
LFPP_drainage <- lm(formula= LFPP ~ Rip.forest, data = env)
summary(flash_drainage) # p-value: 0.1989

LFPP_meanflow <- lm(formula= LFPP ~ Av.Flow, data = env)
summary(flash_meanflow) # p-value: 0.4537

# -----------------------------------------------------------------------------
# Do the watershed factors that covary with flow metrics also covary with preicpitation?
# -----------------------------------------------------------------------------
env <- read.csv("sp17_data_files/sp17_site_x_env.csv", encoding = 'UTF-8')
env <- merge(env,drainage, by = STAID)

env_q <- select(env, DRAIN_SQKM, Av.Flow, Bas.forest, AP)
aug.pairs(env_q)
# Drainage sqkm  yes
# Av.flow        no
# Bas.forest     no

drainage_AP <- lm(formula= DRAIN_SQKM ~ AP, data = env_q)
summary(drainage_AP) # p = 0.1457 , R-squared:  0.2452

# plots
env1 <- ggplot(env, aes(x=DRAIN_SQKM, y=flash.index)) +
  geom_point(shape=19,aes(size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = "dashed") +
  scale_x_continuous(name = "Drainage Area (sq km)") +
  scale_y_continuous(name = "Flash Index") +
  annotate("rect", xmin = 750, xmax = 1800, ymin = 1.19, ymax = 1.34, fill="white", colour="black") +
  annotate("text", x=1250, y=1.31, label = "y = -1.50E-5(x) + 0.987") + 
  annotate("text", x=1250, y=1.27, label = "R^2 == 0.198", parse=T) + 
  annotate("text", x=1250, y=1.22, label = "alpha == 0.198", parse=T) + 
  theme_bw() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 12)) 

env2 <- ggplot(env, aes(x=Av.Flow, y=flash.index)) +
  geom_point(shape=19,aes(size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = "dashed") +
  scale_x_continuous(name = "Mean Daily Discharge (cubic m / sec)") +
  scale_y_continuous(name = "Flash Index") +
  annotate("rect", xmin = 45, xmax = 105, ymin = 1.19, ymax = 1.34, fill="white", colour="black") +
  annotate("text", x=75, y=1.31, label = "y = -3.60E-3(x) + 1.03") + 
  annotate("text", x=75, y=1.27, label = "R^2 == 0.3574", parse=T) + 
  annotate("text", x=75, y=1.22, label = "alpha == 0.0679", parse=T) + 
  theme_bw() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 12)) 

env3 <- ggplot(env, aes(x=Rip.forest, y=LFPP)) +
  geom_point(shape=19,aes(size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = "dashed") +
  scale_x_continuous(name = "Forested Riparian Zone (%)") +
  scale_y_continuous(name = "LFPP") +
  annotate("rect", xmin = 16, xmax = 34, ymin = 16.5, ymax = 22.5, fill="white", colour="black") +
  annotate("text", x=25, y=21, label = "y = -0.398(x) + 13.827") + 
  annotate("text", x=25, y=19.5, label = "R^2 == 0.197", parse=T) + 
  annotate("text", x=25, y=18, label = "alpha == 0.199", parse=T) + 
  theme_bw() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 12)) 

env4 <- ggplot(env_q, aes(x=AP, y=DRAIN_SQKM)) +
  geom_point(shape=19,aes(size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = "dashed") +
  scale_x_continuous(name = "Precipitation (cm / yr)") +
  scale_y_continuous(name = "Drainage Area (sq km)") +
  annotate("rect", xmin = 90, xmax = 120, ymin = 1100, ymax = 1500, fill="white", colour="black") +
  annotate("text", x=105, y=1400, label = "y = -0.398(x) + 1872") + 
  annotate("text", x=105, y=1300, label = "R^2 == 0.2452", parse=T) + 
  annotate("text", x=105, y=1200, label = "alpha == 0.1457", parse=T) + 
  theme_bw() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 12))

grid.arrange(env1, env2, env3, env4, ncol=2)



env <- read.csv("sp17_data_files/sp17_site_x_env.csv", encoding = 'UTF-8')
env <- merge(env,drainage, by = STAID)

env_q <- select(env, DRAIN_SQKM, Av.Flow, LFPP, HFPP, AP)

aug.pairs(env_q)

# -----------------------------------------------------------------------------
# End 18_E_regression