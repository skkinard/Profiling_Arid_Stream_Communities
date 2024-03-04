# 08_I_div_lm
# Sean Kinard
# 07-19-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by loading two datasets: one containing invertebrate diversity estimates and another containing environmental data. It then cleans up the environmental dataset to include only the a-priori selected variables and merges it with the invertebrate dataset to ensure matching rows (sites).

# Following the data preparation, the script conducts regression tests to assess the relationship between invertebrate diversity measures and precipitation. It computes linear regressions for species richness, Shannon entropy, and Simpson entropy against annual precipitation (AP) and summarizes the regression results, noting the p-values for each test.

# Based on visual inspection of the regression plots, the script observes that biodiversity peaks within the middle of the rainfall gradient. However, linear regressions are not statistically significant for all Hill diversity metrics.

# The script then removes potential outliers and repeats the regression analyses. Despite removing the potential outlier, the linear regressions between precipitation and diversity still have p-values greater than 0.05 and less than 0.10. However, the hump-shaped relationship is still evident, suggesting that the relationship is better characterized by an exponential regression.

# In the visualization section, the script creates plots to visualize significant invertebrate diversity versus environment regressions, focusing on precipitation (AP) and low flow pulse percentage (LFPP).

# Finally, the script exports regression results and visualizations into separate files for further analysis and reporting.

# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# Invert diversity regressions with environmental predictors

# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")

# Load Data
invert <- read_csv("sp17_data_files/invert_diversity_estimates.csv")
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")


# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterinvert <- merge(env,invert, by = "STAID")

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Regression test: Diversity Correlates with Precipitation
# -----------------------------------------------------------------------------

# Species richness
summary(lm(formula= rich ~ AP, data = msterinvert))
# p-value: 0.5101

# Shannon Entropy
summary(lm(formula= shannon ~ AP, data = msterinvert))
# p-value: 0.505

# Simpson Entropy
summary(lm(formula= simpson ~ AP, data = msterinvert))
# p-value: 0.5009

# Result: Visual inspection suggests that biodiversity peaks within the middle of the rainfall gradient. Linear regressions are not significant for all Hill diversity metrics.

# TRC has extremely low diversity and may constitute an outlier.

# -----------------------------------------------------------------------------
# Visualize Regression
# -----------------------------------------------------------------------------
# plots (All Sites)
plot_1 <- ggplot(msterinvert, aes(x=AP, y=rich)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 3), se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Species Richness") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

plot_2 <- ggplot(msterinvert, aes(x=AP, y=shannon)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 3), se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon Entropy") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) 

plot_3 <- ggplot(msterinvert, aes(x=AP, y=simpson)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 3), se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Gini-Simpson Index") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

grid.arrange(plot_1, plot_2, plot_3, nrow=1)

# -----------------------------------------------------------------------------
# Regressions Removing TRC for outlier effects:
# -----------------------------------------------------------------------------
msterinvert_TRCrm <- msterinvert[-c(which(msterinvert$rich < 9)),]

# Species richness
summary(lm(formula= rich ~ AP, data = msterinvert_TRCrm))
# p-value: 0.09988

# Shannon Entropy
summary(lm(formula= shannon ~ AP, data = msterinvert_TRCrm))
# p-value: 0.08669

# Simpson Entropy
summary(lm(formula= simpson ~ AP, data = msterinvert_TRCrm))
# p-value: 0.07457

# Removal of TRC causes linear regressions between preciptation and diversity to have p-values greater than 0.05 and less than 0.10. However, the hump-shape is still evident and the relationship is still better characterized by an exponential regression.

# Conclusion: Invertebrate biodiversity does not exhibit a straightforward linear relationship with annual precipitation, but we observed the highest inverterbate diversity at sites within the middle of the rainfall gradient. 

# -----------------------------------------------------------------------------
# Visualize Regression without TR (potential outlier)
# -----------------------------------------------------------------------------
# plots (All Sites)
plot_4 <- ggplot(msterinvert_TRCrm, aes(x=AP, y=rich)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 3) , se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Species Richness") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

plot_5 <- ggplot(msterinvert_TRCrm, aes(x=AP, y=shannon)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 3) , se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon Entropy") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

plot_6 <- ggplot(msterinvert_TRCrm, aes(x=AP, y=simpson)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 3) , se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Gini-Simpson Index") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

grid.arrange(plot_4, plot_5, plot_6, nrow=1)

# -----------------------------------------------------------------------------
# Regression Table
# -----------------------------------------------------------------------------
d <- select(msterinvert, AP, rich, shannon, simpson)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
r2 <- rep(0,length(d[,-1]))
f.stat <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1]))
p.value <- rep(0,length(d[,-1]))
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
d.var <- noquote(names(d)[1])

for(i in 2:length(d)){
  i.var <- noquote(names(d)[i])
  formula = paste0(d.var, " ~ ", i.var)
  regression <- lm(formula, data=d)
  j <- i-1
  table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
  table.lm$df[j] <- paste(summary(regression)$df[1], summary(regression)$df[2])
  table.lm$r2[j] <- summary(regression)$r.squared
  table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
  table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
  table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                            summary(regression)$fstatistic[2], 
                            summary(regression)$fstatistic[3],
                            lower.tail=FALSE) 
}

invert_diversity_vs_precip <- table.lm

write_csv(invert_diversity_vs_precip, "invert_diversity_vs_precip.csv")

# -----------------------------------------------------------------------------
# Explore Rich vs predictors
# -----------------------------------------------------------------------------
# Warning: looping regressions is not a defensible hypothesis evaluation unless p-value is adjusted to account for permutations

# Running a for loop to run linear regression and export summary statistics to identify significant corelations

d <- select(msterinvert, rich, AP:Rosgen.Index)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
r2 <- rep(0,length(d[,-1]))
f.stat <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1]))
p.value <- rep(0,length(d[,-1]))
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
d.var <- noquote(names(d)[1])

for(i in 2:length(d)){
  i.var <- noquote(names(d)[i])
  formula = paste0(d.var, " ~ ", i.var)
  regression <- lm(formula, data=d)
  j <- i-1
  table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
  table.lm$df[j] <- paste(summary(regression)$df[1], summary(regression)$df[2])
  table.lm$r2[j] <- summary(regression)$r.squared
  table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
  table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
  table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                            summary(regression)$fstatistic[2], 
                            summary(regression)$fstatistic[3],
                            lower.tail=FALSE) 
}

table.rich <- table.lm
# export table
write_xlsx(as.data.frame(table.rich),"invert_rich_v_environment.xlsx")

# -----------------------------------------------------------------------------
# Explore Shannon vs predictors
# -----------------------------------------------------------------------------
# Warning: looping regressions is not a defensible hypothesis evaluation unless p-value is adjusted to account for permutations

d <- select(msterinvert, shannon, AP:Rosgen.Index)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
r2 <- rep(0,length(d[,-1]))
f.stat <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1]))
p.value <- rep(0,length(d[,-1]))
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
d.var <- noquote(names(d)[1])

for(i in 2:length(d)){
  i.var <- noquote(names(d)[i])
  formula = paste0(d.var, " ~ ", i.var)
  regression <- lm(formula, data=d)
  j <- i-1
  table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
  table.lm$df[j] <- paste(summary(regression)$df[1], summary(regression)$df[2])
  table.lm$r2[j] <- summary(regression)$r.squared
  table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
  table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
  table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                            summary(regression)$fstatistic[2], 
                            summary(regression)$fstatistic[3],
                            lower.tail=FALSE) 
}

table.shannon <- table.lm
# export table
write_xlsx(as.data.frame(table.shannon),"invert_shannon_v_environment.xlsx")

# -----------------------------------------------------------------------------
# Explore Simpson vs predictors
# -----------------------------------------------------------------------------
# Warning: looping regressions is not a defensible hypothesis evaluation unless p-value is adjusted to account for permutations
d <- select(msterinvert, simpson, AP:Rosgen.Index)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
r2 <- rep(0,length(d[,-1]))
f.stat <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1]))
p.value <- rep(0,length(d[,-1]))
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
d.var <- noquote(names(d)[1])

for(i in 2:length(d)){
  i.var <- noquote(names(d)[i])
  formula = paste0(d.var, " ~ ", i.var)
  regression <- lm(formula, data=d)
  j <- i-1
  table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
  table.lm$df[j] <- paste(summary(regression)$df[1], summary(regression)$df[2])
  table.lm$r2[j] <- summary(regression)$r.squared
  table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
  table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
  table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                            summary(regression)$fstatistic[2], 
                            summary(regression)$fstatistic[3],
                            lower.tail=FALSE) 
}

table.simpson <- table.lm
# export table
write_xlsx(as.data.frame(table.simpson), "invert_simpson_v_environment.xlsx")

# -----------------------------------------------------------------------------
# Supplemental Visualizations
# -----------------------------------------------------------------------------
# Plot significant invert diversity vs environment regressions
plot_iap <- ggplot(msterinvert, aes(x=AP, y=shannon)) +
  geom_point(shape=19,aes(size=.1), ) + 
  scale_x_continuous(name = "Precipitation") +
  scale_y_continuous(name = expression('Shannon Entropy')) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_iLFPP <- ggplot(msterinvert, aes(x=LFPP, y=shannon)) +
  geom_smooth(method=lm, se=FALSE, color = 'grey', linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Low Flow Pulse %") +
  scale_y_continuous(name = expression('Shannon Entropy')) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

grid.arrange(plot_iap,plot_iLFPP, ncol=2)

# -----------------------------------------------------------------------------
# End 08_I_div_lm