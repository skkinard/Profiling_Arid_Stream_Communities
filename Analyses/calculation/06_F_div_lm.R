# 06_F_div_lm
# Sean Kinard
# 07-19-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by loading two datasets: one containing fish diversity estimates and another containing environmental data. It then cleans up the environmental dataset to include only the a-priori selected variables and merges it with the fish dataset to ensure matching rows (sites).
# 
# Following the data preparation, the script conducts regression tests to assess the relationship between fish diversity measures and precipitation. It computes linear regressions for species richness, Shannon entropy, and Simpson entropy against annual precipitation (AP) and summarizes the regression results, noting the p-values for each test.
# 
# Based on visual inspection of the regression plots, the script observes that all Hill numbers (species richness, Shannon entropy, and Gini-Simpson index) tend to increase with precipitation. Species richness and Shannon entropy exhibit statistically significant positive relationships with precipitation, while Gini-Simpson shows a positive trend but with a p-value slightly above the conventional significance threshold.
# 
# In the visualization section, the script creates plots to visualize significant fish diversity versus environment regressions. Specifically, it plots Shannon entropy against precipitation (AP) and conductivity (log.cond), applying linear regression lines to illustrate the trends.
# 
# Finally, the script exports regression results and visualizations into separate files for further analysis and reporting.

# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")

# Load Data
fish <- read_csv("sp17_data_files/fish_diversity_estimates.csv")
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterfish <- merge(env,fish, by = "STAID")

# -----------------------------------------------------------------------------
# Regression test: Diversity Correlates with Precipitation
# -----------------------------------------------------------------------------
# Species richness
summary(lm(formula= rich ~ AP, data = msterfish))
# p-value: 0.0009328

# Shannon Entropy
summary(lm(formula= shannon ~ AP, data = msterfish))
# p-value: p-value: 0.01087

# Simpson Entropy
summary(lm(formula= simpson ~ AP, data = msterfish))
# p-value: p-value: p-value: 0.05519

# Result: Visual inspection suggests that all Hill numbers increase with precipitation. Species richness and shannon entropy relate positively with precipitation. Gini-Simpson relates positivley, but associated p-value was greater than 0.05 (0.05519).

# Conclusion: fish 'biodiversity' increases with annual rainfall
# -----------------------------------------------------------------------------
# Visualize regression
# -----------------------------------------------------------------------------
plot_1 <- ggplot(msterfish, aes(x=AP, y=rich)) +
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Species Richness") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

plot_2 <- ggplot(msterfish, aes(x=AP, y=shannon)) +
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon Entropy") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) 

plot_3 <- ggplot(msterfish, aes(x=AP, y=simpson)) +
  geom_smooth(method=lm, se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Gini-Simpson Index") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

grid.arrange(plot_1, plot_2, plot_3, nrow=1)
# -----------------------------------------------------------------------------
# Regression table
# -----------------------------------------------------------------------------
d <- select(msterfish, AP, rich, shannon, simpson)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

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

fish_diversity_vs_precip <- table.lm

# Export table
write_csv(fish_diversity_vs_precip, "fish_diversity_vs_precip.csv")

# -----------------------------------------------------------------------------
# Explore richness vs predictors
# -----------------------------------------------------------------------------
# Warning: looping regressions is not a defensible hypothesis evaluation unless p-value is adjusted to account for permutations

# Running a for loop to run linear regression and export summary statistics to identify significant corelations
d <- select(msterfish, rich, AP:Rosgen.Index)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

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
write_xlsx(as.data.frame(table.rich),"fish_rich_v_environment.xlsx")

# -----------------------------------------------------------------------------
# Explore Shannon vs predictors
# -----------------------------------------------------------------------------
# Warning: looping regressions is not a defensible hypothesis evaluation unless p-value is adjusted to account for permutations
d <- select(msterfish, shannon, AP:Rosgen.Index)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

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
write_xlsx(as.data.frame(table.shannon),"fish_shannon_v_environment.xlsx")

# -----------------------------------------------------------------------------
# Explore Simpson vs predictors
# -----------------------------------------------------------------------------
# Warning: looping regressions is not a defensible hypothesis evaluation unless p-value is adjusted to account for permutations
d <- select(msterfish, simpson, AP:Rosgen.Index)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

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
write_xlsx(as.data.frame(table.simpson), "sp17_r_scripts\\fish_simpson_v_environment.xlsx")
           
# -----------------------------------------------------------------------------
# Supplemental Visualizations
# -----------------------------------------------------------------------------
# Plot significant fish diversity vs environment regressions
plot_fap <- ggplot(msterfish, aes(x=AP, y=shannon)) +
  geom_point(shape=19,aes(size=.1), ) + 
  geom_smooth(method=lm, se=FALSE, color = "black")  +
  scale_x_continuous(name = "Precipitation") +
  scale_y_continuous(name = expression('Shannon Entropy'[ fish])) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_fcond <- ggplot(msterfish, aes(x=log.cond, y=shannon)) +
  geom_point(shape=19,aes(size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  scale_x_continuous(name = "Conductivity") +
  scale_y_continuous(name = expression('Shannon Entropy'[ fish])) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

grid.arrange(plot_fap,plot_fcond, ncol=2)

# -----------------------------------------------------------------------------
# End 06_F_div_lm