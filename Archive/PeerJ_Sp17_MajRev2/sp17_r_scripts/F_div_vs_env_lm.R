# Sean Kinard
# 07-19-2021
# Spring 17 Texas Coastal Prairie

# Fish diversity regressions with environmental predictors

# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")

# Load Data
fish <- read_csv("sp17_data_files/fish_diversity_estimates.csv")
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")


# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterfish <- merge(env,fish, by = "STAID")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation :
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Species richness
summary(lm(formula= rich ~ AP, data = msterfish))
# p-value: 0.0009328

# Shannon Entropy
summary(lm(formula= shannon ~ AP, data = msterfish))
# p-value: p-value: 0.01087

# Simpson Entropy
summary(lm(formula= simpson ~ AP, data = msterfish))
# p-value: p-value: p-value: 0.05519

# plots (All Sites)
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


# Result: Visual inspection suggests that all Hill numbers increase with precipitation. Species richness and shannon entropy relate positively with precipitation. Gini-Simpson relates positivley, but associated p-value was greater than 0.05 (0.05519).

# Conclusion: fish 'biodiversity' increases with annual rainfall

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

write_csv(fish_diversity_vs_precip, "fish_diversity_vs_precip.csv")
# :::::::::::::::::::::::::::::::::::::::::::::
# Environmental Predictor Linear Regressions:
# ::::::::::::::::::::::::::::::::::::::::::::

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
           
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
# Supplemental Visualizations

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

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #











