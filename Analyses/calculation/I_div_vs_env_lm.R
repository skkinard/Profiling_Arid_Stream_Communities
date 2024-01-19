# Sean Kinard
# 07-19-2021
# Spring 17 Texas Coastal Prairie

# Invert diversity regressions with environmental predictors

# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")

# Load Data
invert <- read_csv("sp17_data_files/invert_diversity_estimates.csv")
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")


# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterinvert <- merge(env,invert, by = "STAID")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation :
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Species richness
summary(lm(formula= rich ~ AP, data = msterinvert))
# p-value: 0.5101

# Shannon Entropy
summary(lm(formula= shannon ~ AP, data = msterinvert))
# p-value: 0.505

# Simpson Entropy
summary(lm(formula= simpson ~ AP, data = msterinvert))
# p-value: 0.5009

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
# Result: Visual inspection suggests that biodiversity peaks within the middle of the rainfall gradient. Linear regressions are not significant for all Hill diversity metrics.

#TRC has extremely low diversity and may constitute an outlier.

# Regressions Removing TRC for outlier effects:
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
# Removal of TRC causes linear regressions between preciptation and diversity to have p-values greater than 0.05 and less than 0.10. However, the hump-shape is still evident and the relationship is still better characterized by an exponential regression.

 
# Conclusion: Invertebrate biodiversity does not exhibit a straightforward linear relationship with annual precipitation, but we observed the highest inverterbate diversity at sites within the middle of the rainfall gradient. 

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
# :::::::::::::::::::::::::::::::::::::::::::::
# Environmental Predictor Linear Regressions:
# ::::::::::::::::::::::::::::::::::::::::::::

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

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
# Supplemental Visualizations

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

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
