# Sean Kinard
# 03-30-2021
# Spring 17 Texas Coastal Prairie

# Fish diversity linear regressions & aggregate plot lm&RDA at end of script

# Load Packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(Hmisc)
library("writexl")
library(tidyverse)

# Load Data
env <- read.csv("sp17_data_files/sp17_site_x_env.csv")
fish <- read.csv("sp17_data_files/sp17_site_x_fish.csv")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, canopy, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterfish <- merge(env,fish, by = "STAID")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation :
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

par(mfrow=c(1,1),cex.lab=1.25,font.lab=1, cex.axis=1.2)

# diversity vs precipitation Linear Regressions
lm_fs_vs_AP <- lm(formula= shannon ~ AP, data = msterfish)
summary(lm_fs_vs_AP) # fish

ggplot(msterfish, aes(x=AP, y=shannon)) +
    geom_point(shape=19,aes(size=.1)) + 
    geom_smooth(method=lm, se=FALSE, color = "black") +
    ggtitle("Fish") +
    scale_x_continuous(name = "Precipitation (cm/yr)") +
    scale_y_continuous(name = "Shannon") +
    annotate("rect", xmin = 90, xmax = 120, ymin = .6, ymax = .9, fill="white", colour="black") +
    annotate("text", x=105, y=0.85, label = "y = 0.017(x) - 0.336") + 
    annotate("text", x=105, y=0.75, label = "R^2 == 0.602", parse=T) + 
    annotate("text", x=105, y=0.65, label = "alpha == 0.008", parse=T) + 
    theme_bw() + 
    theme(legend.position="none") + 
    theme(text = element_text(size = 20)) 

# Result: fish diversity significantly correlates with precipitation 

# :::::::::::::::::::::::::::::::::::::::::::::
# Environmental Predictor Linear Regressions:
# ::::::::::::::::::::::::::::::::::::::::::::

# Running a for loop to run linear regression and export summary statistics to identify significant corelations


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

table.lm

# export table
# write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\fish_diversity_environment_apriori_selected.xlsx")
# Cleaned up tables in excel and stored in appendix "F_lm"


# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
# Visualizations

# 4 regressions have p value less than .05 : "AP", "canopy", "NH4.", "log.cond"
summary(lm(msterfish$shannon ~ msterfish$AP, data = msterfish))
# y= 0.017271(x) - 0.335986
# R2 = 0.602
# p-value = 0.008336

summary(lm(msterfish$shannon ~ msterfish$canopy, data = msterfish))
# y= -0.009296(x) + 1.677553
# R2 = 0.4479
# p-value = 0.03431

summary(lm(msterfish$shannon ~ msterfish$NH4., data = msterfish))
# y = -4.6471(x) - 4.6471
# R2 = 0.4457
# p-value = 0.03493

summary(lm(msterfish$shannon ~ msterfish$log.cond, data = msterfish))
# y = -0.2744(x) + 3.1111
# R2 = 0.4058
# p-value = 0.0476

# Plot significant fish x environment regressions

plot_fap <- ggplot(msterfish, aes(x=AP, y=shannon)) +
  geom_point(shape=19,aes(size=.1, colour=AP), ) + 
  geom_smooth(method=lm, se=FALSE, color = "black")  +
  scale_x_continuous(name = "Precipitation") +
  scale_y_continuous(name = expression('Shannon'[ fish])) +
  annotate("rect", xmin = 100, xmax = 120, ymin = .6, ymax = .9, fill="white", colour="black") +
  annotate("text", x=110, y=0.85, label = "y = 0.017x - 0.336") + 
  annotate("text", x=110, y=0.75, label = "R^2 == 0.602", parse=T) + 
  annotate("text", x=110, y=0.65, label = "alpha == 0.008", parse=T) + 
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_fcanopy <- ggplot(msterfish, aes(x=canopy, y=shannon)) +
  geom_point(shape=19,aes(size=.1, colour=AP)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  scale_x_continuous(name = "Canopy Cover") +
  scale_y_continuous(name = expression('Shannon'[ fish])) +
  annotate("rect", xmin = 0, xmax = 42.5, ymin =.6, ymax = 1.0, fill="white", colour="black") +
  annotate("text", x=21.25, y=0.9, label = "y= -0.00929(x) + 1.677") + 
  annotate("text", x=21.25, y=.8, label = "R^2 == 0.448", parse=T) + 
  annotate("text", x=21.25, y=.7, label = "alpha == 0.0343", parse=T) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_fNH4 <- ggplot(msterfish, aes(x=NH4., y=shannon)) +
  geom_point(shape=19,aes(size=.1, colour=AP)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  scale_x_continuous(name = expression('NH'[4])) +
  scale_y_continuous(name = expression('Shannon'[ fish])) +
  annotate("rect", xmin = .225, xmax = 0.3, ymin = 1.4, ymax = 1.8, fill="white", colour="black") +
  annotate("text", x=.267, y=1.7, label = "-4.64(x) - 4.64") + 
  annotate("text", x=.267, y=1.6, label = "R^2 == 0.445", parse=T) + 
  annotate("text", x=.267, y=1.5, label = "alpha == 0.0349", parse=T) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_fcond <- ggplot(msterfish, aes(x=log.cond, y=shannon)) +
  geom_point(shape=19,aes(size=.1, colour=AP)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  scale_x_continuous(name = "Conductivity") +
  scale_y_continuous(name = expression('Shannon'[ fish])) +
  annotate("rect", xmin = 7.5, xmax = 9, ymin = 1.4, ymax = 1.8, fill="white", colour="black") +
  annotate("text", x=8.25, y=1.7, label = "y = -0.274(x) + 3.11") + 
  annotate("text", x=8.25, y=1.6, label = "R^2 == 0.406", parse=T) + 
  annotate("text", x=8.25, y=1.5, label = "alpha == 0.0476", parse=T) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 


grid.arrange(plot_fap,plot_fcanopy,plot_fcond,plot_fNH4, ncol=2)



# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
# Run script F_RDA before running the following plot arrangements



# Horizontal arrangement: Create Combination plot of fish linear regression + ordination
grid.arrange(plot_fap,plot_fcanopy,plot_frda_spec_nolabel,plot_fcond,plot_fNH4,plot_frda_env_nolabel, ncol=3)
grid.arrange(plot_fap,plot_fcanopy,plot_frda_spec, plot_fcond,plot_fNH4,plot_frda_env, ncol=3)

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #











