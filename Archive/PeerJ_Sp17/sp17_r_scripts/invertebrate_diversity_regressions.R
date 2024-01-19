# sp_17 Invertebrate regression
# 12-14-2020
# Sean Kinard

#::::::::::::::::::::::::::::::
# Load programs and datasets  :
# :::::::::::::::::::::::::::::

# Load Packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(Hmisc)
library("writexl")

# Set Work Directory
setwd("R:\\Current Projects\\Texas\\Sean Folder\\PeerJ_Sp17\\")

# Import Data Files
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv")
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")

# Merge Community x site with env x site
masterinvert <- merge(imat, emat, by = "STAID")
row.names(masterinvert) <- masterinvert$staid
masterinvert <- Filter(is.numeric, masterinvert)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation :
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# invetebrate diversity vs precipitation Linear Regressions
lm_is_vs_AP <- lm(formula= shannon ~ AP, data = masterinvert)
summary(lm_is_vs_AP)

plot_iap <- ggplot(masterinvert, aes(x=AP, y=shannon)) +
  geom_point(aes(color = AP, size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = expression('Shannon'[ invertebrate])) +
  annotate("rect", xmin = 100, xmax = 120, ymin = 1.7, ymax = 2.0, fill="white", colour="black") +
  annotate("text", x=110, y=1.9, label = "R^2 == 0.0174", parse=T) + 
  annotate("text", x=110, y=1.8, label = "alpha == 0.717", parse=T) + 
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none")  

# Result: Invertebrate diversity does NOT significantly correlate with precipitation 

# Invertebrate Diversity vs 7 a piori selected environmental predictors:
myvariables <- which(colnames(masterinvert) == "AP" |
                       colnames(masterinvert) == "flash.index" |
                       colnames(masterinvert) == "LFPP" |
                       colnames(masterinvert) == "NH4."|
                       colnames(masterinvert) == "log.cond"|
                       colnames(masterinvert) == "canopy"|
                       colnames(masterinvert) == "Rosgen.Index")

# Running a for loop to run linear regression and export summary statistics to identify significant corelations

d <- masterinvert[,c(2,myvariables)] # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

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
# write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\invertebrate_diversity_environment_apriori_selected.xlsx")

# Regressions have p value less than .05 : "LFPP"
summary(lm(masterinvert$shannon ~ masterinvert$LFPP, data = masterinvert))
# y= -0.03498(x) + 3.05718
# R2 = 0.4114
# p-value = 0.04563

# Plot significant invert x environment regressions

plot_ilfpp <- ggplot(masterinvert, aes(x=LFPP, y=shannon)) +
  geom_point(aes(color = AP, size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")  +
  scale_x_continuous(name = "Low Flow Pulse Percent") +
  scale_y_continuous(name = expression('Shannon'[ invertebrate])) +
  annotate("rect", xmin = 0, xmax = 10, ymin = 1.8, ymax = 2.2, fill="white", colour="black") +
  annotate("text", x=5, y=2.1, label = "y = -0.0350x + 3.06") + 
  annotate("text", x=5, y=2.0, label = "R^2 == 0.411", parse=T) + 
  annotate("text", x=5, y=1.9, label = "alpha == 0.0456", parse=T) + 
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none")


# Horizontal arrangement: Create Combination plot of fish linear regression + ordination
grid.arrange(plot_iap,plot_ilfpp,plot_irda_spec_nolabel,plot_irda_env_nolabel, ncol=2)
grid.arrange(plot_iap,plot_ilfpp,plot_irda_spec,plot_irda_env, ncol=2)

