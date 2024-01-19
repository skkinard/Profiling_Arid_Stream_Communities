# Sean Kinard
# 07-19-2021
# Spring 17 Texas Coastal Prairie

# Invertebrate diversity calculations & univariate regressions with environmental predictors

# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")
library(iNEXT)

invert <- read_csv("sp17_data_files/sp17_site_x_invert.csv")
i_nosite <- select(invert, -STAID)


i_invert <- iNEXT(as.data.frame(t(i_nosite))) #create iNEXT object
ggiNEXT(i_invert) #plot rarefaction curves

#Extract Hill numbers from iNEXT object
invert$rich <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Species richness")),4]
invert$rich.s.e <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Species richness")),5]
invert$shannon <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Shannon diversity")),4]
invert$shannon.s.e <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Shannon diversity")),5]
invert$simpson <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Simpson diversity")),4]
invert$simpson.s.e <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Simpson diversity")),5]

# Load environmental data 
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


# diversity vs precipitation Linear Regressions
rich_v_AP <- lm(formula= rich ~ AP, data = msterinvert)
summary(rich_v_AP)
# MR outlier p-value: 0.1212

shannon_v_AP <- lm(formula= shannon ~ AP, data = msterinvert)
summary(shannon_v_AP)
# MR outlier p-value: 0.1656


simpson_v_AP <- lm(formula= simpson ~ AP, data = msterinvert)
summary(simpson_v_AP)
# MR outlier p-value: p-value: 0.2671

# MR is an outlier due to high variety of species with a low sample size n = 11

# removed MR
# diversity vs precipitation Linear Regressions
rich_v_AP <- lm(formula= rich ~ AP, data = msterinvert[-7,])
summary(rich_v_AP)
# p-value: 0.002347

shannon_v_AP <- lm(formula= shannon ~ AP, data = msterinvert[-7,])
summary(shannon_v_AP)
# p-value: 0.04427


simpson_v_AP <- lm(formula= simpson ~ AP, data = msterinvert[-7,])
summary(simpson_v_AP)

# plots with outlier
plot_1 <- ggplot(msterinvert, aes(x=AP, y=rich)) +
  geom_point(shape=19,aes(size=.1)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Species Richness") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) +
  ylim(0,30)

plot_2 <- ggplot(msterinvert, aes(x=AP, y=shannon)) +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon Entropy") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) +
  ylim(0,30)

plot_3 <- ggplot(msterinvert, aes(x=AP, y=simpson)) +
  geom_point(shape=19,aes(size=.1)) + 
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Gini-Simpson Index") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) +
  ylim(0,30)

grid.arrange(plot_1, plot_2, plot_3, nrow=1)

# Result: Visual inspection suggests that invertebrate diversity may decrease with rainfall. However, a low value at Tranguitas and high values at sites receiving interemediate rainfall values (80-90 cm/yr) do not permit a significant linear regression.


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

table.lm

# export table
write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\invert_rich_v_environment.xlsx")
# Cleaned up tables in excel and stored in appendix "F_lm"


# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
# Visualizations

# Plot relevant invert x environment regressions

 ggplot(msterinvert, aes(x=LFPP, y=rich)) +
  geom_point(shape=19,aes(size=.1, colour=AP), ) + 
  geom_smooth(method=lm, se=FALSE, color = "black")  +
  scale_x_continuous(name = "Low Flow Pulse %") +
  scale_y_continuous(name = expression('Species Richness'[ invert])) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 



# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
