# Sean Kinard
# 07-19-2021
# Spring 17 Texas Coastal Prairie

# Fish diversity calculations & univariate regressions with environmental predictors

# Load Packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library("writexl")
library(iNEXT)

fish <- read_csv("sp17_data_files/sp17_site_x_fish.csv")
f_nosite <- select(fish, -site.code, -STAID)


i_fish <- iNEXT(as.data.frame(t(f_nosite))) #create iNEXT object
ggiNEXT(i_fish) #plot rarefaction curves

#Extract Hill numbers from iNEXT object
fish$rich <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Species richness")),4]
fish$rich.s.e <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Species richness")),5]
fish$shannon <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Shannon diversity")),4]
fish$shannon.s.e <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Shannon diversity")),5]
fish$simpson <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Simpson diversity")),4]
fish$simpson.s.e <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Simpson diversity")),5]

# Load environmental data 
env <- read_csv("sp17_data_files/sp17_site_x_env.csv")


# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, STAID, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

# Merge dataframes to ensure matching rows (sites) 
msterfish <- merge(env,fish, by = "STAID")
msterfish_orm <- msterfish[-c(which(msterfish$shannon.s.e > 1)),] # data frame omitting 2 sites with high standard error associated with estimates of diversity estimates

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation :
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Species richness
summary(lm(formula= rich ~ AP, data = msterfish))
# p-value: 0.1212
summary(lm(formula= rich ~ AP, data = msterfish_orm))
# outliers removed, p-value: 0.004259

# Shannon Entropy
summary(lm(formula= shannon ~ AP, data = msterfish))
# p-value: p-value: 0.1656
summary(lm(formula= shannon ~ AP, data = msterfish_orm))
# outliers removed, p-value: 0.002131

# Simpson Entropy
summary(lm(formula= simpson ~ AP, data = msterfish))
# p-value: p-value: p-value: 0.2671
summary(lm(formula= simpson ~ AP, data = msterfish_orm))
# outliers removed, p-value: p-value: 0.03707

# plots (All Sites)
plot_1 <- ggplot(msterinvert, aes(x=AP, y=rich)) +
  geom_smooth(method=loess, se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) +
  geom_errorbar(aes(ymin = msterinvert$rich-msterinvert$rich.s.e, ymax = msterinvert$rich + msterinvert$rich.s.e)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Species Richness") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

plot_2 <- ggplot(msterinvert, aes(x=AP, y=shannon)) +
  geom_smooth(method=loess, se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  geom_errorbar(aes(ymin = msterinvert$shannon-msterinvert$shannon.s.e, ymax = msterinvert$shannon + msterinvert$shannon.s.e)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon Entropy") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) 

plot_3 <- ggplot(msterinvert, aes(x=AP, y=simpson)) +
  geom_smooth(method=loess, se=FALSE, color = "grey", linetype = 'dashed') +
  geom_point(shape=19,aes(size=.1)) + 
  geom_errorbar(aes(ymin = msterinvert$simpson-msterinvert$simpson.s.e, ymax = msterinvert$simpson + msterinvert$simpson.s.e)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Gini-Simpson Index") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))


grid.arrange(plot_1, plot_2, plot_3, nrow=1)


# Result: Visual inspection suggests that all Hill numbers increase with precipitation. However, there wer two outliers at Mission River and Placedo Creek which may have been caused by a low sample size, high species count, and evenness of catch compared to other electrofishing events. We computed regressions with all sites included and relationships with precipitation were not significant. But when the sites with shannon entropy standard errors greater than one  were removed, all hill numbers had positive relationships with rainfall. 


# plots Exlcuding outliers at MR and PLC
plot_4 <- ggplot(msterfish_orm, aes(x=AP, y=rich)) +
  geom_smooth(method=lm, se=FALSE, color = "red") +
  geom_point(shape=19,aes(size=.1)) +
  geom_errorbar(aes(ymin = msterfish_orm$rich-msterfish_orm$rich.s.e, ymax = msterfish_orm$rich + msterfish_orm$rich.s.e)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Species Richness") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))

plot_5 <- ggplot(msterfish_orm, aes(x=AP, y=shannon)) +
  geom_smooth(method=lm, se=FALSE, color = "red") +
  geom_point(shape=19,aes(size=.1)) + 
  geom_errorbar(aes(ymin = msterfish_orm$shannon-msterfish_orm$shannon.s.e, ymax = msterfish_orm$shannon + msterfish_orm$shannon.s.e)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon Entropy") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) 

plot_6 <- ggplot(msterfish_orm, aes(x=AP, y=simpson)) +
  geom_smooth(method=lm, se=FALSE, color = "red") +
  geom_point(shape=19,aes(size=.1)) + 
  geom_errorbar(aes(ymin = msterfish_orm$simpson-msterfish_orm$simpson.s.e, ymax = msterfish_orm$simpson + msterfish_orm$simpson.s.e)) +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Gini-Simpson Index") +
  theme_classic() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20))


grid.arrange(plot_4, plot_5, plot_6, nrow=1)


# comparison plots
grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, nrow=2)



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

fish.all.diversity <- table.lm

d <- select(msterfish_orm, AP, rich, shannon, simpson)  # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

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

fish.orm.diversity <- table.lm


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
write_xlsx(as.data.frame(table.rich),"sp17_r_scripts\\fish_rich_v_environment.xlsx")

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
write_xlsx(as.data.frame(table.shannon),"sp17_r_scripts\\fish_shannon_v_environment.xlsx")

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
write_xlsx(as.data.frame(table.simpson),"sp17_r_scripts\\fish_simpson_v_environment.xlsx"
           
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #
# Supplemental Visualizations

# Plot significant fish x environment regressions

plot_fap <- ggplot(msterfish, aes(x=AP, y=rich)) +
  geom_point(shape=19,aes(size=.1), ) + 
  geom_smooth(method=lm, se=FALSE, color = "black")  +
  scale_x_continuous(name = "Precipitation") +
  scale_y_continuous(name = expression('Species Richness'[ fish])) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_fNH4 <- ggplot(msterfish, aes(x=NH4., y=rich)) +
  geom_point(shape=19,aes(size=.1, colour=AP)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  scale_x_continuous(name = expression('NH'[4])) +
  scale_y_continuous(name = expression('Species Richness'[ fish])) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 

plot_fcond <- ggplot(msterfish, aes(x=log.cond, y=rich)) +
  geom_point(shape=19,aes(size=.1, colour=AP)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  scale_x_continuous(name = "Conductivity") +
  scale_y_continuous(name = expression('Species Richness'[ fish])) +
  theme_classic() +
  scale_colour_viridis_c(direction=-1) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="none") 


grid.arrange(plot_fap,plot_fcond,plot_fNH4, ncol=2)

# End
# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #











