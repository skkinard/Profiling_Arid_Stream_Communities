# Last update: 3-24-2020
# Subject: Spring 2017 Fish Community analysis along Precipitation Gradient in South Texas
# Analyst: Sean Kinard

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
fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
emat <- read.csv("sp17_data_files\\sp17_site_x_env.csv", fileEncoding="UTF-8-BOM")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")

# Merge Community x site with env x site
msterfish <- merge(fmat, emat, by = "STAID")
row.names(msterfish) <- msterfish$staid
msterfish <- Filter(is.numeric, msterfish)

msterinvert <- merge(imat,emat, by = "STAID")
row.names(msterinvert) <- msterinvert$staid
msterfish <- Filter(is.numeric, msterfish)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Linear regression Using Shannon Index and Rarified Richness
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Overview of diversity vs environmental predictors
aug.pairs(msterfish[,c(3,4,23:32)], na.action=na.omit)
aug.pairs(msterfish[,c(3,4,33:42)], na.action=na.omit)
aug.pairs(msterfish[,c(3,4,43:47)], na.action=na.omit)

aug.pairs(msterinvert[,c(3,4,100:109)], na.action=na.omit)
aug.pairs(msterinvert[,c(3,4,110:119)], na.action=na.omit)
aug.pairs(msterinvert[,c(3,4,120:124)], na.action=na.omit)

# Identified need to log transform conductivity & NO3
msterfish$conductivity<-log(msterfish$conductivity)
msterfish$NO3.<-log(msterfish$NO3.)
names(msterfish)[which(names(msterfish) == "conductivity")] <- "log.cond"
names(msterfish)[which(names(msterfish) == "NO3.")] <- "log.no3"

msterinvert$conductivity<-log(msterinvert$conductivity)
msterinvert$NO3.<-log(msterinvert$NO3.)
names(msterinvert)[which(names(msterinvert) == "conductivity")] <- "log.cond"
names(msterinvert)[which(names(msterinvert) == "NO3.")] <- "log.no3"


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Linear Regression models
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# select dependent variable: Fish Rarified richness
d <- msterfish[,-c(1:2,4:21)] 
d.var <- noquote(names(d)[1])
# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d)-1)
df <- rep(0,length(d)-1)
r2 <- rep(0,length(d)-1)
f.stat <- rep(0,length(d)-1)
p.value <- rep(0,length(d)-1)
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
# run regressions for each independent variable and store in a table                                       
for(i in 2:length(d)){
    i.var <- noquote(names(d)[i])
    formula = paste0(d.var, " ~ ", i.var)
    regression <- lm(formula, data=d)
    j <- i-1
    table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
    table.lm$df[j] <- summary(regression)$df[2]
    table.lm$r2[j] <- summary(regression)$r.squared
    table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
    table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
    table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
    summary(regression)$fstatistic[2], 
    summary(regression)$fstatistic[3],
    lower.tail=FALSE) 
    }
# assign table values to a new object                                       
fishrich.lm <- table.lm

# Select dependent variable: Fish Shannon Index
d <- msterfish[,-c(1,3:21)] 
d.var <- noquote(names(d)[1])
# Create output table
predictor <- names(d[,-1])
estimate <- rep(0,length(d)-1)
df <- rep(0,length(d)-1)
r2 <- rep(0,length(d)-1)
f.stat <- rep(0,length(d)-1)
p.value <- rep(0,length(d)-1)
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
# Loop regression and export values to table
for(i in 2:length(d)){
    i.var <- noquote(names(d)[i])
    formula = paste0(d.var, " ~ ", i.var)
    regression <- lm(formula, data=d)
    j <- i-1
    table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
    table.lm$df[j] <- summary(regression)$df[2]
    table.lm$r2[j] <- summary(regression)$r.squared
    table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
    table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
    table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
    summary(regression)$fstatistic[2], 
    summary(regression)$fstatistic[3],
    lower.tail=FALSE) 
}
# Save table to new object
fishSI.lm <- table.lm

# Select dependent variable: Invert Rare Rich
d <- msterinvert[,-c(1:3,5:99)] 
d.var <- noquote(names(d)[1])
# create data frame for regression outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d)-1)
df <- rep(0,length(d)-1)
r2 <- rep(0,length(d)-1)
f.stat <- rep(0,length(d)-1)
p.value <- rep(0,length(d)-1)
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)

# Loop regressions and output to table                                       
for(i in 2:length(d)){
    i.var <- noquote(names(d)[i])
    formula = paste0(d.var, " ~ ", i.var)
    regression <- lm(formula, data=d)
    j <- i-1
    table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
    table.lm$df[j] <- summary(regression)$df[2]
    table.lm$r2[j] <- summary(regression)$r.squared
    table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
    table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
    table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                              summary(regression)$fstatistic[2], 
                              summary(regression)$fstatistic[3],
                              lower.tail=FALSE) 
    }
# save table as new object
invertrich.lm <- table.lm

# Select dependent variable: Invert Shannon index
d <- msterinvert[,-c(1,3:99)] 
d.var <- noquote(names(d)[1])
# create data fram for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d)-1)
df <- rep(0,length(d)-1)
r2 <- rep(0,length(d)-1)
f.stat <- rep(0,length(d)-1)
p.value <- rep(0,length(d)-1)
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
# Loop regressions and export values to table                                    
for(i in 2:length(d)){
    i.var <- noquote(names(d)[i])
    formula = paste0(d.var, " ~ ", i.var)
    regression <- lm(formula, data=d)
    j <- i-1
    table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
    table.lm$df[j] <- summary(regression)$df[2]
    table.lm$r2[j] <- summary(regression)$r.squared
    table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
    table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
    table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                              summary(regression)$fstatistic[2], 
                              summary(regression)$fstatistic[3],
                              lower.tail=FALSE) 
}
# save table as new object
invertSI.lm <- table.lm

# Select dependent variable: Invert Shannon index
d <- msterfish[,-c(1:21)] 
d.var <- noquote(names(d)[1])
# create data fram for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d)-1)
df <- rep(0,length(d)-1)
r2 <- rep(0,length(d)-1)
f.stat <- rep(0,length(d)-1)
p.value <- rep(0,length(d)-1)
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
# Loop regressions and export values to table                                    
for(i in 2:length(d)){
    i.var <- noquote(names(d)[i])
    formula = paste0(d.var, " ~ ", i.var)
    regression <- lm(formula, data=d)
    j <- i-1
    table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
    table.lm$df[j] <- summary(regression)$df[2]
    table.lm$r2[j] <- summary(regression)$r.squared
    table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
    table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
    table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                              summary(regression)$fstatistic[2], 
                              summary(regression)$fstatistic[3],
                              lower.tail=FALSE) 
}
# save table as new object
env.cov.lm <- table.lm

# Export dataframes to excel

write.xlsx(fishrich.lm, file="sp17_tables.xlsx", sheetName="frich", row.names=FALSE)
write.xlsx(fishSI.lm, file="sp17_tables.xlsx", sheetName="fSI", append=TRUE, row.names=FALSE)
write.xlsx(invertrich.lm, file="sp17_tables.xlsx", sheetName="irich", append=TRUE, row.names=FALSE)
write.xlsx(invertSI.lm, file="sp17_tables.xlsx", sheetName="iSI", append=TRUE, row.names=FALSE)
write.xlsx(env.cov.lm, file="sp17_tables.xlsx", sheetName="Env", append=TRUE, row.names=FALSE)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Diversity linear regression plots
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

par(mfrow=c(4,3), mai = c(.6, .6, .2, .2),cex.lab=1.25,font.lab=1, cex.axis=1.2)

plot(shannon ~ AP, data=msterfish, pch=19, lty=3, xlab="Precipitation", ylab="Shannon-fish", cex=1.2, cex.lab=1.5)
abline(lm(shannon ~ AP, msterfish))
mtext(paste0("(", "a", ")"), side = 3, adj = 0.05, line = -1.3,)
plot(shannon ~ PET, data=msterfish, pch=19, lty=3, xlab="Potential Evapotranspiration", ylab="Shannon-fish", cex=1.2, cex.lab=1.5)
abline(lm(shannon ~ PET, msterfish))
mtext(paste0("(", "b", ")"), side = 1, adj = 0.05, line = -1.3)
plot(shannon ~ log.cond, data=msterfish, pch=19, lty=3, xlab="Conductivity", ylab="Shannon-fish", cex=1.2, cex.lab=1.5)
abline(lm(shannon ~ log.cond, msterfish))
mtext(paste0("(", "c", ")"), side = 1, adj = 0.05, line = -1.3)
plot(shannon ~ NH4., data=msterfish, pch=19, lty=3, xlab=expression('NH'[4]^' +'), ylab="Shannon-fish", cex=1.2, cex.lab=1.5)
abline(lm(shannon ~ NH4., msterfish))
mtext(paste0("(", "d", ")"), side = 1, adj = 0.05, line = -1.3)
plot(shannon ~ runoff.factor, data=msterfish, pch=19, lty=3, xlab="Surface Runoff", ylab="Shannon-fish", cex=1.2, cex.lab=1.5)
abline(lm(shannon ~ runoff.factor, msterfish))
mtext(paste0("(", "e", ")"), side = 3, adj = 0.05, line = -1.3)
plot(rare.rich ~ runoff.factor, data=msterfish, pch=19, lty=3, xlab="Surface Runoff", ylab="Richness-fish", cex=1.2)
abline(lm(rare.rich ~ runoff.factor, msterfish))
mtext(paste0("(", "f", ")"), side = 3, adj = 0.05, line = -1.3)
plot(rare.rich ~ Rip.forest, data=msterfish, pch=19, lty=3, xlab="Forested Riparian", ylab="Richness-fish", cex=1.2, cex.lab=1.5)
abline(lm(rare.rich ~ Rip.forest, msterfish))
mtext(paste0("(", "g", ")"), side = 3, adj = 0.05, line = -1.3)
plot(shannon ~ LFPP, data=msterinvert, pch=19, lty=3, xlab="Low Flow Pulse %", ylab="Shannon-invert", cex=1.2, cex.lab=1.5)
abline(lm(shannon ~ LFPP, msterinvert))
mtext(paste0("(", "h", ")"), side = 1, adj = 0.05, line = -1.3)
fit <- lm(shannon~poly(AP,2,raw=TRUE),msterinvert)
plot(shannon~AP,msterinvert,pch=19, lty=3, cex=1.2, cex.lab=1.5, xlab="Precipitation", ylab="Shannon-invert")
curve(predict(fit,newdata=data.frame(AP=x)),add=T)
summary(fit)
mtext(paste0("(", "i", ")"), side = 3, adj = 0.05, line = -1.3)
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# covarying environmental variable regressions
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
plot(AP ~ log.cond, data=msterfish, pch=19, lty=3, xlab="Conductivity", ylab="Precipitation", cex=1.2, cex.lab=1.5)
abline(lm(AP ~ log.cond,msterfish))
mtext(paste0("(", "j", ")"), side = 1, adj = 0.05, line = -1.3)
plot(AP~runoff.factor, data=msterfish, pch=19, lty=3, xlab="Surface Runoff", ylab="Precipitation", cex=1.2, cex.lab=1.5)
abline(lm(AP ~ runoff.factor,msterfish))
mtext(paste0("(", "k", ")"), side = 3, adj = 0.05, line = -1.3)
plot(AP ~ PET, data=msterfish, pch=19, lty=3, xlab="Potential Evapotranspiration", ylab="Precipitation", cex=1.2, cex.lab=1.5)
abline(lm(AP ~ PET,msterfish))
mtext(paste0("(", "l", ")"), side = 1, adj = 0.05, line = -1.3)

# Fitting Quadratic to invert SI and richness
fit <- lm(shannon~poly(ppt,2,raw=TRUE),msterinvert)
plot(shannon~ppt,msterinvert,pch=19, lty=3, cex=2, xlab="Precipitation (cm/yr)", ylab="Shannon (i)", cex.lab=1.5, cex.axis=2)
curve(predict(fit,newdata=data.frame(ppt=x)),add=T, lw=2)
summary(fit)

fit <- lm(rare.rich~poly(ppt,2,raw=TRUE),msterinvert)
plot(rare.rich~ppt,msterinvert,pch=19, lty=3, cex=2, xlab="Precipitation (cm/yr)", ylab="richness (i)", cex.lab=1.5, cex.axis=2)
curve(predict(fit,newdata=data.frame(ppt=x)),add=T, lw=2)
summary(fit)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# NMDS Fish
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

ford <- msterfish
ford <- ford[order(ford$ppt),]
rownames(ford) <- 1:10
nms1 <- metaMDS(ford[,c(4:21)], noshare=0.2,k=2)

# heirarchical clustering identifies groupings of sites
par(mfrow=c(1,1))
clusters <- hclust(dist(ford[,c(22:24)]), method="ward.D")
plot(clusters)
rect.hclust(clusters,3)
clusterCut <- cutree(clusters,3)
table(clusterCut, ford$ppt)
ford$clusterCut <- clusterCut
# Shade with Color Gradient based on precipitation
rbPal <- colorRampPalette(c('yellow','blue')) 
ford$Col <- rbPal(10)[as.numeric(cut(ford$ppt,breaks = 10))]#This adds a column of color values based on the y values

# Fit Environmental variables to ordination
ffit <- envfit(nms1, ford[,c(23:49)], perm = 999)
# Export fitted environmental variable stats
flist <- ffit[[1]]
fefit <- as.data.frame(flist[[c(1)]])
r2 <- ffit[[1:2]]
fefit$r2 <- as.vector(r2)
p_value <- ffit[[c(1,4)]]
fefit$p_value <- as.vector(p_value)
write.xlsx(fefit, file="sp17_tables.xlsx", sheetName="fefit", append=TRUE, row.names=TRUE)
# Plotting environmental variables ( red p-value < 0.1 )
plot(nms1)
plot(ffit)
plot(ffit, p.max = 0.1, col = "red")

# Fish NMDS Plot with labels
par(mfrow=c(1,1))
plot(nms1, type="n", ylim=c(-1.2,1.2), xlim=c(-1.2,1.2))
ordiellipse(nms1, groups = ford$clusterCut, draw="polygon", kind = "ehull",border=NA, col=c("blue", "green"), alpha=.25)
plot(ffit, p.max = 0.1, col = "black",text=2, cex=1.5)
points(nms1, display = "sites", cex = 2, pch=21, col="black", bg=ford$Col)
ordilabel(nms1, display = "spec", cex=1, col="black")

# Fish NMDS Plot without labels
par(mfrow=c(1,1))
plot(nms1, type="n", ylim=c(-1.2,1.2), xlim=c(-1.2,1.2))
ordiellipse(nms1, groups = ford$clusterCut, draw="polygon", kind = "ehull",border=NA, col=c("blue", "green"), alpha=.25)
plot(ffit, p.max = 0.1, col = "black",text=2, cex=1.5)
points(nms1, display = "sites", cex = 2, pch=21, col="black", bg=ford$Col)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# NMDS Invert
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

iord <- msterinvert
iord <- iord[order(iord$ppt),]
rownames(iord) <- 1:10
nms2 <- metaMDS(iord[,c(5:99)], noshare=0.2,k=2)
# heirarchical clustering identifies groupings of sites
par(mfrow=c(1,1))
clusters <- hclust(dist(ford[,c(22:31)]), method="ward.D")
plot(clusters)
rect.hclust(clusters,3)
clusterCut <- cutree(clusters,3)
table(clusterCut, iord$ppt)
iord$clusterCut <- clusterCut

# Shade with Color Gradient based on precipitation
rbPal <- colorRampPalette(c('yellow','blue')) 
iord$Col <- rbPal(10)[as.numeric(cut(iord$ppt,breaks = 10))]#This adds a column of color values based on the y values

# Fit Environmental variables to ordination
ifit <- envfit(nms1, iord[,c(100:129)], perm = 999)
# Export fitted environmental variable stats
flist <- ifit[[1]]
iefit <- as.data.frame(flist[[c(1)]])
r2 <- ifit[[1:2]]
iefit$r2 <- as.vector(r2)
p_value <- ifit[[c(1,4)]]
iefit$p_value <- as.vector(p_value)
write.xlsx(iefit, file="sp17_tables.xlsx", sheetName="iefit", append=TRUE, row.names=TRUE)


# Plotting environmental variables ( red p-value < 0.1 )
plot(nms1)
plot(ifit)
plot(ifit, p.max = 0.1, col = "red")

# invert NMDS Plot with labels
par(mfrow=c(1,1))
plot(nms2, type="n", ylim=c(-1.2,1.2), xlim=c(-1.2,1.2))
ordiellipse(nms2, groups = iord$clusterCut, draw="polygon", kind = "ehull",border=NA, col=c("blue", "green"), alpha=.25)
points(nms2, display = "sites", cex = 2, pch=21, col="black", bg=ford$Col)
plot(ifit, p.max = 0.1, col = "black",text=2, cex=1.5, pos=3)
ordilabel(nms2, display = "spec", cex=.75, col="black")

# Invert NMDS Plot no labels
par(mfrow=c(1,1))
plot(nms2, type="n", ylim=c(-1.2,1.2), xlim=c(-1.2,1.2))
ordiellipse(nms2, groups = iord$clusterCut, draw="polygon", kind = "ehull",border=NA, col=c("blue", "green"), alpha=.25)
points(nms2, display = "sites", cex = 2, pch=21, col="black", bg=ford$Col)
plot(ifit, p.max = 0.1, col = "black",text=2, cex=1.5, pos=3)

