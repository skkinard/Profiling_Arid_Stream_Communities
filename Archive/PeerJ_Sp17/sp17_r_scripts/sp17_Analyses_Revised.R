# Last update: 12-14-2020
# Subject: Spring 2017 Fish & macroinvertebrate Community analysis along Precipitation Gradient in South Texas
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
library(ggplot2)
library(dplyr)
library(gridExtra)
library(Hmisc)
library("writexl")

# Set Work Directory
setwd("R:\\Current Projects\\Texas\\Sean Folder\\PeerJ_Sp17\\")

# Source stat programs
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')
source('sp17_r_source_files/correctedLMEAnova.R', encoding = 'UTF-8')
source('sp17_r_source_files/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('sp17_r_source_files/multcompUtilities.R', encoding = 'UTF-8')
source('sp17_r_source_files/profilePlot.R', encoding = 'UTF-8')
source('sp17_r_source_files/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

# Import Data Files
fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")

# Merge Community x site with env x site
msterfish <- merge(fmat, emat, by = "STAID")
row.names(msterfish) <- msterfish$staid
msterfish <- Filter(is.numeric, msterfish)

msterinvert <- merge(imat,emat, by = "STAID")
row.names(msterinvert) <- msterinvert$staid
msterfish <- Filter(is.numeric, msterfish)


# ::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation
# :::::::::::::::::::::::::::::::::::::::::

par(mfrow=c(1,2),cex.lab=1.25,font.lab=1, cex.axis=1.2)

# diversity vs precipitation Linear Regressions
lm_fs_vs_AP <- lm(formula= shannon ~ AP, data = msterfish)
summary(lm_fs_vs_AP) # fish

lm_is_vs_AP <- lm(formula= shannon ~ AP, data = msterinvert)
summary(lm_is_vs_AP) # invert

plot0 <- ggplot(msterfish, aes(x=AP, y=shannon)) +
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

plot1 <- ggplot(msterinvert, aes(x=AP, y=shannon)) +
    geom_point(shape=19,aes(size=.1)) + 
    ggtitle("Invertebrate") +
    scale_x_continuous(name = "Precipitation (cm/yr)") +
    scale_y_continuous(name = "Shannon") +
    annotate("rect", xmin = 100, xmax = 120, ymin =2.0, ymax = 2.25, fill="white", colour="black") +
    annotate("text", x=110, y=2.18, label = "R^2 == 0.017", parse=T) + 
    annotate("text", x=110, y=2.07, label = "alpha == 0.717", parse=T) + 
    theme_bw() + 
    theme(legend.position="none")+ 
    theme(text = element_text(size = 20))

grid.arrange(plot0, plot1, ncol=2)

# Result: fish diversity significantly correlates with precipitation while invertebrate diversity does not


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Fish Diversity vs 6 environmental predictors indicated by PCA:
# "canopy", "bank,height", "NH4", "Soil.Org", "Bas.forest", "Rip.forest"

# running multiple linear regressions and exporting model outputs in a table
myvariables <- which(colnames(msterfish) == "AP" |
          colnames(msterfish) == "canopy" |
          colnames(msterfish) == "bank.height" |
          colnames(msterfish) == "NH4."|
          colnames(msterfish) == "Soil.Org"|
          colnames(msterfish) == "Bas.forest"|
          colnames(msterfish) == "Rip.forest"
)


d <- msterfish[,c(2,myvariables)] # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1]))
r2 <- rep(0,length(d[,-1]))
f.stat <- rep(0,length(d[,-1]))
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
write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\fish_diversity_environment_table.xlsx")

# 3 regressions have p value less than .05 : canopy and NH4.
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


# significant fish x environment regressions
par(mfrow=c(1,3),cex.lab=1.25,font.lab=1, cex.axis=1.2)

# plot
plot0 <- ggplot(msterfish, aes(x=AP, y=shannon)) +
    geom_point(shape=19,aes(size=.1)) + 
    geom_smooth(method=lm, se=FALSE, color = "black")  +
    scale_x_continuous(name = "Precipitation") +
    scale_y_continuous(name = expression('Shannon'[ (fish)])) +
    annotate("rect", xmin = 100, xmax = 120, ymin = .6, ymax = .9, fill="white", colour="black") +
    annotate("text", x=110, y=0.85, label = "y = 0.017x - 0.336") + 
    annotate("text", x=110, y=0.75, label = "R^2 == 0.602", parse=T) + 
    annotate("text", x=110, y=0.65, label = "alpha == 0.008", parse=T) + 
    theme_bw() + 
    theme(legend.position="none") + 
    theme(text = element_text(size = 20))
          
plot1 <- ggplot(msterfish, aes(x=canopy, y=shannon)) +
      geom_point(shape=19,aes(size=.1)) + 
      geom_smooth(method=lm, se=FALSE, color = "black") +
      scale_x_continuous(name = "Canopy Cover") +
      scale_y_continuous(name = expression('Shannon'[ (fish)])) +
      annotate("rect", xmin = 0, xmax = 42.5, ymin =.6, ymax = 1.0, fill="white", colour="black") +
      annotate("text", x=21.25, y=0.9, label = "y= -0.00929(x) + 1.677") + 
      annotate("text", x=21.25, y=.8, label = "R^2 == 0.448", parse=T) + 
      annotate("text", x=21.25, y=.7, label = "alpha == 0.0343", parse=T) +
      theme_bw() + 
      theme(legend.position="none") + 
      theme(text = element_text(size = 20))

plot2 <- ggplot(msterfish, aes(x=NH4., y=shannon)) +
    geom_point(shape=19,aes(size=.1)) + 
    geom_smooth(method=lm, se=FALSE, color = "black") +
    scale_x_continuous(name = expression('NH'[4])) +
    scale_y_continuous(name = expression('Shannon'[ (fish)])) +
    annotate("rect", xmin = .225, xmax = 0.3, ymin = 1.4, ymax = 1.8, fill="white", colour="black") +
    annotate("text", x=.267, y=1.7, label = "-4.64(x) - 4.64") + 
    annotate("text", x=.267, y=1.6, label = "R^2 == 0.445", parse=T) + 
    annotate("text", x=.267, y=1.5, label = "alpha == 0.0349", parse=T) +
    theme_bw() + 
    theme(legend.position="none") + 
    theme(text = element_text(size = 20))

          
grid.arrange(plot0, plot1, plot2, ncol=3)



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

