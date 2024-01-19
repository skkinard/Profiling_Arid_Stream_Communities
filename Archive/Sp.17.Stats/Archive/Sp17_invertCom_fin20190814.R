# Last update: August 2019
# Subject: Spring 2017 Benthic Invertebrate Community analysis along Precipitation Gradient in South Texas
# Analyst: Sean Kinard
########################################
# Install Packages
install.packages("nlme")
install.packages("multcomp")
install.packages("bio.infer")
install.packages("vegan")
install.packages("colorRamps")
install.packages("factoextra")
install.packages("cluster")

# Set Work Directory
setwd("C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats")

# Load Packages
# Load Packages
library(nlme)
library(multcomp)
library(bio.infer)
library(vegan)
library(cluster)

# Sean's PC loading packages
library("multcomp", lib.loc="~/R/win-library/3.5")
library("nlme", lib.loc="~/R/win-library/3.5")
library("bio.infer", lib.loc="~/R/win-library/3.5")
library("vegan", lib.loc="~/R/win-library/3.5")

# Import Data Filesinvert <- read.csv("Electroinverting.Survey.Spring2017.csv", fileEncoding="UTF-8-BOM")
invert <- read.csv("Kicknet.Survey.Spring2017.csv", fileEncoding="UTF-8-BOM")
gauge.data <- read.csv("Sp17_gaugesconcotenated.csv", fileEncoding="UTF-8-BOM")
habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")

# Generate Diversity and evenness metrics
invert$shannon <- diversity(invert[,8:111])
invert$simpson <- diversity(invert[,8:111], index ="simpson")
invert$rich <- specnumber(invert[,8:111])

(raremax <- min(rowSums(invert[,8:111]))) # rarefaction is unclear here. What exactly is happening here?
invert$rare.rich <- rarefy(invert[,8:111], raremax)

# Intro Plots invert
msterinvert <- merge(invert[,-c(8:111)], habitat[,-31], by = "USGS.gauge") # merge stream habitat data and diversity index data for invert
factor(gauge.data$USGS.gauge)


msterinvert <- merge(msterinvert, gauge.data, by = "USGS.gauge") # merge USGS, stream hab, and div. index data frames
colnames(msterinvert)
row.names(msterinvert) <- msterinvert$USGS.gauge
msterinvert <- Filter(is.numeric, msterinvert)

# Shannon 
par(mfrow=c(2,2))
plot(shannon ~ PPTAVG_SITE, data=msterinvert, main="Shannon")
abline(lm(shannon ~ PPTAVG_SITE, data=msterinvert))
summary(lm(shannon ~ PPTAVG_SITE, data=msterinvert))
plot(simpson ~ PPTAVG_SITE, data=msterinvert, main="Simpson")
abline(lm(simpson ~ PPTAVG_SITE, data=msterinvert))
summary(lm(simpson ~ PPTAVG_SITE, data=msterinvert))
plot(PIE ~ PPTAVG_SITE, data=msterinvert, main="PIE")
abline(lm(PIE ~ PPTAVG_SITE, data=msterinvert))
summary(lm(PIE ~ PPTAVG_SITE, data=msterinvert))
plot(rich ~ PPTAVG_SITE, data=msterinvert, main="rich")
abline(lm(rich ~ PPTAVG_SITE, data=msterinvert))
summary(lm(rich ~ PPTAVG_SITE, data=msterinvert))


# rare.rich - stronger
par(mfrow=c(1,2))
plot(rare.rich ~ PPTAVG_SITE, data=msterinvert, main="rare.rich")
abline(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert))
summary(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert))
plot(abun ~ PPTAVG_SITE, data=msterinvert, main="abun")
abline(lm(abun ~ PPTAVG_SITE, data=msterinvert))
summary(lm(abun ~ PPTAVG_SITE, data=msterinvert))

# These plots indicate little correlation between diversity metrics and climate gradient.
# there appears to be an outlier (8212300=TRC) messing with several regressions
# Shannon and Simpson appear to have potential polynomial relation to ppt

# Here, I run side-by-side regressions with and without TRC (an apparent outlier)

msterinvert.not <- msterinvert[-13,]

# Shannon - still poor relation
par(mfrow=c(1,2))
plot(shannon ~ PPTAVG_SITE, data=msterinvert, main="All 13 sites")
abline(lm(shannon ~ PPTAVG_SITE, data=msterinvert))
summary(lm(shannon ~ PPTAVG_SITE, data=msterinvert))
plot(shannon ~ PPTAVG_SITE , data = msterinvert.not, main="TRC removed")
abline(lm(shannon ~ PPTAVG_SITE, data=msterinvert.not))
summary(lm(shannon ~ PPTAVG_SITE, data=msterinvert.not))

# Simpson - lower p val but still poor correlation
par(mfrow=c(1,2))
plot(simpson ~ PPTAVG_SITE, data=msterinvert, main="All 13 sites")
abline(lm(simpson ~ PPTAVG_SITE, data=msterinvert))
summary(lm(simpson ~ PPTAVG_SITE, data=msterinvert))
plot(simpson ~ PPTAVG_SITE , data = msterinvert.not, main="TRC removed")
abline(lm(simpson ~ PPTAVG_SITE, data=msterinvert.not))
summary(lm(simpson ~ PPTAVG_SITE, data=msterinvert.not))

# rich - lower p val but still poor
par(mfrow=c(1,2))
plot(rich ~ PPTAVG_SITE, data=msterinvert, main="All 13 sites")
abline(lm(rich ~ PPTAVG_SITE, data=msterinvert))
summary(lm(rich ~ PPTAVG_SITE, data=msterinvert))
plot(rich ~ PPTAVG_SITE , data = msterinvert.not, main="TRC removed")
abline(lm(rich ~ PPTAVG_SITE, data=msterinvert.not))
summary(lm(rich ~ PPTAVG_SITE, data=msterinvert.not))

# rare.rich - Lower p val still not sign
par(mfrow=c(1,2))
plot(rare.rich ~ PPTAVG_SITE, data=msterinvert, main="All 13 sites")
abline(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert))
summary(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert))
plot(rare.rich ~ PPTAVG_SITE , data = msterinvert.not, main="TRC removed")
abline(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert.not))
summary(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert.not))

# Simple regressions against climate: Not significant

# PCA INVERT - using msterinvert.not since TRC is an apparent outlier
library(factoextra)
msterinvert <- msterinvert.not
Sample <- data.frame(msterinvert)
Sample.scaled <- data.frame(apply(Sample, 2, scale))
Sample.scaled.2 <- data.frame(t(na.omit(t(Sample.scaled))))
res.pca <- prcomp(Sample.scaled.2, retx=TRUE)

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(res.pca)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# This plot indicates climate data from gauges is influential on PC axis 1. Consider trimming # of variables and rerunning PCA?

#Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Access to the PCA results
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# var.contrib. The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
# Helper function 
#::::::::::::::::::::::::::::::::::::::::
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])
var.coord <- as.data.frame(var.coord[,1:4])
sort.PC1 <- var.coord[order(var.coord$PC1),]
sort.PC1 # PRECIP_SEAS_IND, T_AVG_BASIN, PET, PPTAVG_BASIN, NLCD01_06_DEV, IMPNLCD06
sort.PC2 <- var.coord[order(var.coord$PC2),]
sort.PC2 # CDL_ALL_OTHER_LAND, WDMAX_BASIN, NITR_APP_KG_SQKM, PESTAPP_KG_SQKM, RIP100_82, turbidity.adjusted, NO3.NO2, 
sort.PC3 <- var.coord[order(var.coord$PC3),]
sort.PC3 # HYDRO_DISTURB_INDX, pH
sort.PC4 <- var.coord[order(var.coord$PC4),]
sort.PC4 # dissolved.oxygen

# I just noticed sorting PCA var.coord contributions produces positive and negative values. It appears that precip and temperature are inversely colinear.

# Create trimmed data.frame using variables from PCA
trm.invert <- msterinvert[,c("rich", "shannon", "PPTAVG_SITE", "T_AVG_BASIN", "PRECIP_SEAS_IND", "PET", "NLCD01_06_DEV", "IMPNLCD06", "CDL_ALL_OTHER_LAND", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PESTAPP_KG_SQKM","RIP100_82", "turbidity.adjusted", "conductivity", "salinity", "dissolved.oxygen", "pH", "NH3.N", "PO43.", "NO3.NO2", "HYDRO_DISTURB_INDX")]
head(trm.invert) #Remember that rownames are the USGS gauges

# Proceed to aug.pairs, glm, and NMDS
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

aug.pairs(trm.invert, na.action=na.omit)
# Identified correlation > abs(0.4): "WD_SITE", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PHOS_APP_KG_SQKM", "HYDRO_DISTURB_INDX", "dissolved.oxygen", "PO43."

# Identified need to log transform: 
# RIP100_43
# NITR_APP_KG_SQKM
# NO3.NO2
# conductivity
# PO43.
trm.invert$RIP100_82 <- c(0.000001, 0.000001,15.71,2.59,44.92,0.000001,0.10,0.27,0.77,10.24,62.03,2.73) # Preventing undefined values by substituting 0.000001 for 0.
trm.invert$RIP100_82 <- log10(trm.invert$RIP100_82)
trm.invert$NITR_APP_KG_SQKM <- log10(trm.invert$NITR_APP_KG_SQKM)
trm.invert$conductivity <- log10(trm.invert$conductivity)
trm.invert$NH3.N <- log10(trm.invert$NH3.N)
colnames(trm.invert)
# NO3.NO2 contains negative values which do not log transform. Substitute LR nitrate values for NO3NO2. LR Nitrate data contained NAs and was removed

# Evaluate log transformations
aug.pairs(trm.invert)

# Log transformations improve linear regressions. Replace true variables with log transformed and proceed to glms.
colnames(trm.invert)
trm3.invert <- trm.invert[,-c(4,5,6,8,9,12,14,17,18,19,15)] # removing variables with excessive colinearity or low correlation with diversity metrics
aug.pairs(trm3.invert)

# PIE = Probability of interspecies encounter
full.model.invert <- lm(PIE~ WD_SITE + PHOS_APP_KG_SQKM + HYDRO_DISTURB_INDX + turbidity.adjusted + dissolved.oxygen, data = trm3.invert)
summary(full.model.invert)
vif(full.model.invert) 
# A rule of thumb for interpreting the variance inflation factor:
# 1 = not correlated.
# Between 1 and 5 = moderately correlated.
# Greater than 5 = highly correlated.

# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)  
# (Intercept)          -0.3230099  0.1870511  -1.727   0.1349  
# WD_SITE               0.0046950  0.0018826   2.494   0.0469 *
# PHOS_APP_KG_SQKM      0.1547421  0.0585315   2.644   0.0383 *
# HYDRO_DISTURB_INDX   -0.0012588  0.0019592  -0.643   0.5443  
# turbidity.adjusted    0.0002908  0.0016245   0.179   0.8638  
# dissolved.oxygen     -0.0035302  0.0013069  -2.701   0.0355 *
#
# Residual standard error: 0.04721 on 6 degrees of freedom
# Multiple R-squared:  0.8125,	Adjusted R-squared:  0.6563 
# F-statistic:   5.2 on 5 and 6 DF,  p-value: 0.03448

min.model<-lm(PIE~1,data=trm3.invert)
forward.model <- traditional.forward(min.model,trm3.invert[,4:8])
summary(forward.model)

backward.model <- traditional.backward(full.model.invert)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)   
#  (Intercept)       -0.336739   0.166174  -2.026  0.07729 . 
#   WD_SITE           0.004584   0.001527   3.002  0.01702 * 
#   PHOS_APP_KG_SQKM  0.169774   0.041063   4.134  0.00328 **
#   dissolved.oxygen -0.003810   0.001102  -3.458  0.00860 **
# 
# Residual standard error: 0.04226 on 8 degrees of freedom
# Multiple R-squared:  0.7996,	Adjusted R-squared:  0.7245 
# F-statistic: 10.64 on 3 and 8 DF,  p-value: 0.003636

options(na.action = "na.fail")
dredge(full.model.invert)
options(na.action = "na.omit")

# Summary of PIE glms:  
# Climate: WD_SITE = Site average of annual number of days (days) of measurable precipitation, derived from 30 years of record (1961-1990), 2km PRISM.
# Land Use: Phos_APP = Estimate of phosphorus from fertilizer and manure, from Census of Ag 1997, based on county-wide sales and percent agricultural land cover in watershed, kg/sq km
# water quality: dissolved oxygen

# shannon
full.model.invert <- lm(shannon~ WD_SITE + PHOS_APP_KG_SQKM + HYDRO_DISTURB_INDX + turbidity.adjusted + dissolved.oxygen, data = trm3.invert)
summary(full.model.invert)
vif(full.model.invert)
# Residual standard error: 0.2959 on 6 degrees of freedom
# Multiple R-squared:  0.6634,	Adjusted R-squared:  0.3829 
# F-statistic: 2.365 on 5 and 6 DF,  p-value: 0.1622

min.model<-lm(shannon~1,data=trm3.invert)
forward.model <- traditional.forward(min.model,trm3.invert[,-c(1:3)])
summary(forward.model)

backward.model <- traditional.backward(full.model.invert)
summary(backward.model)
vif(backward.model)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       4.752486   1.020605   4.657  0.00163 **
#   WD_SITE          -0.022323   0.009378  -2.380  0.04452 * 
#   PHOS_APP_KG_SQKM -0.726437   0.252201  -2.880  0.02050 * 
#  dissolved.oxygen  0.013271   0.006768   1.961  0.08554 . 
#
# Residual standard error: 0.2596 on 8 degrees of freedom
# Multiple R-squared:  0.6546,	Adjusted R-squared:  0.5251 
# F-statistic: 5.054 on 3 and 8 DF,  p-value: 0.02977

options(na.action = "na.fail")
dredge(full.model.invert)
options(na.action = "na.omit")

# Summary Shannon glms: similar to PIE (DO, Phos.APP, WD_SITE)

# Richness
full.model.invert <- lm(rich~ WD_SITE + PHOS_APP_KG_SQKM + HYDRO_DISTURB_INDX + turbidity.adjusted + dissolved.oxygen, data = trm3.invert)
summary(full.model.invert)
vif(full.model.invert)

min.model<-lm(rich~1,data=trm3.invert)
forward.model <- traditional.forward(min.model,trm3.invert[,-c(1:3)])
summary(forward.model)

backward.model <- traditional.backward(full.model.invert)
summary(backward.model)
vif(backward.model)

options(na.action = "na.fail")
dredge(full.model.invert)
options(na.action = "na.omit")

invert.model.17 <- lm(rich~ HYDRO_DISTURB_INDX + PHOS_APP_KG_SQKM, data=trm3.invert)
summary(invert.model.17)
vif(invert.model.17)
# Residual standard error: 5.579 on 9 degrees of freedom
# Multiple R-squared:  0.3831,	Adjusted R-squared:  0.246 
# F-statistic: 2.794 on 2 and 9 DF,  p-value: 0.1138

# Summary Richness: not significant

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INVERT glm conclusions:
# variation in PIE, SI, richness among the sites in the gradient is  explained by 3 disparate paramters # days rain, fertilizer application, and dissolved oxygen levels. Keep in mind TRC is not included in this multivariate regression analysis

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Inverts

trm3.invert$USGS.gauge <- rownames(trm3.invert)
ford <- merge(fmat[,c(1:90)],trm3.invert, by= "USGS.gauge")
ford <- ford[order(ford$WD_SITE),]
nms1 <- metaMDS(ford[,c(2:90)], k=2, noshare=0.2)
plot(nms1)
plot(nms1, type="n")
points(nms1, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(nms1, display= "spec", cex=0.7, col="blue")
# Lets try it with 13/13 by including TRC again

# Reset 13/13 sites
msterinvert <- merge(fmat[,-c(2:90)], habitat[,-31], by = "USGS.gauge") 
factor(gauge.data$USGS.gauge)
msterinvert <- merge(msterinvert, gauge.data, by = "USGS.gauge")
row.names(msterinvert) <- msterinvert$USGS.gauge
msterinvert <- Filter(is.numeric, msterinvert)
trm.invert <- msterinvert[,c("PIE", "rich", "shannon", "PPTAVG_SITE", "WD_BASIN", "RIP100_43", "WD_SITE", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PHOS_APP_KG_SQKM", "HYDRO_DISTURB_INDX", "RH_BASIN", "turbidity.adjusted", "conductivity", "salinity", "dissolved.oxygen", "pH", "NH3.N", "PO43.")]
trm.invert$RIP100_43 <- c(8.64, 5.90, 0.42, 0.36, 0.02, 0.23, 0.000001, 0.18, 0.09, 0.01, 0.000001, 0.38, 0.38) # Preventing undefined values by substituting 0.000001 for 0.
trm.invert$RIP100_43 <- log10(trm.invert$RIP100_43)
trm.invert$PHOS_APP_KG_SQKM <- log10(trm.invert$PHOS_APP_KG_SQKM)
trm.invert$conductivity <- log10(trm.invert$conductivity)
trm.invert$PO43. <- log10(trm.invert$PO43.)
trm.invert$NH3.N <- log10(trm.invert$NH3.N)
trm3.invert <- trm.invert[,-c(4,5,6,8,9,12,14,17,18,19,15)]

# Rerun NMDS
trm3.invert$USGS.gauge <- rownames(trm3.invert)
ford <- merge(fmat[,c(1:90)],trm3.invert, by= "USGS.gauge")
ford <- ford[order(ford$WD_SITE),]
nms1 <- metaMDS(ford[,c(2:90)], k=2, noshare=0.1)
plot(nms1)
plot(nms1, type="n")
points(nms1, display = "sites", cex = 1.5, pch=21, col="red", bg="yellow")
# text(nms1, display= "spec", cex=0.7, col="blue")
# This may be workable. There are too many species to see clearly what is going on here.

# Color Gradient to Reveal groups?
par(mfrow=c(2,2))
rbPal <- colorRampPalette(c('yellow','blue')) #Create a function to generate a continuous color palette
ford$Col <- rbPal(10)[as.numeric(cut(ford$WD_SITE,breaks = 10))]#This adds a column of color values based on the y values
plot(nms1, type="n", main= "invert col. WD_SITE")
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)

ford <- ford[order(ford$HYDRO_DISTURB_INDX),]
ford$Col <- rbPal(10)[as.numeric(cut(ford$HYDRO_DISTURB_INDX,breaks = 10))]#This adds a column of color values based on the y values
plot(nms1, type="n", main= "invert col. H-D-indx")
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)

ford <- ford[order(ford$dissolved.oxygen),]
ford$Col <- rbPal(10)[as.numeric(cut(ford$dissolved.oxygen,breaks = 10))]#This adds a column of color values based on the y values
plot(nms1, type="n", main= "invert col. D.O")
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)

ford <- ford[order(ford$PHOS_APP_KG_SQKM),]
ford$Col <- rbPal(10)[as.numeric(cut(ford$PHOS_APP_KG_SQKM,breaks = 10))]#This adds a column of color values based on the y values
plot(nms1, type="n", main= "invert col. PO4.App")
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)

# assign groups by WD
par(mfrow=c(1,1))
ford <- ford[order(ford$WD_SITE),]
ford$groupWD <- c("<70","<70","<70","<70","<70","<70",">70",">70",">70",">70",">70",">70",">70")
ford$Col <- rbPal(10)[as.numeric(cut(ford$WD_SITE,breaks = 10))]
par(mfrow=c(1,1))
plot(nms1, disp = "sites", type = "n", main="Inv gr. by WD", ylim=c(-.7,.7), xlim=c(-.7,.7)) # Why won't my x and y axis adjust? Is it because my aspect ratio is locked? How do I change aspect ratio?
ordiellipse(nms1, groups = ford$groupWD, draw="polygon", border = FALSE, kind = "ehull", col=c("yellow", "blue"), alpha=.2)
# ordispider(nms1, groups = ford$groupWD, col=c("chocolate", "blue"), label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
# ordihull(nms1, groups = ford$groupWD, lwd = 1)

text(nms1, display = "spec", cex=0.4, col="black")

# There is too much overlap to differentiate based on WD_SITE

# assign groups by Hydro_DIST_INDX
par(mfrow=c(1,1))
ford <- ford[order(ford$HYDRO_DISTURB_INDX),]
ford$groupHYD <- c("<10","<10","<10","<20","<20","<20","<20","<20",">21",">21",">21",">21",">21")
ford$Col <- rbPal(10)[as.numeric(cut(ford$HYDRO_DISTURB_INDX,breaks = 10))]
par(mfrow=c(1,1))
plot(nms1, disp = "sites", type = "n", main="Inv gr. by HYD-D-INDX", ylim=c(-.7,.7), xlim=c(-.7,.7)) # Why won't my x and y axis adjust? Is it because my aspect ratio is locked? How do I change aspect ratio?
ordiellipse(nms1, groups = ford$groupHYD, draw="polygon", border = FALSE, kind = "ehull", col=c("yellow", "purple", "blue"), alpha=.2)
#ordispider(nms1, groups = ford$groupHYD, col=c("red", "purple", "blue"), label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
# ordihull(nms1, groups = ford$groupHYD, lwd = 1)

text(nms1, display = "spec", cex=0.4, col="black")


# Fit environmental Variables
colnames(ford)
ord2 <- decorana(ford[2:90])
ord.fit <- envfit(ord2 ~ WD_SITE + dissolved.oxygen + HYDRO_DISTURB_INDX + PHOS_APP_KG_SQKM + turbidity.adjusted + PPTAVG_SITE, data=ford, perm=999, na.rm=TRUE)
ord.fit
ford$Col <- rbPal(10)[as.numeric(cut(ford$WD_SITE,breaks = 10))]
# ::::::::::::::::::::::::::::::::::::::::::::::::
plot(nms1, disp = "sites", type = "n", main="Inv Ordination with Env. var. fit", ylim=c(-.7,.7), xlim=c(-.7,.7))
#ordispider(nms1, groups = ford$groupPPT, col="black", label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
#ordihull(nms1, groups = ford$groupPPT, lwd = 1)
ordiellipse(nms1, groups = ford$groupPPT, draw="polygon", kind = "ehull", border = NA, col="grey", alpha=.25)
plot(ord.fit, col="red", cex=0.7)
# ::::::::::::::::::::::::::::::::::::::::::::::::
# Final Inv. figure with fitted env does not reveal much.
# Now I wonder how I got the plot I did in my poster analysis. This community matrix is not stratified in 2-D space. Could this be due to relaxed nmds via noshare=.2 ?
# The invertebrate data is unclear. Where did I go wrong?

# Closing Coding Comments:
# Although the invertebrate data contains more entries, the conclusions are unclear since the ordinations cannot be clustered by evident environmental gradients or a-priori group assignments. The multivariate linear regreession analysis reveals potential impacts of climate, land use, and water quality on macroinvertebrate community diversity. The ordination with fitted environmental variables loosely suggests a role of PPT_AVG in driving some of the observered differences in m.invert community compositions.
