# Last update: August 14 2019
# Subject: Spring 2017 Fish Community analysis along Precipitation Gradient in South Texas
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
library(nlme)
library(multcomp)
library(vegan)
library(cluster)

# Sean's PC loading packages
library("multcomp", lib.loc="~/R/win-library/3.5")
library("nlme", lib.loc="~/R/win-library/3.5")
library("vegan", lib.loc="~/R/win-library/3.5")

# Import Data Files
fmat <- read.csv("Electrofishing.Survey.Spring2017.csv", fileEncoding="UTF-8-BOM")
gauge.data <- read.csv("Sp17_gaugesconcotenated.csv", fileEncoding="UTF-8-BOM")
habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")

# convert the original dataframe into a site x species matrix
fmat <- fmat[,c(3,15,16)] # USGS.gauge, Genus.Species, Abundance
fmat <- makess(fmat, tname="genus.species")

# Generate Diversity and evenness metrics
fmat$shannon <- diversity(fmat[,2:23])
fmat$simpson <- diversity(fmat[,2:23], index = "simpson")
fmat$rich <- specnumber(fmat[,2:23])
(raremax <- min(rowSums(fmat[,2:23]))) # rarefaction is unclear here. What exactly is happening here?
fmat$rare.rich <- rarefy(fmat[,2:23], raremax)
fmat$abun <- rowSums(fmat[,2:23])

# Merge data.frames
msterfish <- merge(fmat[,-c(2:23)], habitat[,-31], by = "USGS.gauge")
msterfish <- merge(msterfish, gauge.data, by = "USGS.gauge")
row.names(msterfish) <- msterfish$USGS.gauge
msterfish <- Filter(is.numeric, msterfish)
colnames(msterfish)
# remove OSO and CC due to pollution and compromised sample effort. These sites do not reflect similar local conditions to our other 1st order creeks.
# OSO 08211520
# CC  08189200
msterfish <- msterfish[-c(7,11),]
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Trim variables using PCA and investigator discretion
# PCA FISH - using msterfish.not which has n<10 removed
library(factoextra)
Sample <- data.frame(msterfish)
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
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates

loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])
var.coord <- as.data.frame(var.coord[,1:4])
sort.PC1 <- var.coord[order(var.coord$PC1),]
sort.PC1 # T_AVG_BASIN, PET, PRECIP_SEAS_IND, PPTAVG_BASIN, NLCD01_06_DEV, IMPNLCD06, RIP100_43
sort.PC2 <- var.coord[order(var.coord$PC2),]
sort.PC2 # NITR_APP_KG_SQKM, RIP100_PLANT, PESTAPP_KG_SQKM
sort.PC3 <- var.coord[order(var.coord$PC3),]
sort.PC3 # HYDRO_DISTURB_INDX, NH3.N
sort.PC4 <- var.coord[order(var.coord$PC4),]
sort.PC4 # turbidity.adjusted, NO3.NO2

# Create trimmed data.frame using variables from PCA
msterfish$USGS.gauge <- rownames(msterfish)
trm.fish <- msterfish[,c("USGS.gauge", "rare.rich", "shannon", "PPTAVG_BASIN", "PRECIP_SEAS_IND", "IMPNLCD06", "RIP100_43", "RIP100_PLANT", "NH3.N", "conductivity", "dissolved.oxygen", "PO43.", "width.depth")]

# Investigate Diversity ~ Precipitation
par(mfrow=c(1,2))
plot(shannon ~ PPTAVG_BASIN, data=trm.fish)
abline(lm(shannon ~ PPTAVG_BASIN, trm.fish))
summary(lm(shannon ~ PPTAVG_BASIN, trm.fish))
plot(rare.rich ~ PPTAVG_BASIN, data=trm.fish)
abline(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))
summary(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))

# Proceed to aug.pairs, glm, and NMDS
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

aug.pairs(trm.fish[,-1], na.action=na.omit)

# Identified need to log transform: 
trm.fish$RIP100_43 <- trm.fish$RIP100_43+.0000001
trm.fish$RIP100_43 <- log10(trm.fish$RIP100_43)
trm.fish$conductivity <- log10(trm.fish$conductivity)

# Evaluate log transformations
aug.pairs(trm.fish[,-1], na.action=na.omit)
trm3.fish <- trm.fish[,-c(5,6,7,10,11)]
aug.pairs(trm3.fish[,-1])

# shannon multivariate regressions
full.model.trm3.fish <- lm(shannon~ PPTAVG_BASIN+RIP100_PLANT+NH3.N+PO43.+width.depth, data = trm3.fish)
summary(full.model.trm3.fish)
vif(full.model.trm3.fish)

b.model.trm <- traditional.backward(full.model.trm3.fish)
summary(b.model.trm)
vif(b.model.trm)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  2.25451    0.53651   4.202  0.00299 **
#   PO43.       -0.21637    0.08025  -2.696  0.02724 * 
#   width.depth -0.06045    0.03190  -1.895  0.09469 . 
# Residual standard error: 0.3918 on 8 degrees of freedom
# Multiple R-squared:  0.5308,	Adjusted R-squared:  0.4135 
# F-statistic: 4.525 on 2 and 8 DF,  p-value: 0.04848

options(na.action = "na.fail")
dredge(full.model.trm3.fish)
options(na.action = "na.omit")
# The top ten (ranked by AICc) models in dredge indicate that phosphates, ammonia, and width.depth ratio are negative correlates of shannon diversity, but precipitation is a positive correlate with shannon diversity. Is the connection mitigated through natural biogeochemical processing (precipitationRegime->soilchemistry->runoff) or is it anthropogenically driven (crops/pasture->runoff)?

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Rarified.Richness multivariate regressions
f.mod.trm <- lm(rare.rich~ PPTAVG_BASIN +RIP100_PLANT+NH3.N+PO43.+width.depth, data = trm3.fish)
summary(f.mod.trm)
vif(f.mod.trm)
# Residual standard error: 0.2551 on 4 degrees of freedom
# Multiple R-squared:  0.7655,	Adjusted R-squared:  0.2966 
# F-statistic: 1.633 on 8 and 4 DF,  p-value: 0.3344

backward.model <- traditional.backward(f.mod.trm)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.73820    0.08225   21.13 5.58e-09 ***
#   PO43.       -0.11479    0.04130   -2.78   0.0214 
# Residual standard error: 0.2067 on 9 degrees of freedom
# Multiple R-squared:  0.462,	Adjusted R-squared:  0.4022 
F-statistic: 7.727 on 1 and 9 DF,  p-value: 0.02141

options(na.action = "na.fail")
dredge(f.mod.trm)
options(na.action = "na.omit")
# The top ten ranked models from dredge implicate phosphate, ammonia, and agricultural land-use within the 100m riparian buffer zone are negative correlates with rarified richness.
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Fish
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:23)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_BASIN),]
# ford <- ford[,-c(6,7,20)] # had to remove columns with only zeros. These came from sites that were removed because of n <10.
nms1 <- metaMDS(ford[,c(2:23)], noshare=0.2,k=2)

# heirarchical clustering identifies groupings of sites
clusters <- hclust(dist(ford[,c(27:38)]), method="median")
plot(clusters)
clusterCut <- cutree(clusters, 3)
table(clusterCut, ford$PPTAVG_BASIN)
ford$clusterCut <- clusterCut

# Shade with Color Gradient based on precipitation
rbPal <- colorRampPalette(c('yellow','blue')) 
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_BASIN,breaks = 10))]#This adds a column of color values based on the y values

# Fit Environmental variables to nmds
ord2 <- decorana(ford[2:26])
ord.fit <- envfit(ord2 ~ PPTAVG_BASIN + PRECIP_SEAS_IND+RIP100_PLANT+PESTAPP_KG_SQKM+NH3.N+pH+turbidity.adjusted+PO43., data=ford, perm=999, na.rm=TRUE)

# Plot
par(mfrow=c(1,1))
plot(nms1, type="n", main= "nmds.envfit.fish")
ordiellipse(nms1, groups = ford$clusterCut, draw="polygon", kind = "ehull", border = NA, col=c("yellow", "blue"), alpha=.25)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
plot(ord.fit, col="red", cex=01.0)
# text(nms1, display = "spec", cex=0.4, col="black")

# ::::::::::::::::::::::::::::::::::::::::::::::::
# Final figure entails: remove env variable labels and replace in paint or ppt.
# axis recommendations?
# Title recommendation?
# Any species labels? too much clutter?
# which to remove: NH3N or pH in plot to simplify (they are colinear)?

# This concludes the fish community analysis of 13 streams along the South Texas Coastal Prairie. In it, 103 environmental variables were evaluated as predictors of variation in 5 different fish community diversity metrics. I used PCA to identify influential variables and trim the data set down to <20 variables to conduct multivariate linear regressions. Finally, I used NMDS to reveal clusters of fish community composition in conjunction with the climate variable (annual precipiation). Environmental variables were fit to the NMDS and species labels added for interprettations and conclusions.

# Closing Coding Comments:
# This analysis involved several reboots. First, I ran PCA and glm on 13 sites using bio.infer at the start. Then, I realized the correlations and nmds were not as clear as they had been during my preliminary data analysis. The second go involved bypassing the bio.infer OTU assignment code. This produced double the species and produced stronger R and p-vals, but again seemed weak for simple linear regressions. So, the third and final script involved removing sites with fewer than 10 sample size for the PCA, linear regression, and mulivariate glm analysis. Ulitmately, each script improved R and p-vals, simplified results, leading to simpler conclusions. For the NMDS plots at the end for visualization of these conclusions, I reset the community data to include 13 of 13 sites. This results in 2 distinct clusters of fish community composition. Arid streams contain G.affinis, P.formosa, and C. variegatus. These communities are evidently constrained by environmental variables: PPT, Hydro-Dist, and ammonia concentrations (colinear with pH). 