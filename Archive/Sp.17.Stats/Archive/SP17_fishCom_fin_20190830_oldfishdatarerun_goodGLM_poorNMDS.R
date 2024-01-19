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
fmat <- read.csv("Sp17_electrofishingsurvey_originalposterdata.csv", fileEncoding="UTF-8-BOM")
gauge.data <- read.csv("Sp17_gaugesconcotenated.csv", fileEncoding="UTF-8-BOM")
habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")

# Generate Diversity and evenness metrics
fmat$shannon <- diversity(fmat[,3:24])
fmat$simpson <- diversity(fmat[,3:24], index = "simpson")
fmat$rich <- specnumber(fmat[,3:24])
(raremax <- min(rowSums(fmat[,3:24]))) # rarefaction is unclear here. What exactly is happening here?
fmat$rare.rich <- rarefy(fmat[,3:24], raremax)
fmat$abun <- rowSums(fmat[,3:24])


# Merge data.frames
msterfish <- merge(fmat[,-c(3:24)], habitat[,-31], by = "USGS.gauge")
msterfish <- merge(msterfish, gauge.data, by = "USGS.gauge")
row.names(msterfish) <- msterfish$USGS.gauge
msterfish <- Filter(is.numeric, msterfish)

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
trm.fish <- msterfish[,c("USGS.gauge", "rare.rich", "shannon", "PPTAVG_BASIN", "PRECIP_SEAS_IND", "IMPNLCD06", "RIP100_43", "RIP100_PLANT", "NH3.N", "conductivity", "dissolved.oxygen", "PO43.", "depth.max", "width.depth")]

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
trm.fish$L.IMPNLCD06 <- log10(trm.fish$IMPNLCD06+.000001)
trm.fish$L.RIP100_43 <- log10(trm.fish$RIP100_43+.000001)
trm.fish$L.conductivity <- log10(trm.fish$conductivity+.000001)
trm.fish$L.PO43. <- log10(trm.fish$PO43.+.000001)
trm.fish <- trm.fish[,-c(6,7,10,12)]


# Evaluate log transformations
aug.pairs(trm.fish[,-1], na.action=na.omit)
trm3.fish <- trm.fish[,-c(5,6,8,9,11,14)]
aug.pairs(trm3.fish[,-1])

# shannon multivariate regressions
full.model.trm3.fish <- lm(shannon~ PPTAVG_BASIN+NH3.N+width.depth+L.RIP100_43+L.conductivity, data = trm3.fish)
summary(full.model.trm3.fish)
vif(full.model.trm3.fish)

b.model.trm <- traditional.backward(full.model.trm3.fish)
summary(b.model.trm)
vif(b.model.trm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.221599   0.336198   0.659  0.52832   
# PPTAVG_BASIN 0.012188   0.003561   3.422  0.00906 **
# Residual standard error: 0.2141 on 8 degrees of freedom
# Multiple R-squared:  0.5942,	Adjusted R-squared:  0.5434 
# F-statistic: 11.71 on 1 and 8 DF,  p-value: 0.009056

options(na.action = "na.fail")
dredge(full.model.trm3.fish)
options(na.action = "na.omit")
# PPT is a positive correlate with SI
# Log.conductivity is a negative correlate with SI.
# Interpretation: Precipitation drives greater species diversity and evenness. Conductivity is likely an inverse covariate of precipitation through evaporation/evapotranspiration.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Rarified.Richness multivariate regressions
f.mod.trm <- lm(rare.rich~ PPTAVG_BASIN+NH3.N+width.depth+L.RIP100_43+L.conductivity, data = trm3.fish)
summary(f.mod.trm)
vif(f.mod.trm)

b.model.trm <- traditional.backward(f.mod.trm)
summary(backward.model)
vif(backward.model)

options(na.action = "na.fail")
dredge(f.mod.trm)
options(na.action = "na.omit")
# Same as SI

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Fish
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(2:24)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_BASIN),]
nms1 <- metaMDS(ford[,c(2:23)], noshare=0.2,k=2)

# heirarchical clustering identifies groupings of sites
clusters <- hclust(dist(ford[,c(24:30)]), method="median")
plot(clusters)
clusterCut <- cutree(clusters, 3)
table(clusterCut, ford$PPTAVG_BASIN)
ford$clusterCut <- clusterCut

# Shade with Color Gradient based on precipitation
rbPal <- colorRampPalette(c('yellow','blue')) 
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_BASIN,breaks = 10))]#This adds a column of color values based on the y values

# Fit Environmental variables to nmds
ord2 <- decorana(ford[2:26])
ord.fit <- envfit(ord2 ~ PPTAVG_BASIN+NH3.N+width.depth+L.RIP100_43+L.conductivity, data=ford, perm=999, na.rm=TRUE)

# Plot
par(mfrow=c(1,1))
plot(nms1, type="n", main= "nmds.envfit.fish")
ordispider(nms1, groups = ford$clusterCut, draw="polygon", kind = "ehull", border = NA, alpha=.25)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
plot(ord.fit, col="red", cex=01.0)
# text(nms1, display = "spec", cex=0.4, col="black")

# ::::::::::::::::::::::::::::::::::::::::::::::::
# Running this analysis with older data that didn't go through QaQc produces a stronger linear analysis within the narrative of ppt driven effects on diversity. However, the NMDS does not support a gradient driven change in composition as the 13 site recent analysis provides.

# My conclusion is to run with the new 13 site analysis. It provides a clear visual of composition shits along the gradient. The linear regression analysis is weaker due to poor sample sizes at some sites which reflect poor diversity indices. Diversity isn't as important as composition in my opinion though. 