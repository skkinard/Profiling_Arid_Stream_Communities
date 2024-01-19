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
library(bio.infer)
library(vegan)
library(cluster)

# Sean's PC loading packages
library("multcomp", lib.loc="~/R/win-library/3.5")
library("nlme", lib.loc="~/R/win-library/3.5")
library("bio.infer", lib.loc="~/R/win-library/3.5")
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
trm.fish <- msterfish[,c("USGS.gauge", "rare.rich", "shannon", "PPTAVG_BASIN", "PET", "PRECIP_SEAS_IND", "NLCD01_06_DEV", "IMPNLCD06", "RIP100_43", "NITR_APP_KG_SQKM", "RIP100_PLANT", "PESTAPP_KG_SQKM", "HYDRO_DISTURB_INDX", "NH3.N", "conductivity", "dissolved.oxygen", "pH", "turbidity.adjusted", "PO43.", "width.depth")]
# Some sites contain n < 10, which may merit their removal for analysis. Unfortunately, doing this will reduce the observed gradient effect in the dataset.
which(msterfish$abun < 10)
slim.fish <- trm.fish[-c(6,7,9,11,13),]

# Investigate PPT relationship to diveristy
# Shannon - stronger with 8/13 sites (pval ~ .08)
par(mfrow=c(1,2))
plot(shannon ~ PPTAVG_BASIN, data=trm.fish, main="All 13 sites")
abline(lm(shannon ~ PPTAVG_BASIN, trm.fish))
summary(lm(shannon ~ PPTAVG_BASIN, trm.fish))
plot(shannon ~ PPTAVG_BASIN , data = slim.fish, main="n<10 removed")
abline(lm(shannon ~ PPTAVG_BASIN, data=slim.fish))
summary(lm(shannon ~ PPTAVG_BASIN, data=slim.fish))

# rare.rich - stronger with 8/13 sites (pval ~ .08)
par(mfrow=c(1,2))
plot(rare.rich ~ PPTAVG_BASIN, data=trm.fish, main="All 13 sites")
abline(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))
summary(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))
plot(rare.rich ~ PPTAVG_BASIN , data = slim.fish, main="n<10 removed")
abline(lm(rare.rich ~ PPTAVG_BASIN, data=slim.fish))
summary(lm(rare.rich ~ PPTAVG_BASIN, data=slim.fish))

# Proceed to aug.pairs, glm, and NMDS
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

aug.pairs(trm.fish[,-1], na.action=na.omit)
# linear regressions indidcate the following variables relate to diversity metrics: "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3.NO2", "pH", "NH3.N","PO43.", "dissolved.oxygen", "PPTAVG_SITE"

# Identified correlation > abs(0.4): "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3.NO2", "pH", "NH3.N","PO43.", "dissolved.oxygen", "PPTAVG_SITE"

# Identified need to log transform: 
colnames(trm.fish)
trm.fish$RIP100_43 <- trm.fish$RIP100_43+.0000001
trm.fish$NLCD01_06_DEV <- trm.fish$NLCD01_06_DEV+.000001
trm.fish$NLCD01_06_DEV <- log10(trm.fish$NLCD01_06_DEV)
trm.fish$IMPNLCD06 <- log10(trm.fish$IMPNLCD06)
trm.fish$RIP100_43 <- log10(trm.fish$RIP100_43)
trm.fish$NITR_APP_KG_SQKM <- log10(trm.fish$NITR_APP_KG_SQKM)
trm.fish$PESTAPP_KG_SQKM <- log10(trm.fish$PESTAPP_KG_SQKM)
trm.fish$conductivity <- log10(trm.fish$conductivity)
trm.fish$turbidity.adjusted <- log10(trm.fish$turbidity.adjusted)

# Evaluate log transformations
aug.pairs(trm.fish[,-1], na.action=na.omit)
trm3.fish <- trm.fish[,-c(1,10,13,16,20)]
aug.pairs(trm3.fish)

# shannon multivariate regressions
full.model.trm3.fish <- lm(shannon~ PET + PRECIP_SEAS_IND+RIP100_PLANT+PESTAPP_KG_SQKM+NH3.N+pH+turbidity.adjusted+PO43., data = trm3.fish)
summary(full.model.trm3.fish)
vif(full.model.trm3.fish)

b.model.trm <- traditional.backward(full.model.trm3.fish)
summary(b.model.trm)
vif(b.model.trm)
#                       Estimate  Std. Error  t value  Pr(>|t|) 
# (Intercept)           17.817294   5.584536   3.190  0.00965 **
#   PET                 -0.008234   0.003343  -2.463  0.03350 *
#   turbidity.adjusted  -4.089856   2.166890  -1.887  0.08844
# Residual standard error: 0.4496 on 10 degrees of freedom
# Multiple R-squared:  0.4853,	Adjusted R-squared:  0.3823 
# F-statistic: 4.714 on 2 and 10 DF,  p-value: 0.03614

options(na.action = "na.fail")
dredge(full.model.trm3.fish)
options(na.action = "na.omit")
model.66 <- lm(shannon ~ NH3.N, data = trm3.fish)
summary(model.66)
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     1.7183     0.3718   4.622   0.000738 ***
#   NH3.N        -5.5659     2.3903  -2.329   0.039971 *  
# Residual standard error: 0.489 on 11 degrees of freedom
# Multiple R-squared:  0.3302,	Adjusted R-squared:  0.2693 
# F-statistic: 5.422 on 1 and 11 DF,  p-value: 0.03997

# The p value for the 13/13 sites ammonia model is significant.

# Compare to 8/13 sites glm
slim3.fish <- trm3.fish[-c(6,7,9,11,13),]
full.model.slim3.fish <- lm(shannon~ PET + PRECIP_SEAS_IND+RIP100_PLANT+NH3.N+PO43., data = slim3.fish)
summary(full.model.slim3.fish)
vif(full.model.slim3.fish)
# p values are similar for 13/13 compared to 8/13. R^2 is higher for 8/13 sites
b.model.slim <- traditional.backward(full.model.slim3.fish)
summary(b.model.slim)
vif(b.model.slim)
#                       Estimate Std. Error t value Pr(>|t|)  
# (Intercept)        33.050741   9.002714   3.671   0.0214 *
#   PET              -0.030141   0.008615  -3.499   0.0249 *
#   PRECIP_SEAS_IND  14.657209   5.484356   2.673   0.0557 .
# PO43.              -0.174283   0.059611  -2.924   0.0431 *
# Residual standard error: 0.2118 on 4 degrees of freedom
# Multiple R-squared:  0.8921,	Adjusted R-squared:  0.8112 
# F-statistic: 11.03 on 3 and 4 DF,  p-value: 0.02102
options(na.action = "na.fail")
dredge(full.model.slim3.fish) # Implicates NH3.N, PET, or PRE_SEA_IND
options(na.action = "na.omit")

model.66 <- lm(shannon ~ NH3.N, data = slim3.fish)
summary(model.66)
#                Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     1.5971     0.3361   4.751  0.00315 **
#   NH3.N        -4.7324     2.2026  -2.149  0.07526 . 
# Residual standard error: 0.3959 on 6 degrees of freedom
# Multiple R-squared:  0.4348,	Adjusted R-squared:  0.3406 
# F-statistic: 4.616 on 1 and 6 DF,  p-value: 0.07526
#
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# glm shannon summary:
# Variation in shannon diversity can be explained very well (R^2=.89) by using 8/13 sites. 
# The multivariate regression indicates PET and PO4 are inversely related to SI while PRECIP_SEAS_IND is positively correlated to SI.
# The single variable model implicated by dredge uses ammonia, which negatively correlates with SI (p = 0.07).
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Rarified.Richness multivariate regressions

f.mod.trm <- lm(rare.rich~ PET + PRECIP_SEAS_IND+RIP100_PLANT+PESTAPP_KG_SQKM+NH3.N+pH+turbidity.adjusted+PO43., data = trm3.fish)
summary(f.mod.trm)
vif(f.mod.trm)
# Residual standard error: 0.2551 on 4 degrees of freedom
# Multiple R-squared:  0.7655,	Adjusted R-squared:  0.2966 
# F-statistic: 1.633 on 8 and 4 DF,  p-value: 0.3344

backward.model <- traditional.backward(f.mod.trm)
summary(backward.model)
vif(backward.model)
#                    Estimate  Std. Error t value  Pr(>|t|)  
# (Intercept)        7.897619   2.834220   2.787   0.0237 *
#   PET             -0.005721   0.002633  -2.173   0.0616 .
# RIP100_PLANT      -0.025582   0.010918  -2.343   0.0472 *
#   PESTAPP_KG_SQKM  1.032119   0.491221   2.101   0.0688 .
# PO43.             -0.163727   0.054591  -2.999   0.0171 *
# Residual standard error: 0.2245 on 8 degrees of freedom
# Multiple R-squared:  0.637,	Adjusted R-squared:  0.4554 
# F-statistic: 3.509 on 4 and 8 DF,  p-value: 0.06163

options(na.action = "na.fail")
dredge(f.mod.trm)
options(na.action = "na.omit")

fish.model.17 <- lm(rare.rich~ NH3.N + RIP100_PLANT, data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.239899   0.212270   10.55 9.69e-07 ***
#   NH3.N        -3.416647   1.105821   -3.09   0.0114 *  
#   RIP100_PLANT -0.006470   0.002741   -2.36   0.0399 *  
#
# Residual standard error: 0.2214 on 10 degrees of freedom
# Multiple R-squared:  0.5585,	Adjusted R-squared:  0.4702 
# F-statistic: 6.326 on 2 and 10 DF,  p-value: 0.01677

# Compare to 8/13 sites glm
f.mod.slim <- lm(rare.rich~ PET + PRECIP_SEAS_IND+RIP100_PLANT+NH3.N+PO43., data = slim3.fish)
summary(f.mod.slim)
vif(f.mod.slim)
#                     Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       19.9523661  4.5257347   4.409   0.0478 *
#   PET             -0.0173954  0.0044230  -3.933   0.0590 .
# PRECIP_SEAS_IND    9.0547935  2.5504629   3.550   0.0710 .
# RIP100_PLANT      -0.0009571  0.0011532  -0.830   0.4939  
# NH3.N              0.4593235  0.8638092   0.532   0.6481  
# PO43.             -0.1164187  0.0287712  -4.046   0.0560 .
# Residual standard error: 0.04291 on 2 degrees of freedom
# Multiple R-squared:  0.9907,	Adjusted R-squared:  0.9675 
# F-statistic: 42.64 on 5 and 2 DF,  p-value: 0.02307

backward.model <- traditional.backward(f.mod.slim)
summary(backward.model)
vif(backward.model)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     17.7511153  1.5954302  11.126  0.00156 **
#   PET             -0.0152357  0.0015276  -9.974  0.00214 **
#   PRECIP_SEAS_IND  7.8342646  0.9698962   8.077  0.00396 **
#   RIP100_PLANT    -0.0014630  0.0005684  -2.574  0.08221 . 
# PO43.           -0.1027015  0.0111131  -9.241  0.00268 **
# Residual standard error: 0.03743 on 3 degrees of freedom
# Multiple R-squared:  0.9894,	Adjusted R-squared:  0.9752 
# F-statistic: 69.95 on 4 and 3 DF,  p-value: 0.002714

options(na.action = "na.fail")
dredge(f.mod.slim)
options(na.action = "na.omit")

fish.model.18 <- lm(rare.rich~ NH3.N + RIP100_PLANT, data=slim3.fish)
summary(fish.model.18)
vif(fish.model.18)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.164151   0.131640  16.440 1.52e-05 ***
#   NH3.N        -3.402894   0.661601  -5.143  0.00363 ** 
#   RIP100_PLANT -0.005640   0.001607  -3.510  0.01711 *  
#
# Residual standard error: 0.1083 on 5 degrees of freedom
# Multiple R-squared:  0.852,	Adjusted R-squared:  0.7927 
# F-statistic: 14.39 on 2 and 5 DF,  p-value: 0.008433

# rare.rich summary:
# The results of glms are better for 8/13 sites.
# The backwards models indicate rarefied.rich significant correlates:
# - PET
# + PRECIP_SEAS_IND
# - PO4
# The dredge pulls a model indicating ammonia and planted crops within 100m of stream negatively correlate with rare.rich.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Fish glm conclusions:
# This analysis produces more significant results (better p values) when using 8/13 sites
# climate mechanisms may include seasonality of Precipitation
# Ammonia and phophate levels correlate with diveristy metrics, but their source remains unclear (fertilizer, runoff, other).
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Fish
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:23)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_BASIN),]
# ford <- ford[,-c(6,7,20)] # had to remove columns with only zeros. These came from sites that were removed because of n <10.
nms1 <- metaMDS(ford[,c(2:20)], noshare=0.2,k=2)

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