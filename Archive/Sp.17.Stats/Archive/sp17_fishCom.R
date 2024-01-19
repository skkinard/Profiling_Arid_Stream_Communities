# Last update: January 2020
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
fmat <- read.csv("Spring2017_Electrofish_SiteSpeciesMatrix.csv", fileEncoding="UTF-8-BOM")
gauge.data <- read.csv("Sp17_gaugesconcotenated.csv", fileEncoding="UTF-8-BOM")
habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")
flow <- read.csv("USGS_FlowMetrics.csv", fileEncoding = "UTF-8-BOM")
outfall <- read.csv("OutfallMetrics_TCEQ_skk.csv", fileEncoding= "UTF-8-BOM")

# Generate Diversity and evenness metrics
fmat$shannon <- diversity(fmat[,3:20])
fmat$simpson <- diversity(fmat[,3:20], index = "simpson")
fmat$rich <- specnumber(fmat[,3:20])
(raremax <- min(rowSums(fmat[,3:20]))) # rarefaction is unclear here. What exactly is happening here?
fmat$rare.rich <- rarefy(fmat[,3:20], raremax)
fmat$abun <- rowSums(fmat[,3:20])

write.csv(fmat,"C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\TCP_fish_x_site.csv", row.names = FALSE)

# Merge data.frames
msterfish <- merge(fmat[,-c(3:20)], habitat[,-31], by = "USGS.gauge")
msterfish <- merge(msterfish, gauge.data, by = "USGS.gauge")
msterfish <- merge(msterfish, flow[,-1], by = "USGS.gauge")
msterfish <- merge(msterfish, outfall, by = "USGS.gauge")
row.names(msterfish) <- msterfish$USGS.gauge
msterfish <- Filter(is.numeric, msterfish)

# Create list of non-redundant environmental variables incorporating flow and OF metrics
aug.pairs(msterfish[,c(2,5,6,142:149)], na.action=na.omit)
aug.pairs(msterfish[,c(2,5,6,150:157)], na.action=na.omit)

# Create trimmed data.frame using variables from PCA. See end of script for PCA
msterfish$USGS.gauge <- rownames(msterfish)
trm.fish <- msterfish[,c("USGS.gauge", "rare.rich", "shannon", "abun",  "PPTAVG_BASIN", "RIP100_PLANT", "minDD30", "LFPP", "MeanFlow", "salinity", "dissolved.oxygen", "pH", "temperature.water", "NH3.N", "PO43.", "width.depth", "canopy.mid", "OF_per_sqkm")]
row.names(trm.fish) <- trm.fish$USGS.gauge
trm.fish <- trm.fish[,-1]
head(trm.fish)

# Aug.pairs to confirm significant LR relationships
aug.pairs(trm.fish[,c(-2,-3)], na.action=na.omit)
aug.pairs(trm.fish[,c(-1,-3)], na.action=na.omit)
aug.pairs(trm.fish[,c(-1,-2)], na.action=na.omit)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot Diversity vs Precipitation
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
plot(shannon ~ PPTAVG_BASIN, xlab="Annual Precipitation (cm)", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ PPTAVG_BASIN, trm.fish))
summary(lm(shannon ~ PPTAVG_BASIN, trm.fish))

plot(rare.rich ~ PPTAVG_BASIN, xlab="Annual Precipitation (cm)", ylab="Vertebrate Rarified Species Richness", data=trm.fish)
abline(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))
summary(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Proceed to Linear regression analysis
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

# Evaluate simple regression analysis of diversity vs environmental predictors
aug.pairs(trm.fish[,c(-1,-3)], na.action=na.omit)


# Identified need to log transform: 
trm.fish$salinity <- log10(trm.fish$salinity)
trm.fish$PO43. <- log10(trm.fish$PO43.)
trm.fish$OF_per_sqkm <- log10(trm.fish$OF_per_sqkm)

# Evaluate log transformations
aug.pairs(trm.fish[,c(-1,-3)], na.action=na.omit)
trm3.fish <- trm.fish

# shannon multivariate regressions
full.model.trm3.fish <- lm(shannon~ PPTAVG_BASIN+LFPP+salinity+NH3.N+canopy.mid+OF_per_sqkm, data = trm3.fish)
summary(full.model.trm3.fish)
vif(full.model.trm3.fish)

b.model.trm <- traditional.backward(full.model.trm3.fish)
summary(b.model.trm)
vif(b.model.trm)

options(na.action = "na.fail")
dredge(full.model.trm3.fish)
options(na.action = "na.omit")

best.dredge <- lm(shannon~ PPTAVG_BASIN+LFPP, data = trm3.fish)
summary(best.dredge)
vif(best.dredge)

# SI Univariate Regressions:
par(mfrow=c(2,3))
plot(shannon ~ LFPP, xlab="Low Flow Pulse Percentage", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ LFPP, trm.fish))
summary(lm(shannon ~ LFPP, trm.fish))

plot(shannon ~ canopy.mid, xlab="Canopy Cover", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ canopy.mid, trm.fish))
summary(lm(shannon ~ canopy.mid, trm.fish))

plot(shannon ~ PPTAVG_BASIN, xlab="Annual Precipitation", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ PPTAVG_BASIN, trm.fish))
summary(lm(shannon ~ PPTAVG_BASIN, trm.fish))

plot(shannon ~ OF_per_sqkm, xlab="log-transformed Outfall density", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ OF_per_sqkm, trm.fish))
summary(lm(shannon ~ OF_per_sqkm, trm.fish))

plot(shannon ~ salinity, xlab="salinity", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ salinity, trm.fish))
summary(lm(shannon ~ salinity, trm.fish))

plot(shannon ~ NH3.N, xlab="NH3N.", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ NH3.N, trm.fish))
summary(lm(shannon ~ NH3.N, trm.fish))

# rare.rich multivariate regressions
full.model.trm3.fish <- lm(rare.rich~ PPTAVG_BASIN+LFPP+salinity+NH3.N+canopy.mid+OF_per_sqkm, data = trm3.fish)
summary(full.model.trm3.fish)
vif(full.model.trm3.fish)

b.model.trm <- traditional.backward(full.model.trm3.fish)
summary(b.model.trm)
vif(b.model.trm)

options(na.action = "na.fail")
dredge(full.model.trm3.fish)
options(na.action = "na.omit")

best.dredge <- lm(rare.rich~ PPTAVG_BASIN+LFPP, data = trm3.fish)
summary(best.dredge)
vif(best.dredge)

# Rarified Richness Univariate Linear Regressions:
par(mfrow=c(2,3))
plot(rare.rich ~ LFPP, xlab="Low Flow Pulse Percentage", ylab="Rarefied Richness", data=trm.fish)
abline(lm(rare.rich ~ LFPP, trm.fish))
summary(lm(rare.rich ~ LFPP, trm.fish))

plot(rare.rich ~ canopy.mid, xlab="Canopy Cover", ylab="Rarefied Richness", data=trm.fish)
abline(lm(rare.rich ~ canopy.mid, trm.fish))
summary(lm(rare.rich ~ canopy.mid, trm.fish))

plot(rare.rich ~ PPTAVG_BASIN, xlab="Annual Precipitation", ylab="Rarefied Richness", data=trm.fish)
abline(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))
summary(lm(rare.rich ~ PPTAVG_BASIN, trm.fish))

plot(rare.rich ~ OF_per_sqkm, xlab="log-transformed Outfall density", ylab="Rarefied Richness", data=trm.fish)
abline(lm(rare.rich ~ OF_per_sqkm, trm.fish))
summary(lm(rare.rich ~ OF_per_sqkm, trm.fish))

plot(rare.rich ~ salinity, xlab="salinity", ylab="Rarefied Richness", data=trm.fish)
abline(lm(rare.rich ~ salinity, trm.fish))
summary(lm(rare.rich ~ salinity, trm.fish))

plot(rare.rich ~ NH3.N, xlab="NH3N.", ylab="Rarefied Richness", data=trm.fish)
abline(lm(rare.rich ~ NH3.N, trm.fish))
summary(lm(rare.rich ~ NH3.N, trm.fish))

# ::::::::::::::::::::::::::::::::::::::::::::

# NMDS Fish
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(2:20)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_BASIN),]
rownames(ford) <- 1:10
# ford <- ford[,-c(6,7,20)] # had to remove columns with only zeros. These came from sites that were removed because of n <10.
nms1 <- metaMDS(ford[,c(2:19)], noshare=0.2,k=2)

# heirarchical clustering identifies groupings of sites
clusters <- hclust(dist(ford[,c(20,21,23)]), method="ward.D")
plot(clusters)
rect.hclust(clusters,3)
clusterCut <- cutree(clusters,3)
table(clusterCut, ford$PPTAVG_BASIN)
ford$clusterCut <- clusterCut

# Shade with Color Gradient based on precipitation
rbPal <- colorRampPalette(c('yellow','blue')) 
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_BASIN,breaks = 10))]#This adds a column of color values based on the y values

# Fit Environmental variables to nmds
ord2 <- decorana(ford[2:19])
ord.fit <- envfit(ord2 ~ PPTAVG_BASIN+LFPP+salinity+NH3.N+canopy.mid+OF_per_sqkm, data=ford, perm=999, na.rm=TRUE)

# Plot
par(mfrow=c(1,1))
plot(nms1, type="n", ylim=c(-1.5,1.5), xlim=c(-2,2),main= "nmds.envfit.fish")
ordihull(nms1, groups = ford$clusterCut, draw="polygon", kind = "ehull",border=NA, col=c("blue", "green"), alpha=.25)
plot(ord.fit, col="black", cex=1)
points(nms1, display = "sites", cex = .4, pch=21, col="black", bg=ford$Col)
#text(nms1, display = "spec", cex=0.3, col="black")

# ::::::::::::::::::::::::::::::::::::::::::::::::
# Plots to identify drivers of ammonia concentrations

par(mfrow=c(2,2))
plot(NH3.N ~ OF_per_sqkm, xlab="OF_per_sqkm", ylab="NH3.N", data=trm.fish, cex=1, pch=19)
abline(lm(NH3.N ~ OF_per_sqkm, trm.fish),lwd=1, lty=2)
summary(lm(NH3.N ~ OF_per_sqkm, trm.fish))

plot(NH3.N ~ canopy.mid, xlab="canopy.mid", ylab="NH3.N", data=trm.fish, cex=1, pch=19)
abline(lm(NH3.N ~ canopy.mid, trm.fish),lwd=1, lty=2)
summary(lm(NH3.N ~ canopy.mid, trm.fish))

plot(NH3.N ~ PPTAVG_BASIN, xlab="PPTAVG_BASIN", ylab="NH3.N", data=trm.fish, cex=1, pch=19)
abline(lm(NH3.N ~ PPTAVG_BASIN, trm.fish),lwd=1, lty=2)
summary(lm(NH3.N ~ PPTAVG_BASIN, trm.fish))

plot(NH3.N ~ salinity, xlab="salinity", ylab="NH3.N", data=trm.fish, cex=1, pch=19)
abline(lm(NH3.N ~ salinity, trm.fish),lwd=1, lty=2)
summary(lm(NH3.N ~ salinity, trm.fish))

# ::::::::::::::::::::::::::::::::::::::::::::::::
# Plot to identify drivers of salinity concentrations
par(mfrow=c(1,2))
plot(salinity ~ canopy.mid, xlab="canopy.mid", ylab="salinity", data=trm.fish, cex=1, pch=19)
abline(lm(salinity ~ canopy.mid, trm.fish),lwd=1, lty=2)
summary(lm(salinity ~ canopy.mid, trm.fish))

plot(salinity ~ PPTAVG_BASIN, xlab="PPTAVG_BASIN", ylab="salinity", data=trm.fish, cex=1, pch=19)
abline(lm(salinity ~ PPTAVG_BASIN, trm.fish),lwd=1, lty=2)
summary(lm(salinity ~ PPTAVG_BASIN, trm.fish))



























# +:+:+::+:+:+:+::+:+:+:+:+:+:+:+:+:+:+::+:+:+:+:+::+:+:+:+:+::+:

# PCA FISH 
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
