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

# Create trimmed data.frame using variables from PCA. See end of script for PCA
msterfish$USGS.gauge <- rownames(msterfish)
trm.fish <- msterfish[,c("USGS.gauge", "rare.rich", "shannon", "PPTAVG_BASIN", "RIP100_PLANT",  "NH3.N", "conductivity", "dissolved.oxygen", "PO43.", "width.depth")]

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
aug.pairs(trm.fish[,-1], na.action=na.omit)

# Identified need to log transform: 
trm.fish$conductivity <- log10(trm.fish$conductivity)

# Evaluate log transformations
aug.pairs(trm.fish[,-1], na.action=na.omit)
trm3.fish <- trm.fish

# shannon multivariate regressions
full.model.trm3.fish <- lm(shannon~ PPTAVG_BASIN+RIP100_PLANT+NH3.N+conductivity+dissolved.oxygen+PO43.+width.depth, data = trm3.fish)
summary(full.model.trm3.fish)
vif(full.model.trm3.fish)

b.model.trm <- traditional.backward(full.model.trm3.fish)
summary(b.model.trm)
vif(b.model.trm)

options(na.action = "na.fail")
dredge(full.model.trm3.fish)
options(na.action = "na.omit")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# glm shannon summary:
# backwards and dredge models indicate ammonia and phosphates are negative predictors of SI. PPT is a weak positive predictor of SI (seen in dredge).
# Conclusion:This indicates that climate and land-use are larger predictors of SI. 
# Discussion: The question is whether climate is covarying with land-use. For example, what if there is more agriculter in areas with more precipitation?

# SI plots:
plot(shannon ~ NH3.N, xlab="Ammonia concentration (ppm)", ylab="Vertebrate Shannon Index", data=trm.fish)
abline(lm(shannon ~ NH3.N, trm.fish))
summary(lm(shannon ~ NH3.N, trm.fish))
# p-value: 0.03997

# (shannon ~ PO43., xlab="Orthophosphate Concentration (ppm)", ylab="Vertebrate Shannon Index", data=trm.fish)
# abline(lm(shannon ~ PO43., trm.fish))
# summary(lm(shannon ~ PO43., trm.fish))
# p-value: 0.1041
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Rarified.Richness multivariate regressions
f.mod.trm <- lm(rare.rich~ PPTAVG_BASIN+RIP100_PLANT+NH3.N+conductivity+dissolved.oxygen+PO43.+width.depth, data = trm3.fish)
summary(f.mod.trm)
vif(f.mod.trm)

backward.model <- traditional.backward(f.mod.trm)
summary(backward.model)
vif(backward.model)

options(na.action = "na.fail")
dredge(f.mod.trm)
options(na.action = "na.omit")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Result: Ammonia and phosphates are negative predictors of rarified richness. PPT is not implicated as a significant predictor.
# Conclusion: Land use is a significant predictor of community rarified richness.
# Discussion: How does fertilizer application decrease rarified richness? What is the gradient of fertilizer application and how does it relate spatially to our diversity indices? What links ammonia and phosphate to fish?
# RR plots:

plot(rare.rich ~ NH3.N, xlab="Ammonia Concentration (ppm)", ylab="Vertebrate Rarified Species Richness", data=trm.fish)
abline(lm(rare.rich ~ NH3.N, trm.fish))
summary(lm(rare.rich ~ NH3.N, trm.fish))
#  p-value: 0.047

# plot(rare.rich ~ PO43., xlab="Orthophosphate Concentration (ppm)", ylab="Vertebrate Rarified Species Richness", data=trm.fish)
# abline(lm(rare.rich ~ PO43., trm.fish))
# summary(lm(rare.rich ~ PO43., trm.fish))
# p-value: 0.05698

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Fish
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:23)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_BASIN),]
rownames(ford) <- 1:13
# ford <- ford[,-c(6,7,20)] # had to remove columns with only zeros. These came from sites that were removed because of n <10.
nms1 <- metaMDS(ford[,c(2:20)], noshare=0.2,k=2)

# heirarchical clustering identifies groupings of sites
clusters <- hclust(dist(ford[,c(24:26)]), method="ward.D")
plot(clusters)
rect.hclust(clusters, h=80)
clusterCut <- cutree(clusters, h=80)
table(clusterCut, ford$PPTAVG_BASIN)
ford$clusterCut <- clusterCut

# Shade with Color Gradient based on precipitation
rbPal <- colorRampPalette(c('yellow','blue')) 
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_BASIN,breaks = 10))]#This adds a column of color values based on the y values

# Fit Environmental variables to nmds
ord2 <- decorana(ford[2:23])
ord.fit <- envfit(ord2 ~ PPTAVG_BASIN+RIP100_PLANT+NH3.N+conductivity+PO43.+width.depth, data=ford, perm=999, na.rm=TRUE)

# Simplify ord.fit labels
colnames(ford)[colnames(ford)=="PPTAVG_BASIN"] <- "P"
colnames(ford)[colnames(ford)=="RIP100_PLANT"] <- "R"
colnames(ford)[colnames(ford)=="NH3.N"] <- "N"
colnames(ford)[colnames(ford)=="conductivity"] <- "C"
colnames(ford)[colnames(ford)=="PO43."] <- "O"
colnames(ford)[colnames(ford)=="width.depth"] <- "W"
ord.fit <- envfit(ord2 ~ P+R+N+C+O+W, data=ford, perm=999, na.rm=TRUE)

# Plot
par(mfrow=c(1,1))
plot(nms1, type="n", ylim=c(-1.5,1.5), xlim=c(-1.5,1.5),main= "nmds.envfit.fish")
ordihull(nms1, groups = ford$clusterCut, draw="polygon", kind = "ehull",border=NA, col=c("yellow","blue"), alpha=.2)
plot(ord.fit, col="black", cex=.1)
points(nms1, display = "sites", cex = 2, pch=21, col="black", bg=ford$Col)
#text(nms1, display = "spec", cex=0.3, col="black")

# ::::::::::::::::::::::::::::::::::::::::::::::::
# Create multiplot for top environmental predictors
# Beautify NMDS in ppt or paint (Redo Axis labels, Title, env.fit arrows and labels, insert iconic fish photos with small species labels)
# Write explanatory caption with short interpretation































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
