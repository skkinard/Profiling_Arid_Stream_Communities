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
invert <- read.csv("Kicknet.Survey.Spring2017.csv", fileEncoding="UTF-8-BOM", na.strings="")
gauge.data <- read.csv("Sp17_gaugesconcotenated.csv", fileEncoding="UTF-8-BOM")
habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")
flow <- read.csv("USGS_FlowMetrics.csv", fileEncoding = "UTF-8-BOM")

# bio.infer
icnt.TX <- invert[,c(4,19,13)]
i_spec <- get.taxonomic(icnt.TX)
i_spec <- get.otu(i_spec)
invert <- makess(i_spec)

# Analyze same sites as the fish analysis (10 sites). Remove 8068450, 8189200, 8211520
invert <- invert[-c(4,10,12),]

# Generate Diversity and evenness metrics
invert$shannon <- diversity(invert[,2:96])
invert$rich <- specnumber(invert[,2:96])

(raremax <- min(rowSums(invert[,2:96])))
invert$rare.rich <- rarefy(invert[,2:96], raremax)

write.csv(invert,"C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\TCP_invert_x_site.csv", row.names = FALSE)

# Merge data.frames
msterinv <- merge(invert[,-c(2:96)], habitat[,-31], by = "USGS.gauge")
msterinv <- merge(msterinv, gauge.data, by = "USGS.gauge")
msterinv <- merge(msterinv, flow[,-1], by = "USGS.gauge")
msterinv <- merge(msterinv, outfall, by = "USGS.gauge")
row.names(msterinv) <- msterinv$USGS.gauge
msterinv <- Filter(is.numeric, msterinv)

# Glance at new final variables for sig correlations >0.4

aug.pairs(msterinv[,c(2,5,6,142:149)], na.action=na.omit)
aug.pairs(msterinv[,c(2,5,6,150:157)], na.action=na.omit)

# Intro Plots invert
msterinvert <- merge(invert[,-c(2:96)], habitat[,-31], by = "USGS.gauge")
factor(gauge.data$USGS.gauge)
msterinvert <- merge(msterinvert, gauge.data, by = "USGS.gauge")
row.names(msterinvert) <- msterinvert$USGS.gauge
msterinvert <- Filter(is.numeric, msterinvert)

# Shannon 
par(mfrow=c(1,2))
plot(shannon ~ PPTAVG_SITE, data=msterinvert, main="Shannon")
abline(lm(shannon ~ PPTAVG_SITE, data=msterinvert))
summary(lm(shannon ~ PPTAVG_SITE, data=msterinvert))

plot(rich ~ PPTAVG_SITE, data=msterinvert, main="rich")
abline(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert))
summary(lm(rare.rich ~ PPTAVG_SITE, data=msterinvert))

# These plots indicate little correlation between diversity metrics and climate gradient.
# Previous script runs side-by-side comparisons with and without TRC. Both produce ns results.

# Ran PCA (see end of script)
# ::::::::::::::::::::::::::::::::::::::::::::::::::
# Proceed to aug.pairs, glm, and NMDS
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

# Create trimmed data.frame using variables from PCA
trm.invert <- msterinvert[,c("rare.rich", "shannon", "PPTAVG_BASIN", "NLCD01_06_DEV", "IMPNLCD06","RIP100_PLANT", "turbidity.adjusted", "conductivity", "dissolved.oxygen", "NH3.N", "PO43.", "NO3.NO2", "HYDRO_DISTURB_INDX")]

#Identify variables that require log transformation
aug.pairs(trm.invert, na.action=na.omit)

trm.invert$NLCD01_06_DEV <- log10(trm.invert$NLCD01_06_DEV+.000001)
trm.invert$IMPNLCD06 <- log10(trm.invert$IMPNLCD06+.000001)
trm.invert$conductivity <- log10(trm.invert$conductivity)
trm.invert$turbidity.adjusted <- log10(trm.invert$turbidity.adjusted+.000001)
trm.invert$PO43. <- log10(trm.invert$PO43.)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::
# Linear Regression analysis
# shannon vs PPT
full.model.invert <- lm(shannon~ PPTAVG_BASIN+RIP100_PLANT+conductivity+dissolved.oxygen+NH3.N+PO43.+HYDRO_DISTURB_INDX, data=trm.invert)
summary(full.model.invert)
vif(full.model.invert)

backward.model <- traditional.backward(full.model.invert)
summary(backward.model)
vif(backward.model)

options(na.action = "na.fail")
dredge(full.model.invert)
options(na.action = "na.omit")

# Summary Shannon:
# Results: Phosphate concentration is a positive predictor of Invertbrate shannon diversity. Weaker predictors include hydrologic_disturbance index (-) and pasture/cropland within 100m riparian zone (+)
# Conclusion: Phosphates drive invertebrate species diversity and evenness up. PPT is a weakly positive predictor of invert SI.
# Discussion: What is the nature of the Phosphate gradient? Are phophates colinear with climate or land-use variables? How do phosphates change streams conditions to promote higher biodiversity in inverts?

# rare.richness
full.model.invert <- lm(rare.rich~ PPTAVG_BASIN+RIP100_PLANT+conductivity+dissolved.oxygen+NH3.N+PO43.+HYDRO_DISTURB_INDX, data=trm.invert)
summary(full.model.invert)
vif(full.model.invert)

backward.model <- traditional.backward(full.model.invert)
summary(backward.model)
vif(backward.model)

options(na.action = "na.fail")
dredge(full.model.invert)
options(na.action = "na.omit")

# rarified species richness summary:
# Results: Phosphate is strong (+) of RR. Hydro_Dist_INDX and rip_Plant are negative predictors
# Conclusions: Similar to SI, Phosphates are a strong predictor of invertebrate diversity
# Discussion: How does PO4 drive diversity up? what factors are colinear with Phosphates?

# :::::::::::::::::::::::::::::::::::::::::::::::::::::
# INVERT linear regression plots
par(mfrow=c(2,2))

plot(shannon ~ PO43., xlab="log(Phosphate)", ylab="Shannon Index", data=trm.invert,pch=19)
abline(lm(shannon ~ PO43., trm.invert),lwd=1, lty=2)

plot(shannon ~ dissolved.oxygen, xlab="Oxygen (ppm)", ylab="Shannon Index", data=trm.invert,pch=19)
abline(lm(shannon ~ dissolved.oxygen, trm.invert),lwd=1, lty=2)

plot(rare.rich ~ PO43., xlab="log(Phosphate)", ylab="Rarified Richness", data=trm.invert,pch=19)
abline(lm(rare.rich ~ PO43., trm.invert),lwd=1, lty=2)

plot(rare.rich ~ dissolved.oxygen, xlab="Oxygen (ppm)", ylab="Rarified Richness", data=trm.invert,pch=19)
abline(lm(rare.rich ~ dissolved.oxygen, trm.invert),lwd=1, lty=2)

summary(lm(shannon ~ PO43., trm.invert))
summary(lm(shannon ~ dissolved.oxygen, trm.invert))
summary(lm(rare.rich ~ PO43., trm.invert))
summary(lm(rare.rich ~ dissolved.oxygen, trm.invert))


par(mfrow=c(2,2))

plot(shannon ~ PO43., xlab="log(Phosphate)", ylab="Shannon Index", data=trm.invert,pch=19)
abline(lm(shannon ~ PO43., trm.invert),lwd=1, lty=2)

plot(shannon ~ dissolved.oxygen, xlab="Oxygen (ppm)", ylab="Shannon Index", data=trm.invert,pch=19)
abline(lm(shannon ~ dissolved.oxygen, trm.invert),lwd=1, lty=2)

plot(shannon ~ RIP100_PLANT, xlab="RIP100_PLANT", ylab="Shannon Index", data=trm.invert,pch=19)
abline(lm(shannon ~ RIP100_PLANT, trm.invert),lwd=1, lty=2)

plot(shannon ~ PPTAVG_BASIN, xlab="PPTAVG_BASIN", ylab="Shannon Index", data=trm.invert,pch=19)
abline(lm(shannon ~ PPTAVG_BASIN, trm.invert),lwd=1, lty=2)

summary(lm(shannon ~ PO43., trm.invert))
summary(lm(shannon ~ dissolved.oxygen, trm.invert))
summary(lm(shannon ~ RIP100_PLANT, trm.invert))
summary(lm(shannon ~ PPTAVG_BASIN, trm.invert))
# :::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Inverts
trm.invert$USGS.gauge <- rownames(trm.invert)
ford <- merge(invert[,c(1:96)],trm.invert, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_BASIN),]
rownames(ford) <- c(1:11)
nms1 <- metaMDS(ford[,c(2:96)], k=2)

# Fit environmental Variables
# Simplify ord.fit labels
colnames(ford)[colnames(ford)=="PPTAVG_BASIN"] <- "P"
colnames(ford)[colnames(ford)=="RIP100_PLANT"] <- "R"
colnames(ford)[colnames(ford)=="NH3.N"] <- "N"
colnames(ford)[colnames(ford)=="conductivity"] <- "C"
colnames(ford)[colnames(ford)=="PO43."] <- "PO"
colnames(ford)[colnames(ford)=="HYDRO_DISTURB_INDX"] <- "H"
colnames(ford)[colnames(ford)=="dissolved.oxygen"] <- "O"
ord2 <- decorana(ford[2:96])
ord.fit <- envfit(ord2 ~ P+R+C+PO+O, data=ford, perm=999, na.rm=TRUE)

# Color gradient sites by precipitation
rbPal <- colorRampPalette(c('azure','darkred')) 
ford <- ford[order(ford$P),]
ford$Col <- rbPal(10)[as.numeric(cut(ford$P,breaks = 10))]

# heirarchical clustering identifies groupings of sites
clusters <- hclust(dist(ford[,c(97,98,99)]), method="average")
plot(clusters)
rect.hclust(clusters, 3)
clusterCut <- cutree(clusters, 3)
table(clusterCut, ford$P)
ford$clusterCut <- clusterCut

# final plot
par(mfrow=c(1,1))
plot(nms1, disp = "sites", type = "n", main="nmds.invert", ylim=c(-.5,.5), xlim=c(-2,2))
ordihull(nms1, groups = ford$clusterCut, draw="polygon", border = FALSE, kind = "ehull", col=c("azure3", "darkorchid","darkred"), alpha=.2)
plot(ord.fit, col="black", cex=.1)
points(nms1, display = "sites", cex =2.0, pch=21, col="black", bg=ford$Col)
text(nms1, display = "spec", cex=0.4, col="black")

# ::::::::::::::::::::::::::::::::::::::::::::::::
# Create multiplot for top environmental predictors
# Beautify NMDS in ppt or paint (Redo Axis labels, Title, env.fit arrows and labels, insert iconic fish photos with small species labels)
# Write explanatory caption with short interpretation




























































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
