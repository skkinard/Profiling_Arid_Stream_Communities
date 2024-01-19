# Last update: August 2019
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

# Load Packages
library(nlme)
library(multcomp)
library(bio.infer)
library(vegan)

# Sean's PC loading packages
library("multcomp", lib.loc="~/R/win-library/3.5")
library("nlme", lib.loc="~/R/win-library/3.5")
library("bio.infer", lib.loc="~/R/win-library/3.5")
library("vegan", lib.loc="~/R/win-library/3.5")

# Import Data Files
fish <- read.csv("Electrofishing.Survey.Spring2017.csv", fileEncoding="UTF-8-BOM")
invert <- read.csv("Kicknet.Survey.Spring2017.csv", fileEncoding="UTF-8-BOM")
gauge.data <- read.csv("Sp17_gaugesconcotenated_TRC.SFC.dupicated.CC.fixed.csv", fileEncoding="UTF-8-BOM")
habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")

# bio.infer code:
# convert the original dataframe into a site x species matrix
# benl <- fish[,c(3,15,16)] # Site.Cycle, Genus.Species, Abundance
# names(benl) <- c("BenSampID", "FinalID", "Count")
# bcnt.tax <- get.taxonomic(benl)
# names(bcnt.tax)
# bcnt.otu <- get.otu(bcnt.tax) # brings the data to the best common level (ie species or genus)
# fmat <- makess(bcnt.otu) # site x species matrix
# colnames(fmat)[colnames(fmat)=="BenSampID"] <- "USGS.gauge"# add back row labels

# After evaluating with bio.infer, I decided to use genus.species matrix directly from the data. OTU assignment reduces species pool by half resulting in compression of variation in diversity.

# convert the original dataframe into a site x species matrix
benl <- fish[,c(3,15,16)] # USGS.gauge, Genus.Species, Abundance
fmat <- makess(benl, tname="genus.species")

# Generate Diversity and evenness metrics
fmat$shannon <- diversity(fmat[,2:23])
fmat$simpson <- diversity(fmat[,2:23], index = "simpson")
fmat$rich <- specnumber(fmat[,2:23])

fmat$abun <- rowSums(Filter(is.numeric, fmat)) # Creating sample size for PIE
fmat$PIE <- (fmat$abun/(fmat$abun-1))*(1-fmat$simpson)
(raremax <- min(rowSums(fmat[,2:23]))) # rarefaction is unclear here. What exactly is happening here?
fmat$rare.rich <- rarefy(fmat[,2:23], raremax)
fmat$abun <- rowSums(fmat[,2:23])

# Intro Plots Fish
msterfish <- merge(fmat[,-c(2:23)], habitat[,-31], by = "USGS.gauge") # merge stream habitat data and diversity index data for fish
factor(gauge.data$USGS.gauge)

msterfish <- merge(msterfish, gauge.data, by = "USGS.gauge") # merge USGS, stream hab, and div. index data frames
colnames(msterfish)
row.names(msterfish) <- msterfish$USGS.gauge
msterfish <- Filter(is.numeric, msterfish)

# There is weak evidence for ppt as a driver of fish community diversity
# There are several sites with abun < 10 that should be removed and evaluated for outlier effects
which(msterfish$abun < 10)
msterfish.not <- msterfish[-c(6,7,9,11,13),]

# Shannon - stronger
par(mfrow=c(1,2))
plot(shannon ~ PPTAVG_SITE, data=msterfish, main="All 13 sites")
abline(lm(shannon ~ PPTAVG_SITE, data=msterfish))
summary(lm(shannon ~ PPTAVG_SITE, data=msterfish))
plot(shannon ~ PPTAVG_SITE , data = msterfish.not, main="n<10 removed")
abline(lm(shannon ~ PPTAVG_SITE, data=msterfish.not))
summary(lm(shannon ~ PPTAVG_SITE, data=msterfish.not))

# Simpson - stronger
par(mfrow=c(1,2))
plot(simpson ~ PPTAVG_SITE, data=msterfish, main="All 13 sites")
abline(lm(simpson ~ PPTAVG_SITE, data=msterfish))
summary(lm(simpson ~ PPTAVG_SITE, data=msterfish))
plot(simpson ~ PPTAVG_SITE , data = msterfish.not, main="n<10 removed")
abline(lm(simpson ~ PPTAVG_SITE, data=msterfish.not))
summary(lm(simpson ~ PPTAVG_SITE, data=msterfish.not))

# PIE - stronger
par(mfrow=c(1,2))
plot(PIE ~ PPTAVG_SITE, data=msterfish, main="All 13 sites")
abline(lm(PIE ~ PPTAVG_SITE, data=msterfish))
summary(lm(PIE ~ PPTAVG_SITE, data=msterfish))
plot(PIE ~ PPTAVG_SITE , data = msterfish.not, main="n<10 removed")
abline(lm(PIE ~ PPTAVG_SITE, data=msterfish.not))
summary(lm(PIE ~ PPTAVG_SITE, data=msterfish.not))

# rich - weaker
par(mfrow=c(1,2))
plot(rich ~ PPTAVG_SITE, data=msterfish, main="All 13 sites")
abline(lm(rich ~ PPTAVG_SITE, data=msterfish))
summary(lm(rich ~ PPTAVG_SITE, data=msterfish))
plot(rich ~ PPTAVG_SITE , data = msterfish.not, main="n<10 removed")
abline(lm(rich ~ PPTAVG_SITE, data=msterfish.not))
summary(lm(rich ~ PPTAVG_SITE, data=msterfish.not))

# rare.rich - stronger
par(mfrow=c(1,2))
plot(rare.rich ~ PPTAVG_SITE, data=msterfish, main="All 13 sites")
abline(lm(rare.rich ~ PPTAVG_SITE, data=msterfish))
summary(lm(rare.rich ~ PPTAVG_SITE, data=msterfish))
plot(rare.rich ~ PPTAVG_SITE , data = msterfish.not, main="n<10 removed")
abline(lm(rare.rich ~ PPTAVG_SITE, data=msterfish.not))
summary(lm(rare.rich ~ PPTAVG_SITE, data=msterfish.not))

# By removing 4 sites, I lose resolution on the potential threshold between 70-90 cm annual rainfall in the diversity metrics. However, most correlations are greater in intensity and significance.

# PCA FISH - using msterfish.not which has n<10 removed
library(factoextra)
msterfish <- msterfish.not
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
sort.PC1 # Note winter precipitation is large contributor to PC1 (dec, nov, feb, jan, mar) LNG_GAGE, PPTAVG_BASIN, PPTAVG_SITE, WD_BASIN, RIP100_43, WD_SITE
sort.PC2 <- var.coord[order(var.coord$PC2),]
sort.PC2 # WDMAX_BASIN, RIP100_82, NITR_APP_KG_SQKM, PHOS_APP_KG_SQKM, RIP100_PLANT, NO3.NO2, turbidity.adjusted, conductivity, salinity, oxygen.saturation, dissolved.oxygen, pH
sort.PC3 <- var.coord[order(var.coord$PC3),]
sort.PC3 # HYDRO_DISTURB_INDX, PIE, NH3.N, RIP100_95, PO43, NO3.NO2
sort.PC4 <- var.coord[order(var.coord$PC4),]
sort.PC4 # Dissolved.oxygen, rich, shannon, RH_Basin

# It doesn't appear that OTU fix affects the PCA. The same variables compose each of the PCA in similar weights as before.

# Create trimmed data.frame using variables from PCA
trm.fish <- msterfish[,c("PIE", "rich", "shannon", "PPTAVG_SITE", "WD_BASIN", "RIP100_43", "WD_SITE", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PHOS_APP_KG_SQKM", "HYDRO_DISTURB_INDX", "RH_BASIN", "turbidity.adjusted", "conductivity", "salinity", "dissolved.oxygen", "pH", "NH3.N", "PO43.")]
head(trm.fish) #Remember that rownames are the USGS gauges

# Proceed to aug.pairs, glm, and NMDS
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

aug.pairs(trm.fish, na.action=na.omit)
# linear regressions indidcate the following variables relate to diversity metrics: "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3.NO2", "pH", "NH3.N","PO43.", "dissolved.oxygen", "PPTAVG_SITE"

# Identified correlation > abs(0.4): "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3.NO2", "pH", "NH3.N","PO43.", "dissolved.oxygen", "PPTAVG_SITE"

# Identified need to log transform: 
# RIP100_43
# NITR_APP_KG_SQKM
# NO3.NO2
# conductivity
# PO43.
colnames(trm.fish)

trm2.fish <-trm.fish[,-c(5,7,8,9,12,13,15)]

trm2.fish$lg.RIP <- log10(trm.fish$RIP100_43)
trm2.fish$lg.PHOS.A <- log10(trm.fish$PHOS_APP_KG_SQKM)
trm2.fish$lg.cond <- log10(trm.fish$conductivity)
trm2.fish$lg.PO43. <- log10(trm.fish$PO43.)
trm2.fish$lg.NH3.N <- log10(trm.fish$NH3.N)
# NO3.NO2 contains negative values which do not log transform. Substitute LR nitrate values for NO3NO2. LR Nitrate data contained NAs and was removed

# Evaluate log transformations
aug.pairs(trm2.fish)

# Log transformations improve linear regressions. Replace true variables with log transformed and proceed to glms.
colnames(trm2.fish)
trm3.fish <- trm2.fish[,-c(5,6,8,11,12)]
aug.pairs(trm3.fish)

# PIE = Probability of interspecies encounter
full.model.fish <- lm(PIE~ PPTAVG_SITE + dissolved.oxygen + pH + lg.cond + lg.PO43. + lg.NH3.N, data = trm3.fish)
summary(full.model.fish)
vif(full.model.fish) 
# A rule of thumb for interpreting the variance inflation factor:
# 1 = not correlated.
# Between 1 and 5 = moderately correlated.
# Greater than 5 = highly correlated.

# Residual standard error: 0.03379 on 1 degrees of freedom
# Multiple R-squared:  0.9969,	Adjusted R-squared:  0.9785 
# F-statistic: 54.18 on 6 and 1 DF,  p-value: 0.1036

# Comments:
# Removing outlier sites with n < 10 made the site number low and I had to reduce the number of variables to run multivariate linear regression. The overall result is a higher correlation coefficient with less power (p value ~ 0.1).
# Removing OTU step has large impact on glm power:
# Residual standard error: 0.176 on 3 degrees of freedom
# Multiple R-squared:  0.9404,	Adjusted R-squared:  0.7618 
# F-statistic: 5.263 on 9 and 3 DF,  p-value: 0.09943
# Compare this to PIE full model with OTU step with adjusted R-Squared of 0.5506, with F-stat 2.47 on 10 and 2 DF, p-value: 0.3225

min.model<-lm(PIE~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:12])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                     Estimate   Std. Error t value Pr(>|t|)   
# (Intercept)         -7.459056   1.527194  -4.884  0.00814 **
#   dissolved.oxygen  -0.005570   0.002524  -2.207  0.09194 . 
# pH                   1.635106   0.282295   5.792  0.00442 **
#   lg.NH3.N           3.330467   0.474818   7.014  0.00218 **
#
# Residual standard error: 0.07836 on 4 degrees of freedom
# Multiple R-squared:  0.934,	Adjusted R-squared:  0.8846 
# F-statistic: 18.88 on 3 and 4 DF,  p-value: 0.007977

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

# Summary of PIE glms:  Water chemistry paramaters (pH, and ammonia concentrations) are significant exlanatory variables of variation in PIE across the sites. Removing outlier sites with n < 10 significantly reduces the scope of multivariate linear analysis, but the results may be more robust and easier to interpret.

# shannon
full.model.fish <- lm(shannon~ PPTAVG_SITE + dissolved.oxygen + pH + lg.cond + lg.PO43. + lg.NH3.N, data = trm3.fish)
summary(full.model.fish)
vif(full.model.fish)
# Residual standard error: 0.04159 on 1 degrees of freedom
# Multiple R-squared:  0.999,	Adjusted R-squared:  0.9927 
# F-statistic: 160.2 on 6 and 1 DF,  p-value: 0.06041

min.model<-lm(shannon~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:12])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                     Estimate   Std. Error t value Pr(>|t|)   
# (Intercept)         15.816283  2.017488   7.840   0.00432 **
#   dissolved.oxygen  0.020547   0.003299    6.229  0.00834 **
#   pH               -3.043855   0.378050   -8.051  0.00400 **
#   lg.PO43.         -0.246169   0.085435   -2.881  0.06346 . 
# lg.NH3.N           -5.560141   0.681000   -8.165  0.00384 **
#
# Residual standard error: 0.102 on 3 degrees of freedom
# Multiple R-squared:  0.9812,	Adjusted R-squared:  0.9562 
# F-statistic: 39.23 on 4 and 3 DF,  p-value: 0.006352

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

# Summary Shannon glms: Significant predictor variables include DO, pH, lg.NH3N.

# Richness
full.model.fish <- lm(rich~ PPTAVG_SITE + dissolved.oxygen + pH + lg.cond + lg.PO43. + lg.NH3.N, data = trm3.fish)
summary(full.model.fish)
vif(full.model.fish)
# Residual standard error: 2.496 on 3 degrees of freedom
# Multiple R-squared:  0.7208,	Adjusted R-squared:  -0.1167 
# F-statistic: 0.8607 on 9 and 3 DF,  p-value: 0.6233

min.model<-lm(rich~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:12])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                    Estimate  Std. Error t value Pr(>|t|)   
# (Intercept)        -2.89254    1.87165  -1.545  0.18290   
# dissolved.oxygen    0.13299    0.03276   4.059  0.00973 **
#   lg.PO43.         -2.01640    0.78221  -2.578  0.04956 * 
#
# Residual standard error: 1.114 on 5 degrees of freedom
# Multiple R-squared:  0.8517,	Adjusted R-squared:  0.7924 
# F-statistic: 14.36 on 2 and 5 DF,  p-value: 0.008471

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

fish.model.17 <- lm(rich~ dissolved.oxygen, data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
# Residual standard error: 1.553 on 6 degrees of freedom
# Multiple R-squared:  0.6546,	Adjusted R-squared:  0.597 
# F-statistic: 11.37 on 1 and 6 DF,  p-value: 0.01501

fish.model.281 <- lm(rich~ lg.NH3.N + dissolved.oxygen, data=trm3.fish)
summary(fish.model.281)
vif(fish.model.281)
# Coefficients:
#                    Estimate  Std. Error t value Pr(>|t|)  
# (Intercept)        -7.60701    2.90483  -2.619   0.0472 *
#   lg.NH3.N         -5.97942    3.11263  -1.921   0.1128  
# dissolved.oxygen    0.12328    0.03972   3.104   0.0267 *
#
# Residual standard error: 1.29 on 5 degrees of freedom
# Multiple R-squared:  0.8013,	Adjusted R-squared:  0.7218 
# F-statistic: 10.08 on 2 and 5 DF,  p-value: 0.01761


# Summary Richness: variation in species richness among the sites is largely explained by ammonia concentrations and dissolved oxygen!

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Fish glm conclusions:
# variation in PIE, SI, richness among the sites in the gradient is largely explained by water quality paramters such as ammonia(colinear with pH) anddissolved oxygen. This conclusion is much simpler and the results are more robust when using 8/13 sites due to n < 10 at 5 sites.

# These conclustions are different from 13 site genus.species glms.
# These conclusions are different from the analysis that used simplified OTUs (genus-level).
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# NMDS Fish

trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:23)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_SITE),]
ford <- ford[,-c(6,7,20)] # had to remove columns with only zeros. These came from sites that were removed because of n <10.
nms1 <- metaMDS(ford[,c(2:20)], k=2)
plot(nms1)
plot(nms1, type="n")
points(nms1, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(nms1, display= "spec", cex=0.7, col="blue")
# There is something wrong with this plot. I think it has to do with reduction in sites and community data overall. For NMDS I will run the full set of sites.

# Reset 13/13 sites
msterfish <- merge(fmat[,-c(2:23)], habitat[,-31], by = "USGS.gauge") 
factor(gauge.data$USGS.gauge)
msterfish <- merge(msterfish, gauge.data, by = "USGS.gauge")
row.names(msterfish) <- msterfish$USGS.gauge
msterfish <- Filter(is.numeric, msterfish)
trm.fish <- msterfish[,c("PIE", "rich", "shannon", "PPTAVG_SITE", "WD_BASIN", "RIP100_43", "WD_SITE", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PHOS_APP_KG_SQKM", "HYDRO_DISTURB_INDX", "RH_BASIN", "turbidity.adjusted", "conductivity", "salinity", "dissolved.oxygen", "pH", "NH3.N", "PO43.")]
trm2.fish <-trm.fish[,-c(5,7,8,9,12,13,15)]
trm2.fish$lg.RIP <- log10(trm.fish$RIP100_43)
trm2.fish$lg.PHOS.A <- log10(trm.fish$PHOS_APP_KG_SQKM)
trm2.fish$lg.cond <- log10(trm.fish$conductivity)
trm2.fish$lg.PO43. <- log10(trm.fish$PO43.)
trm2.fish$lg.NH3.N <- log10(trm.fish$NH3.N)
trm3.fish <- trm2.fish[,-c(5,6,8,11,12)]

# Rerun NMDS
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:23)],trm3.fish, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_SITE),]
nms1 <- metaMDS(ford[,c(2:23)], k=2, noshare=0.1)
plot(nms1)
plot(nms1, type="n")
points(nms1, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(nms1, display= "spec", cex=0.7, col="blue")
# This NMDS looks good. It just needs labels and hulls

# Color Gradient to Reveal groups?
rbPal <- colorRampPalette(c('white','black')) #Create a function to generate a continuous color palette
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_SITE,breaks = 10))]#This adds a column of color values based on the y values
plot(nms1, type="n", main= "fish com. PPT Gradient")
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
#text(nms1, display= "spec", cex=0.8, col="black")
# This plot shows 3 dry sites group separately in fish community composition from the rest.

plot(nms1, disp = "sites", type = "n", main="NMDS Fish labeled species", ylim=c(-.5,.5), xlim=-c(-2.5,3))
#ordispider(nms1, groups = ford$groupPPT, col="black", label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
#ordihull(nms1, groups = ford$groupPPT, lwd = 1)
ordiellipse(nms1, groups = ford$groupPPT, draw="polygon", kind = "ehull", border = NA, col="grey", alpha=.25)
text(nms1, display = "spec", cex=0.4, col="black")

# Fit environmental Variables
colnames(ford)
ord2 <- decorana(ford[2:26])
ord.fit <- envfit(ord2 ~ PPTAVG_SITE + dissolved.oxygen + lg.NH3.N + HYDRO_DISTURB_INDX + lg.PO43. + pH, data=ford, perm=999, na.rm=TRUE)
ord.fit

# ::::::::::::::::::::::::::::::::::::::::::::::::
plot(nms1, disp = "sites", type = "n", main="Fish Ordination with Env. var. fit", ylim=c(-.5,.5), xlim=-c(-2.5,3))
#ordispider(nms1, groups = ford$groupPPT, col="black", label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
#ordihull(nms1, groups = ford$groupPPT, lwd = 1)
ordiellipse(nms1, groups = ford$groupPPT, draw="polygon", kind = "ehull", border = NA, col="grey", alpha=.25)
plot(ord.fit, col="red", cex=0.7)
# ::::::::::::::::::::::::::::::::::::::::::::::::
# Final figure entails: remove env variable labels and replace in paint or ppt.
# axis recommendations?
# Title recommendation?
# Any species labels? too much clutter?
# which to remove: NH3N or pH in plot to simplify (they are colinear)?

# This concludes the fish community analysis of 13 streams along the South Texas Coastal Prairie. In it, 103 environmental variables were evaluated as predictors of variation in 5 different fish community diversity metrics. I used PCA to identify influential variables and trim the data set down to <20 variables to conduct multivariate linear regressions. Finally, I used NMDS to reveal clusters of fish community composition in conjunction with the climate variable (annual precipiation). Environmental variables were fit to the NMDS and species labels added for interprettations and conclusions.

# Closing Coding Comments:
# This analysis involved several reboots. First, I ran PCA and glm on 13 sites using bio.infer at the start. Then, I realized the correlations and nmds were not as clear as they had been during my preliminary data analysis. The second go involved bypassing the bio.infer OTU assignment code. This produced double the species and produced stronger R and p-vals, but again seemed weak for simple linear regressions. So, the third and final script involved removing sites with fewer than 10 sample size for the PCA, linear regression, and mulivariate glm analysis. Ulitmately, each script improved R and p-vals, simplified results, leading to simpler conclusions. For the NMDS plots at the end for visualization of these conclusions, I reset the community data to include 13 of 13 sites. This results in 2 distinct clusters of fish community composition. Arid streams contain G.affinis, P.formosa, and C. variegatus. These communities are evidently constrained by environmental variables: PPT, Hydro-Dist, and ammonia concentrations (colinear with pH). 