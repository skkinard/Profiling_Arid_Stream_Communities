# July 2019
# Spring 2017 Fish and Macroinvertebrate Communities along Precipitation Gradient
# Sean Kinard
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
(raremax <- min(rowSums(fmat[,2:23])))
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
sort.PC1 # LNG_GAGE, PPTAVG_BASIN, PPTAVG_SITE, WD_BASIN, RIP100_43, WD_SITE
sort.PC2 <- var.coord[order(var.coord$PC2),]
sort.PC2 # WDMAX_BASIN, RIP100_82, NITR_APP_KG_SQKM, PHOS_APP_KG_SQKM, RIP100_PLANT, NO3.NO2, turbidity.adjusted, conductivity, salinity, oxygen.saturation, dissolved.oxygen, pH
sort.PC3 <- var.coord[order(var.coord$PC3),]
sort.PC3 # HYDRO_DISTURB_INDX, PIE, NH3.N, RIP100_95, PO43, NO3.NO2
sort.PC4 <- var.coord[order(var.coord$PC4),]
sort.PC4 # Dissolved.oxygen, rich, shannon, RH_Basin

# It doesn't appear that OTU fix affects the PCA. The same variables compose each of the PCA in similar weights as before.

# Create trimmed data.frame using variables from PCA
trm.fish <- msterfish[,c("PIE", "rich", "shannon", "PPTAVG_SITE", "WD_BASIN", "RIP100_43", "WD_SITE", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PHOS_APP_KG_SQKM", "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3..N.LR.", "turbidity.adjusted", "conductivity", "salinity", "dissolved.oxygen", "pH", "NH3.N", "PO43.")]
head(trm.fish) #Remember that rownames are the USGS gauges

# Proceed to aug.pairs, glm, and NMDS
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/diagPlotsWithoutLMTest.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/traditionalForwardBackward_lm.R', encoding = 'UTF-8')

aug.pairs(trm.fish)
# linear regressions indidcate the following variables relate to diversity metrics: "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3.NO2", "pH", "NH3.N","PO43.", "dissolved.oxygen", "PPTAVG_SITE"

# Identified correlation > abs(0.4): "HYDRO_DISTURB_INDX", "RH_BASIN", "NO3.NO2", "pH", "NH3.N","PO43.", "dissolved.oxygen", "PPTAVG_SITE"

# Identified need to log transform: 
# RIP100_43
# NITR_APP_KG_SQKM
# NO3.NO2
# conductivity
# PO43.
colnames(trm.fish)

trm2.fish <-trm.fish[,c(1:4,11,12,17:18,6,9:10,13,20)]

trm2.fish$lg.RIP <- log10(trm.fish$RIP100_43)
trm2.fish$lg.NITR.A <- log10(trm.fish$NITR_APP_KG_SQKM)
trm2.fish$lg.PHOS.A <- log10(trm.fish$PHOS_APP_KG_SQKM)
trm2.fish$lg.NO3..N.LR. <- log10(trm.fish$NO3..N.LR.)
trm2.fish$lg.cond <- log10(trm.fish$conductivity)
trm2.fish$lg.PO43. <- log10(trm.fish$PO43.)

# NO3.NO2 contains negative values which do not log transform. Substitute LR nitrate values for NO3NO2.

# Evaluate log transformations
trm2.fish$lg.RIP <- trm2.fish$lg.RIP[c(1:6, NA, 8:10,NA, 12:13)]
aug.pairs(na.omit(trm2.fish)[,c(1:3,9:19)])

# Log transformations improve linear regressions. Replace true variables with log transformed and proceed to glms.
# Since there are missing values for lg.NO3..N.LR., I will exclude this variable from the glms rather than lose 2-3 features (sites)

trm3.fish <- trm2.fish[,-c(10:14,17)]
aug.pairs(trm3.fish)

# PIE = Probability of interspecies encounter
full.model.fish <- lm(PIE~ PPTAVG_SITE + HYDRO_DISTURB_INDX + RH_BASIN + dissolved.oxygen + pH + RIP100_43 + lg.PHOS.A + lg.cond + lg.PO43., data=trm3.fish)
summary(full.model.fish)
vif(full.model.fish) 
# A rule of thumb for interpreting the variance inflation factor:
# 1 = not correlated.
# Between 1 and 5 = moderately correlated.
# Greater than 5 = highly correlated.

# Removing OTU step has large impact on glm power:
# Residual standard error: 0.176 on 3 degrees of freedom
# Multiple R-squared:  0.9404,	Adjusted R-squared:  0.7618 
# F-statistic: 5.263 on 9 and 3 DF,  p-value: 0.09943
# Compare this to PIE full model with OTU step with adjusted R-Squared of 0.5506, with F-stat 2.47 on 10 and 2 DF, p-value: 0.3225


min.model<-lm(PIE~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:13])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)        -5.262184   2.676707  -1.966  0.09690 . 
# PPTAVG_SITE        -0.022166   0.005776  -3.838  0.00858 **
#   HYDRO_DISTURB_INDX  0.061253   0.012022   5.095  0.00223 **
#   RH_BASIN            0.139114   0.041653   3.340  0.01562 * 
#   RIP100_43          -0.105771   0.034719  -3.046  0.02261 * 
#   lg.cond            -1.055851   0.210338  -5.020  0.00240 **
#   lg.PO43.           -0.300731   0.136892  -2.197  0.07041 . 
#
# Residual standard error: 0.177 on 6 degrees of freedom
# Multiple R-squared:  0.8795,	Adjusted R-squared:  0.759 
# F-statistic:   7.3 on 6 and 6 DF,  p-value: 0.01448

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

# Analyzing model 9 from Dredge Fish
fish.model.17 <- lm(PIE~ HYDRO_DISTURB_INDX + pH + RIP100_43, data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
# Residual standard error: 0.2648 on 9 degrees of freedom
# Multiple R-squared:  0.5954,	Adjusted R-squared:  0.4605 
# F-statistic: 4.415 on 3 and 9 DF,  p-value: 0.03603

# Analyzing model 33 from Dredge Fish
fish.model.33 <- lm(PIE~ lg.PO43., data=trm3.fish)
summary(fish.model.33)
vif(fish.model.33)
# Residual standard error: 0.3445 on 11 degrees of freedom
# Multiple R-squared:  0.1628,	Adjusted R-squared:  0.08674 
# F-statistic:  2.14 on 1 and 11 DF,  p-value: 0.1715

# Summary of PIE glms: The backwards glm proved most effective in eplaining observed variance in PIE across sites. Specifically,  watershed characteristics (PPT, RH, Hydro-Dist, RIP %) are  explanatory variables for PIE. Conductivity is implicated as a significant predictor variable.

# shannon
full.model.fish <- lm(shannon~ PPTAVG_SITE + HYDRO_DISTURB_INDX + RH_BASIN + dissolved.oxygen + pH + RIP100_43 + lg.PHOS.A + lg.cond + lg.PO43., data=trm3.fish)
summary(full.model.fish)
vif(full.model.fish)
# Residual standard error: 0.2894 on 3 degrees of freedom
# Multiple R-squared:  0.936,	Adjusted R-squared:  0.7441 
# F-statistic: 4.878 on 9 and 3 DF,  p-value: 0.1096

min.model<-lm(shannon~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:13])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                     Estimate   Std. Error t value Pr(>|t|)   
# (Intercept)          -2.592205   1.345629  -1.926  0.09022 . 
# PPTAVG_SITE           0.017059   0.007874   2.167  0.06215 . 
# HYDRO_DISTURB_INDX   -0.063892   0.014144  -4.517  0.00196 **
#   RIP100_43           0.185055   0.052485   3.526  0.00778 **
#   lg.cond             0.943157   0.275368   3.425  0.00902 **
# 
# Residual standard error: 0.3056 on 8 degrees of freedom
# Multiple R-squared:  0.8098,	Adjusted R-squared:  0.7147 
# F-statistic: 8.516 on 4 and 8 DF,  p-value: 0.005546
# This model was also top 5 in the dredge!

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

# Analyzing model 17 from Dredge Fish
fish.model.17 <- lm(shannon~ lg.PO43., data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
# Residual standard error: 0.5168 on 11 degrees of freedom
# Multiple R-squared:  0.252,	Adjusted R-squared:  0.184 
# F-statistic: 3.706 on 1 and 11 DF,  p-value: 0.08047

fish.model.291 <- lm(shannon~ HYDRO_DISTURB_INDX + pH + RIP100_43, data=trm3.fish)
summary(fish.model.291)
vif(fish.model.291)
# Residual standard error: 0.3295 on 9 degrees of freedom
# Multiple R-squared:  0.7512,	Adjusted R-squared:  0.6682 
# F-statistic: 9.056 on 3 and 9 DF,  p-value: 0.004415

# Summary Shannon glms: Significant predictor variables include Hydro-disturbance-index, conductivity, and riparian %.


# Richness
full.model.fish <- lm(rich~ PPTAVG_SITE + HYDRO_DISTURB_INDX + RH_BASIN + dissolved.oxygen + pH + RIP100_43 + lg.PHOS.A + lg.cond + lg.PO43., data=trm3.fish)
summary(full.model.fish)
vif(full.model.fish)
# Residual standard error: 2.496 on 3 degrees of freedom
# Multiple R-squared:  0.7208,	Adjusted R-squared:  -0.1167 
# F-statistic: 0.8607 on 9 and 3 DF,  p-value: 0.6233

min.model<-lm(rich~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:13])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)   
# (Intercept)           -9.6222     5.5691  -1.728  0.11809   
# HYDRO_DISTURB_INDX    -0.1436     0.0655  -2.193  0.05599 . 
# RIP100_43              0.9474     0.2678   3.537  0.00634 **
#   lg.PHOS.A            5.9809     2.0981   2.851  0.01907 * 
#
# Residual standard error: 1.694 on 9 degrees of freedom
# Multiple R-squared:  0.6143,	Adjusted R-squared:  0.4857 
# F-statistic: 4.778 on 3 and 9 DF,  p-value: 0.02939

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

fish.model.17 <- lm(rich~ PPTAVG_SITE., data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
# Residual standard error: 0.5168 on 11 degrees of freedom
# Multiple R-squared:  0.252,	Adjusted R-squared:  0.184 
# F-statistic: 3.706 on 1 and 11 DF,  p-value: 0.08047

fish.model.281 <- lm(rich~ HYDRO_DISTURB_INDX + lg.PHOS.A + RIP100_43, data=trm3.fish)
summary(fish.model.281)
vif(fish.model.281)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)   
# (Intercept)           -9.6222     5.5691  -1.728  0.11809   
# HYDRO_DISTURB_INDX    -0.1436     0.0655  -2.193  0.05599 . 
# lg.PHOS.A              5.9809     2.0981   2.851  0.01907 * 
#   RIP100_43            0.9474     0.2678   3.537  0.00634 **
#
# Residual standard error: 1.694 on 9 degrees of freedom
# Multiple R-squared:  0.6143,	Adjusted R-squared:  0.4857 
# F-statistic: 4.778 on 3 and 9 DF,  p-value: 0.02939


# Summary Richness: variation in species richness among the sites is largely explained by Hydro-dist, riparian %, and fertilizer applications. PPT is weakly indicated from a dredge model.

#########################################################
#########################################################
# Fish glm conclusions:
# variation in PIE, SI, richness among the sites in the gradient is largely explained by riparian %, Hydrologic disturbance, and climate. Secondary predictors include phosphate application in the watershed and stream conductivity.

#These conclusions are different from the analysis that used simplified OTUs (genus-level).
#########################################################
#########################################################
# FISH NMDS
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:26)],trm3.fish, by= "USGS.gauge")


#Creating Ordinate hulls
nms1 <- metaMDS(ford[,c(2:26)], k=2, noshare=0.1)
plot(nms1)
plot(nms1, type="n")
points(nms1, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(nms1, display= "spec", cex=0.7, col="blue")




plot(nms1, disp = "sites", type = "n", main="Fish Ordination")
ordihull(nms1, groups = ford$USGS.gauge, lwd = 1)
ordiellipse(nms1, groups = fish$PPT_Type, col=c( "goldenrod1", "deepskyblue"), draw="polygon")
ordispider(nms1, groups = fish$PPT_Type, col=c( "goldenrod3", "deepskyblue3"), label = TRUE)

# adding points and labels
points(nms1, disp="sites", pch=21, col="red", bg="yellow", cex=0.8)
text(nms1, display = "spec", cex=0.5, col="black")

colnames(fish)
ord2 <- decorana(fish[12:33])
ord.fit <- envfit(ord2 ~ TEMP + PPT + PH + Phosphates + Ammonia, data=fish, perm=999, na.rm=TRUE)
ord.fit
plot(ord, dis="site")
plot(ord.fit)
text(nms1, display = "spec", cex=0.5, col="black")

#What's the best way to display an ordination plot with fitted environmental variables?
#The axis seem poor, and the overlapping text is a nightmare