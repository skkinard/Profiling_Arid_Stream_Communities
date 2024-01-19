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

                                
# convert the original dataframe into a site x species matrix
benl <- fish[,c(3,15,16)] # Site.Cycle, Genus.Species, Abundance
names(benl) <- c("BenSampID", "FinalID", "Count")
bcnt.tax <- get.taxonomic(benl)
names(bcnt.tax)
bcnt.otu <- get.otu(bcnt.tax) # brings the data to the best common level (ie species or genus)
fmat <- makess(bcnt.otu) # site x species matrix
colnames(fmat)[colnames(fmat)=="BenSampID"] <- "USGS.gauge"# add back row labels

fmat$shannon <- diversity(fmat[,2:14])
fmat$simpson <- diversity(fmat[,2:14], index = "simpson")
fmat$rich <- specnumber(fmat[,2:14])

fmat$abun <- rowSums(Filter(is.numeric, fmat)) # Creating sample size for PIE
fmat$PIE <- (fmat$abun/(fmat$abun-1))*(1-fmat$simpson)

(raremax <- min(rowSums(fmat[,2:14])))
fmat$rare.rich <- rarefy(fmat[,2:14], raremax)
fmat$abun <- rowSums(fmat[,2:14])



# Intro Plots Fish
msterfish <- merge(fmat[,-c(2:14)], habitat[,-31], by = "USGS.gauge") # merge stream habitat data and diversity index data for fish
factor(gauge.data$USGS.gauge)

msterfish <- merge(msterfish, gauge.data, by = "USGS.gauge") # merge USGS, stream hab, and div. index data frames
colnames(msterfish)
row.names(msterfish) <- msterfish$USGS.gauge
msterfish <- Filter(is.numeric, msterfish)
#################################################################### 
# We lose TRC and CC during merge (identify important environmental variables with ordination, find alternative sources so that we can include these 2 sites in the final figure/model determination)
# Update 8.12.2019: Duplicated SFC gauge data for TRC. Fixed gauge number for Copano Creek. Now there are 13 of 13 sites with merged environmental variable data frames.
##########################################################

# Plot Diversity metrics vs precipitation
plot(msterfish$shannon ~ msterfish$PPTAVG_SITE)   # Slight
plot(msterfish$simpson ~ msterfish$PPTAVG_SITE)   # No
plot(msterfish$PIE ~ msterfish$PPTAVG_SITE)       # No
plot(msterfish$rich ~ msterfish$PPTAVG_SITE)      # Slight
plot(msterfish$rare.rich ~ msterfish$PPTAVG_SITE) # No
plot(msterfish$abun ~ msterfish$PPTAVG_SITE)      # No

# These plots do not look particularly promising.

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

# For the fish data, the best model, PIE full model has adjusted R-Squared of 0.5506, with F-stat 2.47 on 10 and 2 DF, p-value: 0.3225
# This indicates that there is poor fit, likely due to insufficient sample density across the gradient. It is likely that electrofishing was unreliable in locations like SFC and TRC. These are crucial data points representing the values at the most dry sites in the study. The VIF is poor, indicating colinearity. After trying full models with less variables, R and VIF decrease.

min.model<-lm(PIE~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:13])
summary(forward.model)
# formula = PIE ~ 1
# Residual standard error: 0.3169 on 12 degrees of freedom
# Multiple R-squared:  (not provided?)
# F-statistic:  (not provided?)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)         6.055390   1.109875   5.456  0.00158 **
#   PPTAVG_SITE         0.012012   0.004777   2.515  0.04560 * 
#   HYDRO_DISTURB_INDX  0.021934   0.007286   3.010  0.02369 * 
#   dissolved.oxygen    0.017804   0.005418   3.286  0.01669 * 
#   pH                 -0.710581   0.146950  -4.836  0.00289 **
#   RIP100_43          -0.222196   0.051991  -4.274  0.00524 **
#   lg.PHOS.A          -1.070325   0.307184  -3.484  0.01307 * 
# Residual standard error: 0.1636 on 6 degrees of freedom
# Multiple R-squared:  0.8667,	Adjusted R-squared:  0.7334 
# F-statistic: 6.501 on 6 and 6 DF,  p-value: 0.01921

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

# Analyzing model 9 from Dredge Fish
fish.model.69 <- lm(PIE~ pH + lg.cond, data=trm3.fish)
summary(fish.model.69)
vif(fish.model.69)
# Residual standard error: 0.281 on 10 degrees of freedom
# Multiple R-squared:  0.3448,	Adjusted R-squared:  0.2138 
# F-statistic: 2.631 on 2 and 10 DF,  p-value: 0.1208

# Analyzing model 33 from Dredge Fish
fish.model.33 <- lm(PIE~ lg.PO43., data=trm3.fish)
summary(fish.model.33)
vif(fish.model.33)
# Residual standard error: 0.2951 on 11 degrees of freedom
# Multiple R-squared:  0.2052,	Adjusted R-squared:  0.133 
# F-statistic: 2.841 on 1 and 11 DF,  p-value: 0.12

# Summary of PIE glms: The backwards glm proved most effective in eplaining observed variance in PIE across sites. Specifically, water quality paramaeters (DO & cond) and watershed characteristics (RIP % & Fertilizer application) are strong explanatory variables for PIE. There is LOW concern for Variance Inflation due to colinearity (backwards vif <5). Simpler models with fewere variables provide insufficient correlations to merit discussion.

# shannon
full.model.fish <- lm(shannon~ PPTAVG_SITE + HYDRO_DISTURB_INDX + RH_BASIN + dissolved.oxygen + pH + RIP100_43 + lg.PHOS.A + lg.cond + lg.PO43., data=trm3.fish)
summary(full.model.fish)
vif(full.model.fish)
#Residual standard error: 0.32 on 3 degrees of freedom
#Multiple R-squared:   0.85,	Adjusted R-squared:  0.4001 
#F-statistic: 1.889 on 9 and 3 DF,  p-value: 0.3267

min.model<-lm(shannon~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:13])
summary(forward.model)
# formula = shannon ~ lg.PO43
# Residual standard error: 0.333 on 11 degrees of freedom
# Multiple R-squared:  0.4045,	Adjusted R-squared:  0.3503 
# F-statistic: 7.471 on 1 and 11 DF,  p-value: 0.01946

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)        -2.54915    1.04616  -2.437  0.04078 * 
#   HYDRO_DISTURB_INDX -0.05122    0.01206  -4.249  0.00280 **
#   RIP100_43           0.20533    0.05063   4.055  0.00366 **
#   lg.PHOS.A           0.76691    0.32408   2.366  0.04550 * 
#   lg.cond             0.64354    0.19436   3.311  0.01068 * 

# Residual standard error: 0.2585 on 8 degrees of freedom
# Multiple R-squared:  0.739,	Adjusted R-squared:  0.6085 
# F-statistic: 5.664 on 4 and 8 DF,  p-value: 0.01835

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

# Analyzing model 17 from Dredge Fish
fish.model.17 <- lm(shannon~ lg.PO43., data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
# Residual standard error: 0.333 on 11 degrees of freedom
# Multiple R-squared:  0.4045,	Adjusted R-squared:  0.3503 
# F-statistic: 7.471 on 1 and 11 DF,  p-value: 0.01946

fish.model.291 <- lm(shannon~ HYDRO_DISTURB_INDX + pH + RIP100_43, data=trm3.fish)
summary(fish.model.291)
vif(fish.model.291)
# Residual standard error: 0.2718 on 9 degrees of freedom
# Multiple R-squared:  0.6754,	Adjusted R-squared:  0.5672 
# F-statistic: 6.243 on 3 and 9 DF,  p-value: 0.01401

# Summary Shannon: Shannon diversity can be considerably (R > .4, p val < .05) explained by phosphate concentrations alone as indicated by the forward and dredge models. Other predictor variables include Hydro-disturbance-index, pH, and riparian %. No concerns for VIF evident.


# Richness
full.model.fish <- lm(rich~ PPTAVG_SITE + HYDRO_DISTURB_INDX + RH_BASIN + dissolved.oxygen + pH + RIP100_43 + lg.PHOS.A + lg.cond + lg.PO43., data=trm3.fish)
summary(full.model.fish)
vif(full.model.fish)

min.model<-lm(rich~1,data=trm3.fish)
forward.model <- traditional.forward(min.model,trm3.fish[,4:13])
summary(forward.model)

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)
vif(backward.model)
# oefficients:
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)        -12.0328     4.5119  -2.667   0.0285 * 
#   HYDRO_DISTURB_INDX  -0.1335     0.0520  -2.567   0.0333 * 
#   RIP100_43            0.7992     0.2184   3.660   0.0064 **
#   lg.PHOS.A            4.2862     1.3977   3.067   0.0154 * 
#   lg.cond              1.7906     0.8383   2.136   0.0652 .
#
# Residual standard error: 1.115 on 8 degrees of freedom
# Multiple R-squared:  0.6581,	Adjusted R-squared:  0.4871 
# F-statistic: 3.849 on 4 and 8 DF,  p-value: 0.04964

options(na.action = "na.fail")
dredge(full.model.fish)
options(na.action = "na.omit")

fish.model.17 <- lm(rich~ lg.PO43., data=trm3.fish)
summary(fish.model.17)
vif(fish.model.17)
# Residual standard error: 1.411 on 11 degrees of freedom
# Multiple R-squared:  0.2466,	Adjusted R-squared:  0.1781 
# F-statistic:   3.6 on 1 and 11 DF,  p-value: 0.08434

fish.model.281 <- lm(rich~ lg.PO43. + lg.PHOS.A + RIP100_43, data=trm3.fish)
summary(fish.model.281)
vif(fish.model.281)
# Residual standard error: 1.082 on 9 degrees of freedom
# Multiple R-squared:  0.6376,	Adjusted R-squared:  0.5169 
# F-statistic: 5.279 on 3 and 9 DF,  p-value: 0.0225

fish.model.127 <- lm(rich~ pH, data=trm3.fish)
summary(fish.model.127)
vif(fish.model.127)
# Residual standard error: 1.537 on 11 degrees of freedom
# Multiple R-squared:  0.1061,	Adjusted R-squared:  0.02484 
# F-statistic: 1.306 on 1 and 11 DF,  p-value: 0.2774

# Summary Richness: variation in species richness among the sites is largely explained by phosphate concentrations, fertilizer application, and riparian %. See fish.model.281 which greatly improved the single variable model.17.

#########################################################
#########################################################
# Fish glm conclusions:
# variation in PIE, SI, richness among the sites in the gradient are driven primarily by phosphate concentrations, riparian %, and are secondarily affected by dissolved O2 concentrations, and Hydrological-Disturbance-Index.
#########################################################
#########################################################
# FISH NMDS
trm3.fish$USGS.gauge <- rownames(trm3.fish)
ford <- merge(fmat[,c(1:14)],trm3.fish, by= "USGS.gauge")

#Creating Ordinate hulls
nms1 <- metaMDS(ford[,c(2:14)], k=2)
plot(nms1, disp = "sites", type = "n", main="Fish Ordination")
ordihull(nms1, groups = mster$Code, lwd = 1)
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

































# Invert
benl <- invert[,c(3,12,13)] # Site.Cycle, Genus.Species, Abundance
names(benl) <- c("BenSampID", "FinalID", "Count")
bcnt.tax <- get.taxonomic(benl)
names(bcnt.tax)
bcnt.otu <- get.otu(bcnt.tax) # brings the data to the best common level (ie species or genus)
imat <- makess(bcnt.otu) # site x species matrix
colnames(imat)[colnames(imat)=="BenSampID"] <- "USGS.gauge" # add back row labels
imat$shannon <- diversity(imat[,2:88])
imat$simpson <- diversity(imat[,2:88], index = "simpson")
imat$PIE <- (imat$abun/(imat$abun-1))*(1-fmat$simpson)
imat$rich <- specnumber(imat[,2:88])
(raremax <- min(rowSums(imat[,2:88])))
imat$rare.rich <- rarefy(imat[,2:88], raremax)
imat$abun <- rowSums(imat[,2:88])

                                ##################################################################
                                # OK so now we need some time series plots for each variable
 mster <- merge(unique(f_abundance[,c(1,2,3,4,5,6,7,8)]), ss4,by = "Site.Cycle") 
                                # does unique() remove or merge duplicate rows? This merge is to combine date with our site x species matrix
                                # it is imperative not to include colums from f_abundance that have unique values within each cycle
                                # Create a timeline vector
                                mster$Date <- apply(mster[,c('Year','Month','Day')],1,paste,sep='',collapse='-') # clarify this function. As far as I can tell, apply() is taking 3 columns, ?, pasting the values separated by a space which is then replaced by a dash?
                                mster$Date <- as.Date(mster$Date)
                                #######################################################
                                
                                mster$lnabun <- log(mster$abun)
                                #qplot(Date,lnabun,data=mster,geom='line',color=SiteCode)
                                
                                nms1 <- metaMDS(mster[,9:42], k=2,) # NO CONVERGENCE ASK CHRIS How to plot sequential NMDS
                                plot(nms1, disp = "sites", type = "n")
                                
                                #ordiellipse(nms1, mster$Cycle, col=1:9, kind = "ehull", lwd=2)
                                #ordispider(nms1, mster$Cycle, col=1:9, label = TRUE)
                                #Site.Cycle points
                                #points(nms1, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)
                                #Species names
                                #text(nms1, display = "spec", cex=0.5, col="blue") 
                                #Cycle #s
                                #text(nms1$points[,1], nms1$points[,2], pos = 2, mster$Cycle,col=1:9,) 
                                # NMDS Plot of f_abundance communities at 9 Coastal Texas sites from Spring 2017-Fall 2018 with Hurricane Disturbance 8/27/2017
                                
                                #######################################################
                                #Below is a Mann kendall test which identifies montonic trends underlying the data. It requires at least 4 data points. We only have 3 Cycles QaQc'd
                                install.packages("rkt")
                                library(rkt)
                                library("rkt", lib.loc="~/R/win-library/3.5")
                                site <- unique(mster$Code)
                                out <- as.data.frame(matrix(0,9,3))
                                names(out) <- c("SiteCode", "Slope", "Pval")
                                out$SiteCode <- site
                                for(x in 1:9){
                                  f1 <- rkt(mster[mster$Code == site[x],]$Cycle, mster[mster$Code == site[x],]$lnabun)
                                  out[x,3] <- f1[[1]]
                                  out[x,2] <-f1[[3]]
                                }
                                
                                out <- as.data.frame(matrix(0,9,3))
                                names(out) <- c("SiteCode", "Slope", "Pval")
                                out$SiteCode <- site
                                for(x in 1:9){
                                  f1 <- lm( mster[mster$Code == site[x]& mster$Cycle < 6,]$lnabun ~ mster[mster$Code == site[x] & mster$Cycle < 6,]$Cycle)
                                  out[x,3] <- summary(f1)$coeff[2,4]
                                  out[x,2] <-summary(f1)$coeff[2,1]
                                }
                                #######################################################
                                
                                # T-test of abundance differences between cycles
                                # We should to paired- t-test since the same sites were measure through time. We are testing the significance of site-dependent shifts in abundance.
                                # merge created unequal vectors of abundance per site.cycle from replication to fit species matrix
                                # solution: Create a data frame that I can run my t tests on. I'm running out of time so I will do this in excel and ask Chris at the meeting
                                install.packages("xlsx")
                                # library(xlsx)
                                library("xlsx", lib.loc="~/R/win-library/3.5")
                                write.xlsx(mster, "C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Data Analysis\\SKK_Fish_Workspace\\HHF_R_Stats\\HHF_mster.xlsx")
                                
                                # I deleted the summed coloums with less than 10 samples because they mess with the NMDS. I will try again below
                                library(vegan)
                                
                                mster <- HHF_mster <- read.csv("HHF_mster.csv", fileEncoding="UTF-8-BOM")
                                nms1 <- metaMDS(mster[,10:33], k=2,) 
                                #NMDS
                                plot(nms1, disp = "sites", type = "n") # I can't get xlim or ylim to work on this plot
                                
                                #ordiellipse(nms1, mster$Cycle, col=1:9, kind = "ehull", lwd=2)
                                #ordispider(nms1, mster$Cycle, col=1:9, label = TRUE)
                                ordihull(nms1, mster$Site, col=1:9, label = TRUE)
                                ordispider(nms1, mster$Site, col=1:9, label = TRUE)
                                ordiellipse(nms1, mster$Site, col=1:9, kind = "ehull", lwd=1)
                                #Site.Cycle points
                                #points(nms1, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)
                                #Species names
                                text(nms1, display = "spec", cex=0.5, col="blue")
                                ordilabel(nms1,dis="spec", cex=1.0, col= "blue")
                                #Cycle #s
                                text(nms1$points[,1], nms1$points[,2], pos = 2, mster$Code,col=1:9,) 
                                # NMDS Plot of f_abundance communities at 9 Coastal Texas sites from Spring 2017-Fall 2018 with Hurricane Disturbance 8/27/2017
                                
                                #This plot is ok
                                # Plots over time were highly overlapping
                                # Plots by site are more useful to look at
                                # SFC, TRC, and MR are dinstinctly seperate from the other sites whihc mostly overlap
                                # There is sedondary stratificaiton among the more humid sites (vertical separation)
                                # The presence of Anchovies and Mullet are uniqe to MR due to distance to coast
                                # The arid streams contain unique species: Mexian Tetra, Sheepshead minnows, Sailfin Mollies
                                # Humid Sites contain unique species: Pirate Perch, Slough Darters, several catfishes, Shiners (Wee and Black Tail), small mouth bass, gizzard shad
                                # Common species include gambusia, sunfish, and rio grand cichlids
                                
                                #######################################################
                                # conduct t-tests for sig dif of diversity index values from one cycle to the next
                                
                                t.test(mster[mster$Cycle == 2,]$abun, mster[mster$Cycle == 3,]$abun, paired = TRUE) 
                                # time 1 verse 2 is significant: t = -3.6539, df = 8, p-value = 0.006458
                                t.test(mster[mster$Cycle == 2,]$abun, ss4[mster$Cycle == 4,]$abun, paired = TRUE)  
                                # Time 2 vs Time 3 is not significant: t = 1.0322, df = 8, p-value = 0.3322
                                
                                t.test(mster[mster$Cycle == 2,]$shannon, mster[mster$Cycle == 3,]$shannon, paired = TRUE) 
                                #ns: t = -2.2928, df = 8, p-value = 0.05104
                                t.test(mster[mster$Cycle == 2,]$simpson, mster[mster$Cycle == 3,]$simpson, paired = TRUE)
                                #ns: t = -2.2182, df = 8, p-value = 0.05734
                                t.test(mster[mster$Cycle == 2,]$rare.rich, mster[mster$Cycle == 3,]$rare.rich, paired = TRUE)
                                
                                #####################################################
                                # calculate the deviation from mean ln abundance divided by mean ln abundance for each sampling date
                                cyc <- c(2:12)
                                for (x in 1:9){
                                  nab <- mean(mster[mster$Code == site[x],]$lnabun)
                                  for (y in 2:12){
                                    mster[(x-1)*4+y,74]  <- (mster[mster$Code == site[x] & mster$Cycle == cyc[y],]$lnabun -mnab)/mnab
                                  }
                                }
                                qplot(Date,nlnabun,data=mster,geom='line',color=Code)
                                mster$Cycle1 <- as.factor(mster$Cycle)
                                
                                
                                f1 <- lme(lnabun ~ Cycle1, data = mster, random = ~1|Code, corr = corAR1(form = ~1|Code))
                                anova(f1)
                                # output
                                # numDF denDF  F-value p-value
                                # (Intercept)     1    16 736.9055  <.0001
                                # Cycle1          2    16   9.8930  0.0016
                                # Interpretation: sig dif of ln abun among cycles
                                summary(glht(f1, linfct=mcp(Cycle1 = "Tukey")))
                                # Output: Linear Hypotheses:
                                #              Estimate Std. Error    z value  Pr(>|z|)    
                                #  2 - 1 == 0   1.3575       0.3119   4.353    < 1e-04 ***
                                #  3 - 1 == 0   1.1564       0.3568   3.241    0.00337 ** 
                                #  3 - 2 == 0  -0.2011       0.3119  -0.645    0.79443    
                                # Interpretation: Cycle 2 and 3 log abundance is significantly higher than Cycle 1
                                # How does our analysis benefit from a log transformation of abundance?
                                f2 <- lme(simpson ~ Cycle1, data = mster, random = ~1|SiteCode, corr = corAR1(form = ~1|SiteCode))
                                anova(f2)
                                # Output
                                #               numDF denDF   F-value     p-value
                                # (Intercept)     1    16     159.27278   <.0001
                                # Cycle1          2    16     2.84326     0.0878
                                # Interpretation: Sig dif of alpha diversity among cycles
                                summary(glht(f1, linfct=mcp(Cycle1 = "Tukey")))
                                # Output:
                                #              Estimate    Std. Error  z value   Pr(>|z|)    
                                #  2 - 1 == 0   1.3575     0.3119      4.353     < 0.001 ***
                                #  3 - 1 == 0   1.1564     0.3568      3.241     0.00338 ** 
                                #  3 - 2 == 0  -0.2011     0.3119     -0.645     0.79443    
                                # Interpretation: Simpson index of diversity is sig higher for Cycle 2 and 3 compared to Cycle 1
                                plot(lnabun ~ Date, data = mster)
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "TRC",], bg="red", pch=21, col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "SFC",], bg="orangered", pch=21, col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "AR",], pch=21, bg="orange",col="black" )
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "PDC",], pch=21, bg="yellow",col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "MR",], pch=21, bg="green3",col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "GC",],  pch=21, bg="green2",col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "PLC",],  pch=21, bg="green",col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "WMC",],  pch=21, bg="blue",col="black")
                                points(lnabun ~ Date, data = mster[mster$SiteCode == "EMC",],  pch=21, bg="blue3",col="black")
                                # dip in october vs september. Investigate outliers for method-based dip in CPUE? Sig rise from C1 to C2, small dip from C2 to C3 in abun.
                                
                                dateh <- as.data.frame(matrix(0,3,3))
                                names(dateh) <- c("Year", "Month","Day")
                                dateh[,1] <- c(2017,2017,2017)
                                dateh[,2]<- c(9,11,12)
                                dateh[,3] <- c(25,5,4)
                                dateh$Date <- apply(dateh[,c('Year','Month','Day')],1,paste,sep='',collapse='-')
                                dateh$Date <- as.Date(dateh$Date)
                                dateh$mlnab <- c(-0.1869, 0.05052, 0.08195)
                                dateh$stnlnab <- c(0.046089424,0.03675683,0.061843312)
                                
                                lines(mlnab ~ Date, data=dateh, lwd=2)
                                arrows(dateh$Date, dateh$mlnab-dateh$stnlnab, dateh$Date, dateh$mlnab+dateh$stnlnab, length=0.05, angle=90, code=3)
                                
                                adonis(mster[,8:36] ~ SiteCode*Cycle, data=mster, permutation=99)
                                # Output:
                                # Call:
                                #  adonis(formula = mster[, 8:36] ~ SiteCode * Cycle, data = mster,      permutations = 99) 
                                #
                                #Permutation: free
                                #Number of permutations: 99
                                #
                                #Terms added sequentially (first to last)
                                #
                                #                  Df   SumsOfSqs MeanSqs   F.Model      R2      Pr(>F)   
                                # SiteCode         8    4.2553    0.53192   3.2487     0.53169   0.01 **
                                #  Cycle           1    0.7155    0.71553   4.3701     0.08940   0.01 **
                                #  SiteCode:Cycle  8    1.5590    0.19487   1.1902     0.19479   0.19   
                                #Residuals         9    1.4736    0.16373              0.18412          
                                #Total             26   8.0034                         1.00000          
                                #--- 
                                #  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
                                #
                                # Interpretation:
                                # adonis is Analysis of variance using distance matrices. We used species matrix for our distance matrix
                                # The community distances have R2 .53 between sites (p<.01). Cycle correlation with distance based on species matrix has R2 of .09 with pval<.01.
                                # Geographic differences correlate stronger with observed community differences than temporal differences.
                                # Is there anything else we can get from this PerMANOVA?
                                pairwise.adonis(mster[,8:36], factors = mster$SiteCode)
                                #output
                                #pairs   F.Model         R2 p.value p.adjusted sig
                                #1   AR vs EMC 3.6251410 0.47541954     0.1          1    
                                #2    AR vs GC 1.8514470 0.31640840     0.1          1    
                                #3    AR vs MR 2.7956253 0.41138603     0.1          1    
                                #4   AR vs PDC 2.9644713 0.42565633     0.2          1    
                                #5   AR vs PLC 1.0194353 0.20309760     0.4          1    
                                #6   AR vs SFC 4.2009133 0.51224945     0.1          1    
                                #7   AR vs TRC 6.0705415 0.60280190     0.1          1    
                                #8   AR vs WMC 1.0747795 0.21178842     0.5          1    
                                #9   EMC vs GC 1.2508874 0.23822399     0.4          1    
                                #10  EMC vs MR 2.0152437 0.33502279     0.1          1    
                                #11 EMC vs PDC 4.7933415 0.54511036     0.1          1    
                                #12 EMC vs PLC 1.6240824 0.28877286     0.2          1    
                                #13 EMC vs SFC 5.5474636 0.58104056     0.1          1    
                                #14 EMC vs TRC 6.4622299 0.61767233     0.1          1    
                                #15 EMC vs WMC 1.0721248 0.21137586     0.5          1    
                                #16   GC vs MR 1.5211112 0.27550817     0.1          1    
                                #17  GC vs PDC 1.4339022 0.26388075     0.2          1    
                                #18  GC vs PLC 1.0479175 0.20759402     0.5          1    
                                #19  GC vs SFC 3.5365436 0.46925272     0.1          1    
                                #20  GC vs TRC 3.9738018 0.49835724     0.1          1    
                                #21  GC vs WMC 0.3493742 0.08032747     0.5          1    
                                #22  MR vs PDC 2.7295038 0.40560253     0.1          1    
                                #23  MR vs PLC 1.6424723 0.29109089     0.1          1  
                                #24  MR vs SFC 0.9974800 0.19959660     0.5          1    
                                #25  MR vs TRC 1.0950625 0.21492622     0.5          1    
                                #26  MR vs WMC 1.3723718 0.25544990     0.2          1    
                                #27 PDC vs PLC 2.8035271 0.41206966     0.1          1    
                                #28 PDC vs SFC 5.2074986 0.56557148     0.1          1    
                                #29 PDC vs TRC 6.1338172 0.60528201     0.1          1    
                                #30 PDC vs WMC 1.3383749 0.25070830     0.2          1    
                                #31 PLC vs SFC 4.3389585 0.52032379     0.1          1    
                                #32 PLC vs TRC 4.9818839 0.55465913     0.1          1    
                                #33 PLC vs WMC 0.5175928 0.11457269     0.8          1    
                                #34 SFC vs TRC 1.5123863 0.27436145     0.3          1    
                                #35 SFC vs WMC 3.0870511 0.43559035     0.1          1    
                                #36 TRC vs WMC 3.5710856 0.47167418     0.1          1 
                                
                                # Interpretation: No pval <.05. Highest R2 by sites are ~.60 involving pairwise comparrisons between PDC and TRC and AR vs TRC and EMC vs SFC.
                                
                                pairwise.adonis(mster[,8:36], factors = mster$Cycle)
                                # Output
                                #    pairs  F.Model         R2     p.value  p.adjusted sig
                                # 1 1 vs 2 2.213256 0.12151897   0.040       0.120    
                                # 2 1 vs 3 2.352984 0.12820715   0.025       0.075    
                                # 3 2 vs 3 0.589230 0.03551883   0.777       1.000
                                
                                #Interpretation: sig dif between 1 and 2, 1 and 3, but not 2 and 3. P adjusted suggests that there is not sig diff based on pairwise comparrison of cycle 1 and 2.
                                
                                # creating a vectorized pairwise comparison chart
                                temp <- unlist(as.data.frame(as.matrix(vegdist(mster[,8:36]))))
                                temp2 <- temp[temp > 0]
                                # creating vectors for the table
                                latvec <- 1
                                latv <- mster$Cycle
                                for (x in 1:length(mster[,1])){
                                  latvec <- c(latvec,rep(latv[[x]],(length(mster[,1])-1)))
                                }
                                latvec2 <- 1
                                for (x in 1:length(mster[,1])){
                                  latvec2 <- c(latvec2,latv[-x])
                                }
                                sitevec <- 1
                                sitev <- as.character(mster$SiteCode)
                                for (x in 1:length(mster[,1])){
                                  sitevec <- c(sitevec,rep(sitev[[x]],(length(mster[,1])-1)))
                                }
                                sitevec2 <- 1
                                for (x in 1:length(mster[,1])){
                                  sitevec2 <- c(sitevec2,sitev[-x])
                                }
                                cycle.compare <- as.data.frame(cbind(temp2,sitevec[-1],sitevec2[-1],latvec[-1],latvec2[-1]))
                                
                                # selecting the "within site" comparisons
                                cco <- cycle.compare[cycle.compare$V2 == cycle.compare$V3,]
                                
                                # creating a new column for unique comparisons
                                cco$comp <- paste(cco$V4,".",cco$V5)
                                cco$temp2 <- as.numeric(paste(cco$temp2))
                                
                                cco1 <- cco[cco$comp == "1 . 2"| cco$comp =="1 . 3"| cco$comp =="2 . 3",]
                                
                                summary(aov(temp2 ~ comp, cco1))
                                # Output
                                #              Df   Sum Sq  Mean Sq   F value  Pr(>F)  
                                # comp         2   0.2845   0.14227   2.748    0.0842 .
                                # Residuals   24   1.2424   0.05176 
                                # Interpretation: Compositional shifts by pairwise comparrisons of cycles 1-3 is not sig with pval=.08
                                
                                TukeyHSD(aov(temp2 ~ comp, cco1)) 
                                # Output:
                                # Tukey multiple comparisons of means
                                # 95% family-wise confidence level
                                #
                                # Fit: aov(formula = temp2 ~ comp, data = cco1)
                                #
                                # $`comp`
                                #                   diff        lwr        upr     p adj
                                # 1 . 3-1 . 2  0.007715443 -0.2601269 0.27555778 0.9971514
                                # 2 . 3-1 . 2 -0.213810609 -0.4816529 0.05403173 0.1354888
                                # 2 . 3-1 . 3 -0.221526052 -0.4893684 0.04631629 0.1185342
                                #Interpretation: No sig differences among pairwise comparrisons of compositional shifts cycle 1-3
                                boxplot(temp2 ~ comp, cco1)
                                # Pairwise comparrisons of compositions of f_abundance cycle 1-3 indicate evidence of shift 1 vs 3.
                                
                                