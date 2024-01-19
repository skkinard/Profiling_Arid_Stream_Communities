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

# bio.infer
icnt.TX <- invert[,c(4,19,13)]
i_spec <- get.taxonomic(icnt.TX)
i_spec <- get.otu(i_spec)
invert <- makess(i_spec)

# Remove GC and OSO bc n < 100 indicates poor sample quality.
invert <- invert[-c(6,12),]

# Generate Diversity and evenness metrics
invert$shannon <- diversity(invert[,2:96])
invert$simpson <- diversity(invert[,2:96], index ="simpson")
invert$rich <- specnumber(invert[,2:96])

(raremax <- min(rowSums(invert[,2:96]))) # rarefaction is unclear here. What exactly is happening here?
invert$rare.rich <- rarefy(invert[,2:96], raremax)

# Intro Plots invert
msterinvert <- merge(invert, habitat[,-31], by = "USGS.gauge") # merge stream habitat data and diversity index data for invert
factor(gauge.data$USGS.gauge)
msterinvert <- merge(msterinvert, gauge.data, by = "USGS.gauge")
row.names(msterinvert) <- msterinvert$USGS.gauge
trm.invert <- msterinvert[,c("rare.rich", "shannon", "PPTAVG_SITE", "T_AVG_BASIN", "PRECIP_SEAS_IND", "PET", "NLCD01_06_DEV", "IMPNLCD06", "CDL_ALL_OTHER_LAND", "WDMAX_BASIN", "NITR_APP_KG_SQKM", "PESTAPP_KG_SQKM","RIP100_82", "turbidity.adjusted", "conductivity", "salinity", "dissolved.oxygen", "pH", "NH3.N", "PO43.", "NO3.NO2", "HYDRO_DISTURB_INDX")]
trm.invert$RIP100_82 <- c(0.000001, 0.000001,15.71,44.92,0.000001,0.10,0.27,0.77,10.24,2.73,2.73) # Preventing undefined values by substituting 0.000001 for 0.
trm.invert$RIP100_82 <- log10(trm.invert$RIP100_82)
trm.invert$NITR_APP_KG_SQKM <- log10(trm.invert$NITR_APP_KG_SQKM)
trm.invert$conductivity <- log10(trm.invert$conductivity)
trm.invert$NH3.N <- log10(trm.invert$NH3.N)
trm.invert$USGS.gauge <- row.names(trm.invert)

# run NMDS
rownames(ford) <- ford$USGS.gauge
ford <- merge(invert[,c(1:96)],trm.invert, by= "USGS.gauge")
ford <- ford[order(ford$PPTAVG_SITE),]
nms1 <- metaMDS(ford[,c(2:96)], k=2, noshare=0.1)
par(mfrow=c(1,1))
plot(nms1)
plot(nms1, type="n")
points(nms1, display = "sites", cex = 1.5, pch=21, col="red", bg="yellow")
text(nms1, display= "spec", cex=0.7, col="blue")
# This may be workable. There are too many species to see clearly what is going on here.

# heirarchical clustering identifies groupings of sites
clusters <- hclust(dist(ford[,97:99]), method="average")
plot(clusters)
clusterCut <- cutree(clusters, 3)
table(clusterCut, ford$PPTAVG_SITE)
ford$clusterCut <- clusterCut

# assign groups by hclust
par(mfrow=c(1,1))
ford <- ford[order(ford$PPTAVG_SITE),]
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_SITE,breaks = 10))]
plot(nms1, disp = "sites", type = "n", main="nmds.invert", ylim=c(-1.25,1.25), xlim=c(-1.25,1.25)) # Why won't my x and y axis adjust? Is it because my aspect ratio is locked? How do I change aspect ratio?
ordiellipse(nms1, groups = ford$clusterCut, draw="polygon", border = FALSE, kind = "ehull", col=c("blue","yellow"), alpha=.2)
# ordispider(nms1, groups = ford$groupWD, col=c("chocolate", "blue"), label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
ordihull(nms1, groups = ford$clusterCut, lwd = 1)
text(nms1, display = "spec", cex=0.4, col="black")

# Fit environmental Variables
colnames(ford)
ord2 <- decorana(ford[2:96])
ord.fit <- envfit(ord2 ~ RIP100_82 + PO43. + NH3.N + dissolved.oxygen + HYDRO_DISTURB_INDX + NITR_APP_KG_SQKM + PRECIP_SEAS_IND + PPTAVG_SITE, data=ford, perm=999, na.rm=TRUE)
ord.fit
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_SITE,breaks = 10))]
# ::::::::::::::::::::::::::::::::::::::::::::::::

par(mfrow=c(1,1))
ford <- ford[order(ford$PPTAVG_SITE),]
ford$Col <- rbPal(10)[as.numeric(cut(ford$PPTAVG_SITE,breaks = 10))]
plot(nms1, disp = "sites", type = "n", main="nmds.invert", ylim=c(-1.25,1.25), xlim=c(-1.25,1.25)) # Why won't my x and y axis adjust? Is it because my aspect ratio is locked? How do I change aspect ratio?
ordiellipse(nms1, groups = ford$clusterCut, draw="polygon", border = FALSE, kind = "ehull", col=c("blue","yellow"), alpha=.2)
# ordispider(nms1, groups = ford$groupWD, col=c("chocolate", "blue"), label = FALSE)
points(nms1, display = "sites", cex = 1.5, pch=21, col="black", bg=ford$Col)
ordihull(nms1, groups = ford$clusterCut, lwd = 1)
# text(nms1, display = "spec", cex=0.4, col="black")
plot(ord.fit, col="red", cex=0.7)

