# Set Work Directory
setwd("C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats")
# Import Data
library(readr)
climate <- read.csv("conterm_climate.txt")
bas_classif <- read.csv("conterm_bas_classif.txt")
basinid <- read.csv("conterm_basinid.txt")
lc06_rip100 <- read.csv("conterm_lc06_rip100.txt")
nutrient_app <- read.csv("conterm_nutrient_app.txt")
lc_crops <- read.csv("conterm_lc_crops.txt")
pest_app <- read.csv("conterm_pest_app.txt")
pop_infrastr <- read.csv("conterm_pop_infrastr.txt")
#Extract STAIDs
# AR  08189700
# BBW 08068390
# BC  08115000
# CC  08189200
# GC  08164600
# MC  08189300
# MR  08189500
# OSO 08211520
# PBW 08068450
# PDC 08177300
# PLC 08164800
# SFC 08211900
# TRC 08212300
as.factor(climate$STAID)
climate <- subset(climate, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(bas_classif$STAID)
bas_classif <- subset(bas_classif, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(basinid$STAID)
basinid <- subset(basinid, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(lc06_rip100$STAID)
lc06_rip100 <- subset(lc06_rip100, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(nutrient_app$STAID)
nutrient_app <- subset(nutrient_app, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(lc_crops$STAID)
lc_crops <- subset(lc_crops, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(pest_app$STAID)
pest_app <- subset(pest_app, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
as.factor(pop_infrastr$STAID)
pop_infrastr <- subset(pop_infrastr, STAID %in% c("8189700" , "8068390" , "8115000" , "8189200" , "8164600" , "8189300" , "8189500" , "8211520" , "8068450", "8177300", "8164800", "8211900", "8212300"))
# There are 12/13 STAIDs present: TRC (821300) is not Present in Gauges II.

# create Gauges II concotentated data frame
MyMerge <- function(x, y){
  df <- merge(x, y, by= "STAID", all.x= TRUE, all.y= TRUE)
  return(df)
}
gauges <- Reduce(MyMerge, list(climate,bas_classif, basinid, lc06_rip100, nutrient_app, lc_crops, pest_app, pop_infrastr))
library("xlsx", lib.loc="~/R/win-library/3.5")
write.xlsx(gauges, "C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\Sp17_gaugesconcotenated.xlsx")
##########################################################
# In Excel: Duplicate SFC (08211900) and assign TRC (08212300) gauge since TRC is in SFC watershed unit. In the data analysis, merge gauges with on-site habitat data so that TRC contains SFC gauge data and TRC on-site habitat data.
# Rename STAID "USGS.gauge"