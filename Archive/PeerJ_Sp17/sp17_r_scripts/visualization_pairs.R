# Sean Kinard
# 12-12-2020

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Data Visualization Using Rarified Richness
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")

source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')

# Merge Community x site with env x site
msterfish <- merge(fmat, emat, by = "STAID")
row.names(msterfish) <- msterfish$staid
msterfish <- Filter(is.numeric, msterfish)

# Overview of diversity vs environmental predictors
aug.pairs(msterfish[,c(3,23:32)], na.action=na.omit)
aug.pairs(msterfish[,c(3,33:42)], na.action=na.omit)
aug.pairs(msterfish[,c(3,43:47)], na.action=na.omit)

aug.pairs(msterinvert[,c(3,100:109)], na.action=na.omit)
aug.pairs(msterinvert[,c(3,110:119)], na.action=na.omit)
aug.pairs(msterinvert[,c(3,120:124)], na.action=na.omit)

# Visually identified need to log transform conductivity & NO3