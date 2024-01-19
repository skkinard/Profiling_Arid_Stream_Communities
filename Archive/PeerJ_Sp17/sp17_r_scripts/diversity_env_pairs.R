# pairs plots for environmental variables with fish shannon and invertebrate shannon
library(dplyr)
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')


fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")


# Scale data
emat[,-c(1,2)] <- scale(emat[,-c(1,2)])
# Merge Community x site with env x site
msterfish <- merge(fmat, emat, by = "STAID")
row.names(msterfish) <- msterfish$staid
msterfish <- Filter(is.numeric, msterfish)

msterinvert <- merge(imat,emat, by = "STAID")
row.names(msterinvert) <- msterinvert$staid
msterfish <- Filter(is.numeric, msterfish)

msterfish <- msterfish %>% rename(fish_Shannon = shannon)
msterinvert <- msterinvert %>% rename(invert_Shannon = shannon)


myvariables <- which(colnames(emat) == "STAID" |
                       colnames(emat) == "AP" |
                       colnames(emat) == "flash.index" |
                       colnames(emat) == "LFPP" |
                       colnames(emat) == "NH4."|
                       colnames(emat) == "log.cond"|
                       colnames(emat) == "canopy"|
                       colnames(emat) == "Rosgen.Index"
)

a <- msterinvert[,1:2]
b <- msterfish[,1:2]
c <- emat[,c(myvariables)]

pear_data <- merge(a,b, by = "STAID")
pear_data <- merge(pear_data, c, by = "STAID")
pear_data




# Create aug pairs plot
aug.pairs(pear_data[,c(3,2,4:9)], na.action=na.omit)
