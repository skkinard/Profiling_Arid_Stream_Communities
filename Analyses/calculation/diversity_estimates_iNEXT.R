# Sean Kinard
# 07-19-2021
# Spring 17 Texas Coastal Prairie

# Load Packages
library("writexl")
library(iNEXT)

# Fish diversity calculations

fish <- read_csv("sp17_data_files/sp17_site_x_fish.csv")
fish_nosite <- select(fish, -site.code, -STAID)

i_fish <- iNEXT(as.data.frame(t(fish_nosite))) #create iNEXT object
ggiNEXT(i_fish) #plot rarefaction curves

#Extract Hill numbers from iNEXT object
fish$rich <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Species richness")),3]
fish$rich.lower <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Species richness")),6]
fish$rich.upper <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Species richness")),7]

fish$shannon <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Shannon diversity")),3]
fish$shannon.lower <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Shannon diversity")),6]
fish$shannon.upper <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Shannon diversity")),7]

fish$simpson <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Simpson diversity")),3]
fish$simpson.lower <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Simpson diversity")),6]
fish$simpson.upper <- i_fish$AsyEst[c(which(i_fish$AsyEst$Diversity == "Simpson diversity")),7]

fishdiv <- select(fish, STAID, rich:simpson.upper)

write_csv(as.data.frame(fishdiv),"sp17_data_files\\fish_diversity_estimates.csv")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Invert diversity calculations

invert <- read_csv("sp17_data_files/sp17_site_x_invert.csv")
invert_nosite <- select(invert, -STAID)

i_invert <- iNEXT(as.data.frame(t(invert_nosite))) #create iNEXT object
ggiNEXT(i_invert) #plot rarefaction curves

#Extract Hill numbers from iNEXT object
invert$rich <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Species richness")),3]
invert$rich.lower <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Species richness")),6]
invert$rich.upper <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Species richness")),7]

invert$shannon <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Shannon diversity")),3]
invert$shannon.lower <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Shannon diversity")),6]
invert$shannon.upper <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Shannon diversity")),7]

invert$simpson <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Simpson diversity")),3]
invert$simpson.lower <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Simpson diversity")),6]
invert$simpson.upper <- i_invert$AsyEst[c(which(i_invert$AsyEst$Diversity == "Simpson diversity")),7]

invertdiv <- select(invert, STAID, rich:simpson.upper)

write_csv(as.data.frame(invertdiv),"sp17_data_files\\invert_diversity_estimates.csv")

