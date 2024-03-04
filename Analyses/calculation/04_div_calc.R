# 04_div_calc
# Sean Kinard
# 07-19-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# The script begins by loading two datasets: one containing fish data and another containing invertebrate data. It then removes redundant columns (site.code and STAID) from the datasets. Next, the script computes fish diversity measures using the iNEXT package. It creates an iNEXT object for the fish data and generates rarefaction curves using ggiNEXT. It then extracts species richness, Shannon diversity, and Simpson diversity estimates from the iNEXT object and adds them as new columns to the fish dataset. Similarly, the script repeats the same process for invertebrate data, computing diversity measures, generating rarefaction curves, and extracting diversity estimates. After computing diversity measures for both fish and invertebrate datasets, the script exports the results to CSV files: fish_diversity_estimates.csv and invert_diversity_estimates.csv.

# In summary, the script calculates and visualizes diversity measures for fish and invertebrate communities, extracts and adds these measures to the respective datasets, and finally exports the datasets with the added diversity measures to CSV files for further analysis.

# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# Load Packages
library("writexl")
library(iNEXT)

# load data
fish <- read_csv("sp17_data_files/sp17_site_x_fish.csv")
fish_nosite <- select(fish, -site.code, -STAID)

# -----------------------------------------------------------------------------
# Fish diversity calculations
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Invert diversity calculations
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------
write_csv(as.data.frame(fishdiv),"sp17_data_files\\fish_diversity_estimates.csv")
write_csv(as.data.frame(invertdiv),"sp17_data_files\\invert_diversity_estimates.csv")

# -----------------------------------------------------------------------------
# End 04_div_calc