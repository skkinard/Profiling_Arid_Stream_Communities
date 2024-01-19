# Sean Kinard 2-6-20
# This script creates a site x invertebrate.data matrix and exports it to be used in an analysis of stream community structure across Texas Coastal Prairie using data collected from 13 streams in the Spring of 2017.

# Install relevant packages
install.packages("bio.infer")
library(bio.infer)

# Set Work Directory
setwd("C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats")

#Import raw invertebrate data
invert <- read.csv("/home/sean/Documents/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats/Kicknet.Survey.Spring2017.csv")

# Obtain Taxonomic units
# bio.infer
icnt.TX <- invert[,c(4,19,13)]
i_spec <- get.taxonomic(icnt.TX)
i_spec <- get.otu(i_spec)
invert <- makess(i_spec)

# Remove 3 heavily impacted sample sites: 8068450, 8189200, 8211520
invert <- invert[-c(4,10,12),]

# Generate Diversity and evenness metrics
invert$shannon <- diversity(invert[,2:96])
invert$rich <- specnumber(invert[,2:96])
(raremax <- min(rowSums(invert[,2:96])))
invert$rare.rich <- rarefy(invert[,2:96], raremax)

# Export invertebrate community by site and finalize formatting in Excel
write.csv(invert,"C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\TCP_invert_x_site.csv", row.names = FALSE)

# Formatting in excel included re-ordering columns and rows for easier viewing and coding in R later.