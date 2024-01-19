# Sean Kinard 2-7-20
# This script creates a site x fish_community matrix and exports it to be used in an analysis of stream community structure across Texas Coastal Prairie using data collected from 13 streams in the Spring of 2017.

# Import Data Files
fmat <- read.csv("Spring2017_Electrofish_SiteSpeciesMatrix.csv")

# Generate Diversity and evenness metrics
fmat$shannon <- diversity(fmat[,3:20])
fmat$simpson <- diversity(fmat[,3:20], index = "simpson")
fmat$rich <- specnumber(fmat[,3:20])
(raremax <- min(rowSums(fmat[,3:20]))) # rarefaction is unclear here. What exactly is happening here?
fmat$rare.rich <- rarefy(fmat[,3:20], raremax)
fmat$abun <- rowSums(fmat[,3:20])

# Export data frame and re-order some rows and columns in excel
write.csv(fmat,"C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\TCP_fish_x_site.csv", row.names = FALSE)