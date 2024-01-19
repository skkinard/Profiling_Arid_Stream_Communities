# Sean Kinard 2-5-20
# This script creates a site by environmental covariate matrix to be used in an analysis of stream community structure across Texas Coastal Prairie using data collected from 13 streams in the Spring of 2017.

# Set Work Directory
setwd("C:\\Users\\s2kin\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats")
# Import gauges ii Climate Data
library(readr)
climate <- read.csv("sp17_data_files\\conterm_climate.txt")
basin <- read.csv("sp17_data_files\\conterm_lc06_basin.txt")
soils <- read.csv("sp17_data_files\\conterm_soils.txt")
rip <- read.csv("sp17_data_files\\conterm_lc06_rip100.txt")
topo <- read.csv("conterm_topo.txt")

# Extract Station IDs (STAIDs)
sites <- c("8189700", "8068390", "8115000", "8164600", "8189300", "8189500", "8177300", "8164800", "8211900", "8212300")
as.factor(climate$STAID)
climate <- subset(climate, STAID %in% sites)
basin <- subset(basin, STAID %in% sites)
soils <- subset(soils, STAID %in% sites)
rip <- subset(rip, STAID %in% sites)
topo <- subset(topo, STAID %in% sites)

# There are 12/13 STAIDs present: TRC (821300) is not Present in Gauges II. Since TRC (821300) is located within the same overall watershed as SFC (8211900), I will replicate the climate data from SFC and use it to create a new row for TRC. We are only interested in Average rainfall in our final analysis, but this step will enable exploratory analysis with the 10 sites with the climate data being the same for SFC and TRC.

# Substitute Soils from SFC to TRC STAID
row_to_add = climate[which(climate[,1]=="8211900"),]                    
row_to_add[1,1] <- 8212300
row_to_add
climate = rbind(climate, row_to_add)
climate[,1:3]

row_to_add = basin[which(basin[,1]=="8211900"),]
row_to_add[1,1] <- 8212300 
row_to_add
basin = rbind(basin, row_to_add)
basin[,1:3]

row_to_add = soils[which(soils[,1]=="8211900"),]
row_to_add[1,1] <- 8212300                                          
row_to_add
soils = rbind(soils, row_to_add)
soils[,1:3]

row_to_add = rip[which(climate[,1]=="8211900"),]                    
row_to_add[1,1] <- 8212300
row_to_add
rip = rbind(rip, row_to_add)
rip[,1:3]


MyMerge <- function(x, y){                                                      # Merge gauge data
df <- merge(x, y, by= "STAID", all.x= TRUE, all.y= TRUE)
return(df)
}
gauges <- Reduce(MyMerge, list(climate,basin,soils,rip, topo))

habitat <- read.csv("Spring2017.Stream.Habitat.csv", fileEncoding="UTF-8-BOM")  # Load in situ data
flow <- read.csv("USGS_FlowMetrics.csv", fileEncoding = "UTF-8-BOM")            # Load calculated flow metrics

# Merge data.frames
names(gauges)[1] <- "USGS.gauge"
env_covariates <- merge(gauges, habitat, by = "USGS.gauge")
env_covariates <- merge(env_covariates, flow, by = "USGS.gauge")

# USE Low Range nitrate concentrations. Substitute High range Nitrate where necessary
HR_NO3 <- which(env_covariates$NO3.NO2 > 1)
env_covariates$NO3..N.LR.[c(HR_NO3)] <- env_covariates$NO3.NO2[c(HR_NO3)]

# trim data frame to include the final 3 a priori environmental covariates.
independent <- c("USGS.gauge","PET","PPTAVG_BASIN","RH_BASIN","FORESTNLCD06","PLANTNLCD06","DEVNLCD06","RIP100_FOREST","PERMAVE","OMAVE","RFACT","HFPP3","LFPP","flsh","MeanFlow","Cpy","bank","D50","turbidity.adjusted","NH3.N","NO3..N.LR.","conductivity","PO43.","dissolved.oxygen","temperature.water","pH","width.depth","bank")
env_covariates <- env_covariates[,c(which (names(env_covariates) %in% independent == TRUE))]

# Export env by site and finalize formatting in Excel
write.csv(env_covariates,"C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\TCP_env_x_site.csv", row.names = FALSE)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


gaugestable <- c("USGS.gauge",
                 "PPTAVG_BASIN",
                 "T_AVG_BASIN", 
                 "ELEV_SITE_M", 
                 "SLOPE_PCT", 
                 "T_MIN_BASIN",
                 "T_MAX_BASIN",
                 "PLANTNLCD06",
                 "DEVNLCD06",
                 "PERMAVE",
                 "OMAVE", 
                 "CLAYAVE",
                 "SILTAVE",
                 "SANDAVE")
env_covariates <- env_covariates[,c(which (names(env_covariates) %in% gaugestable == TRUE))]


write.csv(env_covariates,"C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\TCP_env_x_site.csv", row.names = FALSE)

vardesc <- read.csv("sp17_data_files\\Sp17_gaugesconcotenated.csv")

myvars <- which(vardesc$VARIABLE_NAME == "USGS.gauge" |
        vardesc$VARIABLE_NAME =="PPTAVG_BASIN" |
      vardesc$VARIABLE_NAME =="T_AVG_BASIN" |
      vardesc$VARIABLE_NAME =="ELEV_SITE_M" |
      vardesc$VARIABLE_NAME =="SLOPE_PCT" |
      vardesc$VARIABLE_NAME =="T_MIN_BASIN" |
      vardesc$VARIABLE_NAME =="T_MAX_BASIN" |
      vardesc$VARIABLE_NAME =="PLANTNLCD06" |
      vardesc$VARIABLE_NAME =="DEVNLCD06" |
      vardesc$VARIABLE_NAME =="PERMAVE"|
      vardesc$VARIABLE_NAME =="OMAVE"| 
      vardesc$VARIABLE_NAME =="CLAYAVE"|
      vardesc$VARIABLE_NAME =="SILTAVE"|
      vardesc$VARIABLE_NAME =="SANDAVE")

env_var <- vardesc[c(myvars),c(2,3,6,12:13)]
















                    