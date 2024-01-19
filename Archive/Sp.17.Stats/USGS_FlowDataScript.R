# Download Gauge Data for Stream Sites
# Script Author - Christopher J. Patrick
# July 18, 2018
# Edits by Sean Kinard November 13, 2019

setwd("C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats")

# load the package waterData
library(waterData)

# download a single starter series [Contains 2016-Present]
h1 <- importDVs(staid = "08212300", code = "00060", stat = "00003", sdate = "2016-08-01",edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
All <- h1[,c(2,3)]
colnames(All) <- c("08212300","dates")

# list of remaining stream sites
gage.list <- c("08211900", "08211520", "08189700", "08189300", "08177300", "08189200", "08189500", "08164600", "08164800", "08115000", "08068390", "08068450")

# download data from remaining sites and merge with first site
for (x in 1:length(gage.list)){
temp <- importDVs(staid = gage.list[x], code = "00060", stat = "00003", sdate = "2007-08-01",
  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
temp2 <- temp[,c(2,3)]
colnames(temp2) <- c(gage.list[x],"dates")

All <- merge(All,temp2, by = "dates")
}

write.csv(All, "C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\USGS_DailyFlows2.csv")

data <- read.csv("C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\USGS_DailyFlows2.csv", header = TRUE, sep = ",",)

data2 <- reshape(data,varying= 3:15, v.names = "Discharge",timevar = "Site", times = c("TRC", "SFC", "OSO", "AR", "MC", "PDC", "CC", "MR", "GC", "PLC", "BC", "BB", "PB"), direction = "long")

write.csv(data2, "C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\USGS_DailyFlowsLong2.csv")



#Getting the 20 year records
# download a single starter series
h1 <- importDVs(staid = "08211900", code = "00060", stat = "00003", sdate = "2002-01-01",edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
All <- h1[,c(2,3)]
colnames(All) <- c("08211900","dates")

# list of remaining stream sites
gage.list <- c( "08211520", "08189700", "08189300", "08177300", "08189200", "08189500", "08164600", "08164800", "08115000", "08068390", "08068450")

# download data from remaining sites and merge with first site
for (x in 1:length(gage.list)){
temp <- importDVs(staid = gage.list[x], code = "00060", stat = "00003", sdate = "2002-01-01",
  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
temp <- temp[,c(2,3)]
colnames(temp) <- c(gage.list[x],"dates")

All <- merge(All,temp, by = "dates")
}

names(All) <- c("dates", "SFC", "OSO", "AR", "MC", "PDC", "CC", "MR", "GC", "PLC", "BC", "BB", "PB")
data2 <- reshape(All,varying= c("SFC", "OSO", "AR", "MC", "PDC", "CC", "MR", "GC", "PLC", "BC", "BB", "PB"), v.names = "Discharge",timevar = "Site", times = c("SFC", "OSO", "AR", "MC", "PDC", "CC", "MR", "GC", "PLC", "BC", "BB", "PB"), direction = "long")


# Creating a Dataset of Summary Statistics for the 8 streams with 22 year records in the USGS Dataset
mdata <- apply(All[,-1], MARGIN= c(2), FUN = mean) # list of mean flow for the site

#############
# Flow Metrics

# Flow metrics Scripts

#x <- Flow Series of Daily Flows

#### Magnitude of Flow Events

## Average Flow Conditions

# Variability in Daily Flows
varDF <- function(x) {sd(x)/mean(x)}
# Skewness in daily flows
skewDF <- function(x) {mean(x)/median(x)}
# Spread in Daily Flow (based on 10th/90th percentile ranges)
sprdDF90 <- function(x){(x[order(x)][(length(x)*.10)]/x[order(x)][(length(x)*.90)])/median(x)}
sprdDF80 <- function(x){(x[order(x)][(length(x)*.20)]/x[order(x)][(length(x)*.80)])/median(x)}


## Average Low Flow Conditions
# BFI 1  --> avg of lowest 7 day stretch / average daily flow
BFI <- function(x) {
vec <- c(1,2)
for (i in 1:(length(x)-7)){
vec[i] <- mean(x[c(i:(i+6))])
}
min(vec)/mean(x)
}

#1,366,731,1096,1460,1825,2190,2555,2920,3286
## Average High Flow Conditions
# Median annual max flow - 
medAnnMaxFl <- function(x){
vec <- c(1,2)
for (i in 1:10) {
vec[i] <- max(x[c(((1+(i-1)*365)):((1+(i-1)*365)+364))])
}
median(vec)/median(x)
}

#### Frequency of Flow Events
## Low Flow 
# Low Flood Pulse 
LFPP <- function(x) {
thresh <- x[order(x)][(length(x)*.25)]
vec <- c(1,2)
for (i in 1:(length(x)-1)){
vec[i] <- ifelse(((x[i] > thresh)&(x[i+1] < thresh)),1,0)
}
sum(vec)/(length(x)/365)
}

# Frequency of low flow spells
FrLFsp <- function(x) {
thresh <- x[order(x)][(length(x)*.05)]
vec <- c(1,2)
for (i in 1:(length(x)-1)){
vec[i] <- ifelse(((x[i] > thresh)&(x[i+1] < thresh)),1,0)
}
sum(vec)/(length(x)/365)
}


## High Flow
# High Flood Pulse Percentage (3 & 7 times median)
HFPP3 <- function(x) {length(x[x > (median(x)*3)])/length(x)}
HFPP7 <- function(x) {length(x[x > (median(x)*7)])/length(x)}


#### Duration of Flow Events
## Low flow  

#30 minima of daily discharge
minDD30 <- function(x){
vec <- c(1,2) # moving 30 day average within a year
vec2 <- c(1,2) # annuals 
for (i in 1:10) {
temp <- x[c((1+(i-1)*365):((1+(i-1)*365)+364))]
for (t in 1:(length(temp) - 30)){
vec[t] <- mean(temp[c(t:(t+29))])
}
vec2[i] <- min(vec)
}
mean(vec2)/median(x)
}

## High Flow
#30 maxima of daily discharge
maxDD30 <- function(x){
vec <- c(1,2) # moving 30 day average within a year
vec2 <- c(1,2) # annuals 
for (i in 1:10) {
temp <- x[c((1+(i-1)*365):((1+(i-1)*365)+364))]
for (t in 1:(length(temp) - 30)){
vec[t] <- mean(temp[c(t:(t+29))])
}
vec2[i] <- max(vec)
}
mean(vec2)/median(x)
}

#### Timing of Flow Events

#### Rate of Change of Flow Events
# Flashiness

flsh <- function(x){
store <- matrix(0,(length(x-1)),1)
for (i in 2:length(x)) {
store[i-1,1] <- abs(x[i] - x[i-1])
}
(sum(store))/(sum(x))
}
######################################################
#### metrics include flsh,maxDD30,minDD30, HFPP7,HFPP3,FrLFsp, LFPP, varDF, medAnnMaxFl, BFI
funlist <- list("flsh","maxDD30","minDD30", "HFPP7","HFPP3","FrLFsp", "LFPP", "varDF", "medAnnMaxFl", "BFI", "mean","median")

metrics.17 <- as.data.frame(matrix(0,12,13))
colnames(metrics.17) <- c("STAID","flsh","maxDD30","minDD30", "HFPP7","HFPP3","FrLFsp", "LFPP", "varDF", "medAnnMaxFl", "BFI", "MeanFlow","MedianFlow")
metrics.17$STAID <- colnames(All[,-1])

for (x in 1:12){
metrics.17[,x+1] <- as.vector(apply(All[,-1], MARGIN= c(2), FUN = funlist[[x]])) # list of mean flow for the site
}


mdata <- apply(All[,-1], MARGIN= c(2), FUN = mean) # list of mean flow for the site

metrics.4 <- as.data.frame(matrix(0,12,13))
colnames(metrics.4) <- c("STAID","flsh","maxDD30","minDD30", "HFPP7","HFPP3","FrLFsp", "LFPP", "varDF", "medAnnMaxFl", "BFI", "MeanFlow","MedianFlow")
metrics.4$STAID <- colnames(All[,-1])

for (x in 1:12){
metrics.4[,x+1] <- as.vector(apply(All[,-1], MARGIN= c(2), FUN = funlist[[x]])) # list of mean flow for the site
}

metrics.c <- metrics.17
sites_na <- c(5,6,7,10,12)
for (x in sites_na) {
  metrics.c[x,] <- metrics.4[x,]
  metrics.c[x,2] <- flsh(na.omit(All[,x+1]))
  metrics.c[x,3] <- maxDD30(na.omit(All[,x+1]))
  metrics.c[x,4] <- minDD30(na.omit(All[,x+1]))
  metrics.c[x,5] <- HFPP7(na.omit(All[,x+1]))
  metrics.c[x,6] <- HFPP3(na.omit(All[,x+1]))
  metrics.c[x,7] <- FrLFsp(na.omit(All[,x+1]))
  metrics.c[x,8] <- LFPP(na.omit(All[,x+1]))
  metrics.c[x,9] <- varDF(na.omit(All[,x+1]))
  metrics.c[x,10] <- medAnnMaxFl(na.omit(All[,x+1]))
  metrics.c[x,11] <- BFI(na.omit(All[,x+1]))
  metrics.c[x,12] <- mean(na.omit(All[,x+1]))
  metrics.c[x,13] <- median(na.omit(All[,x+1]))
  
}
View(metrics.c)

write.csv(metrics.c, "C:\\Users\\Sean\\Desktop\\Dropbox\\Research\\Manuscipts\\Diss.2_Gradient\\Sp.17.Stats\\USGS_FlowMetrics.csv")
























