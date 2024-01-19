# Regression analyses for sp17 environmnetal variables

library("readr")
library(xlsx)
# mac
setwd("/Users/seankinard/Dropbox/Research/Manuscipts/Diss.2_Gradient/Sp.17.Stats")

# mac
emat <- read.csv("sp17_data_files/sp17_site_x_env.csv")

# log transform
emat$cond<-log(emat$cond)
emat$NO3<-log(emat$no3)
names(emat)[which(names(emat) == "cond")] <- "log.cond"
names(emat)[which(names(emat) == "no3")] <- "log.no3"


# Linear Regression models
N <- length(2:length(emat))
vectorOfTables <- vector(mode = "list", length = N)

for(i in 2:length(emat)){
# select dependent variable: Fish Rarified richness
if(i==2){d <- emat[,c(i:length(emat))]}
if(i %in% 3:(length(emat)-1)){d <- emat[,c(i, 2:(i-1),(i+1):length(emat))]}
if(i == length(emat)){d <- emat[,c(i,2:(i-1))]}
d.var <- noquote(names(d)[1])
# make data frame for model outputs
response <- rep(0,length(d)-1)
predictor <- names(d[,-1])
estimate <- rep(0,length(d)-1)
df <- rep(0,length(d)-1)
r2 <- rep(0,length(d)-1)
f.stat <- rep(0,length(d)-1)
p.value <- rep(0,length(d)-1)
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
# run regressions for each independent variable and store in a table                                       
for(j in 2:length(d)){
  i.var <- noquote(names(d)[j])
  formula = paste0(d.var, " ~ ", i.var)
  regression <- lm(formula, data=d)
  k <- j-1
  table.lm$response <- names(d)[1]
  table.lm$estimate[k] <- summary(regression)$coefficients[2,1]
  table.lm$df[k] <- summary(regression)$df[2]
  table.lm$r2[k] <- summary(regression)$r.squared
  table.lm$f.stat[k] <- summary(regression)$fstatistic[1]
  table.lm$p.value[k] <- summary(regression)$coefficients[2,4]
  table.lm$p.value[k] <- pf(summary(regression)$fstatistic[1], 
                            summary(regression)$fstatistic[2], 
                            summary(regression)$fstatistic[3],
                            lower.tail=FALSE)
  
}
# assign table values to a new object  
vectorOfTables[[i]] <- table.lm
}

# Extract significant correlations:
vectorOfTables[[2]][which(vectorOfTables[[2]]$r2 > 0.4 & vectorOfTables[[2]]$p.value <0.05),]

for(i in 2:length(vectorOfTables)){
  print(vectorOfTables[[i]][which(vectorOfTables[[i]]$r2 > 0.4 & vectorOfTables[[i]]$p.value <0.05),])
}
