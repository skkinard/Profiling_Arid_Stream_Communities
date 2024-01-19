# Revised Enivronmental data frame
# Sean Kinard 12-14-2020

# Set Work Directory
setwd("R:\\Current Projects\\Texas\\Sean Folder\\PeerJ_Sp17\\")

# Import Data Files
emat <- read.csv("sp17_data_files\\sp17_site_x_env.csv", fileEncoding="UTF-8-BOM")

# Identify variables that covary with annual precipitation
d <- emat[,c(3:28)] # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1]))
r2 <- rep(0,length(d[,-1]))
f.stat <- rep(0,length(d[,-1]))
p.value <- rep(0,length(d[,-1]))
table.lm <- data.frame(predictor, estimate, df, r2, f.stat, p.value)
d.var <- noquote(names(d)[1])

for(i in 2:length(d)){
  i.var <- noquote(names(d)[i])
  formula = paste0(d.var, " ~ ", i.var)
  regression <- lm(formula, data=d)
  j <- i-1
  table.lm$estimate[j] <- summary(regression)$coefficients[2,1]
  table.lm$df[j] <- summary(regression)$df[2]
  table.lm$r2[j] <- summary(regression)$r.squared
  table.lm$f.stat[j] <- summary(regression)$fstatistic[1]
  table.lm$p.value[j] <- summary(regression)$coefficients[2,4]
  table.lm$p.value[j] <- pf(summary(regression)$fstatistic[1], 
                            summary(regression)$fstatistic[2], 
                            summary(regression)$fstatistic[3],
                            lower.tail=FALSE) 
}

table.lm


table.lm[c(which(table.lm$p.value < 0.05)),]

# PET and runoff.factor are derivative from AP and can be removed
emat_revised <- emat[,-c(5,11)]
emat_revised

# log transform conductivity and nitrate: identified via aug.pairs plots below
emat_revised$conductivity<-log(emat_revised$conductivity)
emat_revised$NO3.<-log(emat_revised$NO3.)
names(emat_revised)[which(names(emat_revised) == "conductivity")] <- "log.cond"
names(emat_revised)[which(names(emat_revised) == "NO3.")] <- "log.no3"




# Export files (emat_revised is converted to csv using excel)
write_xlsx(as.data.frame(table.lm),"sp17_r_scripts\\AP_colinearities.xlsx")
write_xlsx(as.data.frame(emat_revised),"sp17_data_files\\sp17_site_x_env_revised.xlsx")



