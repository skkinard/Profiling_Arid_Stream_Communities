# Sean Kinard
# 03-30-2021
# Spring 17 Texas Coastal Prairie

# Fish diversity linear regressions & aggregate plot lm&RDA at end of script

# pairs plots for environmental variables with fish shannon and invertebrate shannon
library(tidyverse)
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')
library(openxlsx)

# Load Data
env <- read.csv("sp17_data_files/sp17_site_x_env.csv")

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# cleaning up data frames to include only the a priori selected variables and community abundance matrix
(env <- select(env, AP, flash.index, LFPP, NH4., log.cond, canopy, Rosgen.Index))

# - # - # - # - # - # - # - # - # - # - # - # - # # - # - # - # - #

# Scale data
env <- scale(env)

# Create aug pairs plot
aug.pairs(env, na.action=na.omit)

# Create function to export regression statistics
lm_table <- function(d) {
# make data frame for model outputs
  predictor <- names(d[,-1])
  estimate <- rep(0,length(predictor))
  df <- rep(0,length(predictor))
  r2 <- rep(0,length(predictor))
  f.stat <- rep(0,length(predictor))
  p.value <- rep(0,length(predictor))
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
  return(table.lm)
  }

# To run function, create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# format data frames to enter into function
AP_dat <- as.data.frame(env[,c(1:7)])
cond_dat <- as.data.frame(env[,c(5,1:4,6:7)])
ros_dat <- as.data.frame(env[,c(7,1:6)])
can_dat <- as.data.frame(env[,c(6,1:5,7)])
NH4_dat <- as.data.frame(env[,c(4,1:3,5:7)])
flash_dat <- as.data.frame(env[,c(2,1,3:7)])
LFPP_dat <- as.data.frame(env[,c(3,1:2,4:7)])


# Create Regression tables using function lm_table
AP_lm_table <- lm_table(AP_dat)
cond_lm_table <- lm_table(cond_dat)
ros_lm_table <- lm_table(ros_dat)
can_lm_table <- lm_table(can_dat)
NH4_lm_table <- lm_table(NH4_dat)
flash_lm_table <- lm_table(flash_dat)
LFPP_lm_table <- lm_table(LFPP_dat)

# Export tables to xcel
write.xlsx(AP_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="AP", row.names=FALSE)
write.xlsx(cond_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="cond", row.names=FALSE)
write.xlsx(ros_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="ros", row.names=FALSE)
write.xlsx(can_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="can", row.names=FALSE)
write.xlsx(NH4_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="NH4", row.names=FALSE)
write.xlsx(flash_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="flash", row.names=FALSE)
write.xlsx(LFPP_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="LFPP", row.names=FALSE)

wb <- createWorkbook()

addWorksheet(wb, "AP")
addWorksheet(wb, "cond")
addWorksheet(wb, "ros")
addWorksheet(wb, "can")
addWorksheet(wb, "NH4")
addWorksheet(wb, "flash")
addWorksheet(wb, "LFPP")

writeData(wb, "AP", AP_lm_table)
writeData(wb, "cond", cond_lm_table)
writeData(wb, "ros", ros_lm_table)
writeData(wb, "can", can_lm_table)
writeData(wb, "NH4", NH4_lm_table)
writeData(wb, "flash", flash_lm_table)
writeData(wb, "LFPP", LFPP_lm_table)

saveWorkbook(wb, file = "sp17_E_lm.xlsx", overwrite = TRUE)


























