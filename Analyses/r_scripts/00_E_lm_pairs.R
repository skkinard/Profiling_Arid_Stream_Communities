# 00_E_lm_pairs
# Sean Kinard
# 07-28-2021
# -----------------------------------------------------------------------------
# Description
# -----------------------------------------------------------------------------
# Firstly, the script selects specific variables from the environmental dataset and renames them for clarity. These selected variables are then scaled to ensure uniformity in their scales. Next, the script visualizes the relationships between variables using an augmented pairs plot. For regression analysis, the script defines a function called lm_table to compute regression statistics for each independent variable against the dependent variable. It formats the data frames for each variable combination and applies the lm_table function to generate regression tables. Finally, the script exports the regression tables to Excel format, with each variable combination having its own sheet. Additionally, it consolidates all regression tables into a single Excel file for easier management and further analysis.

# In summary, the script facilitates comprehensive analysis of environmental data, from preprocessing and visualization to regression analysis and result exportation, aiding in understanding and interpreting the relationships between different environmental variables

# Disclaimer: This is my first R project. It is not an accurate representation of my contemporary coding diction. It also utilizes tools from my ecological statistics course.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# pairs plots for environmental variables with fish shannon and invertebrate shannon
library(tidyverse)
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')
library(openxlsx)

# Load Data
env <- read.csv("sp17_data_files/sp17_site_x_env.csv")

# -----------------------------------------------------------------------------
# Data Preprocesing
# -----------------------------------------------------------------------------
# cleaning up data frames to include only the a priori selected variables and community abundance matrix

(env <- select(env, AP, flash.index, LFPP, NH4., log.cond, Rosgen.Index))

env <- rename(env, 'Precipitation' = 'AP', 'Flashiness' = 'flash.index', 'Low Flow Pulse %' = 'LFPP', 'NH4+' = 'NH4.', 'Conductivity' = 'log.cond', 'Channel Shape' = 'Rosgen.Index')

# Canopy removed in major revision 2. We realized that sampling occured prior to leaf emergence. Thus, the variable became uniformative towards gradient effects in this snap-shot of the communities. However, seasonal effects of canopy coverage will be relevant in surveys through time within the region.

# Scale data
env <- scale(env)

# -----------------------------------------------------------------------------
# Visualization
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Create table
# -----------------------------------------------------------------------------
# To run function, create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# format data frames to enter into function
AP_dat <- as.data.frame(env[,c(1:6)])
cond_dat <- as.data.frame(env[,c(5,1:4,6)])
ros_dat <- as.data.frame(env[,c(6,1:5)])
NH4_dat <- as.data.frame(env[,c(4,1:3,5)])
flash_dat <- as.data.frame(env[,c(2,1,3:6)])
LFPP_dat <- as.data.frame(env[,c(3,1:2,4:6)])

# Create Regression tables using function lm_table
AP_lm_table <- lm_table(AP_dat)
cond_lm_table <- lm_table(cond_dat)
ros_lm_table <- lm_table(ros_dat)
NH4_lm_table <- lm_table(NH4_dat)
flash_lm_table <- lm_table(flash_dat)
LFPP_lm_table <- lm_table(LFPP_dat)

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------
# Export tables to xcel
write.xlsx(AP_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="AP", row.names=FALSE)
write.xlsx(cond_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="cond", row.names=FALSE)
write.xlsx(ros_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="ros", row.names=FALSE)
write.xlsx(NH4_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="NH4", row.names=FALSE)
write.xlsx(flash_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="flash", row.names=FALSE)
write.xlsx(LFPP_lm_table, file="sp17_T1_Env_lm.xlsx", sheetName="LFPP", row.names=FALSE)

wb <- createWorkbook()

addWorksheet(wb, "AP")
addWorksheet(wb, "cond")
addWorksheet(wb, "ros")
addWorksheet(wb, "NH4")
addWorksheet(wb, "flash")
addWorksheet(wb, "LFPP")

writeData(wb, "AP", AP_lm_table)
writeData(wb, "cond", cond_lm_table)
writeData(wb, "ros", ros_lm_table)
writeData(wb, "NH4", NH4_lm_table)
writeData(wb, "flash", flash_lm_table)
writeData(wb, "LFPP", LFPP_lm_table)

saveWorkbook(wb, file = "sp17_E_lm.xlsx", overwrite = TRUE)

# -----------------------------------------------------------------------------
# End 00_E_lm_pairs