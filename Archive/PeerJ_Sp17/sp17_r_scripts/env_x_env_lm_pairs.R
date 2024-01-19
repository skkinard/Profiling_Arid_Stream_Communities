# pairs plots for environmental variables with fish shannon and invertebrate shannon
library(dplyr)
source('sp17_r_source_files/augPairs.R', encoding = 'UTF-8')
library(openxlsx)


fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")


# Scale data
emat[,-c(1,2)] <- scale(emat[,-c(1,2)])
# Merge Community x site with env x site
msterfish <- merge(fmat, emat, by = "STAID")
row.names(msterfish) <- msterfish$staid
msterfish <- Filter(is.numeric, msterfish)

msterinvert <- merge(imat,emat, by = "STAID")
row.names(msterinvert) <- msterinvert$staid
msterfish <- Filter(is.numeric, msterfish)

msterfish <- msterfish %>% rename(fish_Shannon = shannon)
msterinvert <- msterinvert %>% rename(invert_Shannon = shannon)


myvariables <- which(colnames(emat) == "STAID" |
                       colnames(emat) == "AP" |
                       colnames(emat) == "flash.index" |
                       colnames(emat) == "LFPP" |
                       colnames(emat) == "NH4."|
                       colnames(emat) == "log.cond"|
                       colnames(emat) == "canopy"|
                       colnames(emat) == "Rosgen.Index"
)

a <- msterinvert[,1:2]
b <- msterfish[,1:2]
c <- emat[,c(myvariables)]

pear_data <- merge(a,b, by = "STAID")
pear_data <- merge(pear_data, c, by = "STAID")
pear_data

# Create aug pairs plot
aug.pairs(pear_data[,c(3,2,4:9)], na.action=na.omit)


# Creating env x env regression table
d <- LFPP_dat
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
AP_dat <- pear_data[,c(4,5:10)] 
cond_dat <- pear_data[,c(5,4,6:10)]
ros_dat <- pear_data[,c(6,4:5,7:10)]
can_dat <- pear_data[,c(7,4:6,8:10)]
NH4_dat <- pear_data[,c(8,4:7,9:10)]
flash_dat <- pear_data[,c(9,4:8,10)]
LFPP_dat <- pear_data[,c(10,4:9)]

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

saveWorkbook(wb, file = "sp17_T1_E_lm.xlsx", overwrite = TRUE)


























