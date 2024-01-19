# Sean Kinard
# 12-17-2020

# PCA to identify patterns in variation among environmental variables

# Load and Install Packages
library(ggplot2)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(writexl)

# Set Working Directory
setwd("C:/Users/s2kin/Dropbox/Research/Manuscipts/Diss.2_Gradient/PeerJ_Sp17")

# Import Data File
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")

# Selecting a-priori selected variables
# 7 variables to evaluate with community diversity metrics and community composition ordinations
# Precipitation: Annual precipitation (30-year average)
# Flood disturbance regime: flash index
# Drought disturbance regime: Low Flow Pulse Percent  
# Biogenic Pollutants: NH4+
# Osmotic stressors: Conductivity
# Canopy effects: Canopy Coverage
# Stream morphology: Rosgen index
myvariables <- which(colnames(emat) == "STAID" |
  colnames(emat) == "AP" |
                       colnames(emat) == "flash.index" |
                       colnames(emat) == "LFPP" |
                       colnames(emat) == "NH4."|
                       colnames(emat) == "log.cond"|
                       colnames(emat) == "canopy"|
                       colnames(emat) == "Rosgen.Index"
)

emat_ap <- emat[,c(myvariables)]

# PCA and plots
mypr <- prcomp(emat_ap[,-1], scale = TRUE)
summary(mypr)
mypr_importance <- as.data.frame(summary(mypr)$importance)
mypr_importance$metric <- rownames(mypr_importance)
mypr_importance <- mypr_importance[,c(8,1:7)]

# diagnostic scree plot
plot(mypr, type = "l")

# PCA plot
ggbiplot(mypr, obs.scale=1, var.scale=1,
         groups = emat_ap$AP,
         labels = emat_ap$STAID) +
  scale_colour_viridis_c(direction=-1) +
  theme_classic() +
 theme(text = element_text(size = 16)) +
 theme(axis.text = element_text(size = 16))

# generating tables
m_env <- cbind(emat_ap[,-1], mypr$x[,1:2])
# correlations with PCA1
Pcor <- as.data.frame(cor(m_env[,1:7],m_env[,8:9]))
Pcor$variable <- rownames(Pcor)
Pcor <- Pcor[,c(3,1,2)]

# export tables to excel
write_xlsx(as.data.frame(mypr_importance),"sp17_r_scripts\\ap_PCA_importance.xlsx")
write_xlsx(as.data.frame(Pcor),"sp17_r_scripts\\ap_PCA_correlations.xlsx")







