#Analyzing Spring_2017 for Limnology Report

#loading source files
source('C:/Users/Sean/Desktop/Stats/R Utilities/._augPairs.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Stats/R Utilities/._diagPlots.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Stats/R Utilities/._multcompUtilities.R', encoding = 'UTF-8')
source('C:/Users/Sean/Desktop/Stats/R Utilities/._traditionalForwardBackward_lm.R', encoding = 'UTF-8')
source('C:/Users/Sean/Downloads/augPairs.R', encoding = 'UTF-8')

#Load Packages
library("vegan", lib.loc="~/R/win-library/3.4")
library("car", lib.loc="~/R/win-library/3.4")
library("MuMIn", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

#Load .csv's
Fish <- read.csv("C:/Users/Sean/Desktop/LimnoData/Sp.17.Fish.csv")
Invert <- read_csv("C:/Users/Sean/Desktop/LimnoData/Sp.17.Invert.csv")
View(Fish)
View(Invert)


#Shannon Index calculations
Fish.Wo.env. <- Fish[,c(19:49)]
View(Fish.Wo.env.)
SI.F <- diversity(Fish.Wo.env.)
plot(SI.F)
SI.F

#Add SI to fish
Fish$ShannonIndex <- SI.F
View(Fish)

#create linear model predicting SI
Fish.Wo.Sp <- Fish[,c(52,2:18)]
View(Fish.Wo.Sp)
aug.pairs(Fish.Wo.Sp)

#Identified NH3, NO3, PO4 negative correlation with SI
# W/D, Drainage sq milage correlates according to RCC preditions
# Avg PPt and Forest cover positive correlation with SI
#fix W/D to D/W
Fish$Depth.Width <- c(0.073583075, 0.08325887, 0.043196785, NA, 0.055064046, 0.047557067, 0.068023256, 0.059875923, NA, 0.06599932, 0.074740784, 0.054558761, 0.041471242)
colnames(Fish)
Fish <- Fish[,c(1:4,53,6:52)]
View(Fish)
#back to correlates

gsub("na","NA", Fish)
View(Fish)

Fish$log.NH3 <- log10(Fish$NH3.N)
Fish$log.NO3 <- log10(Fish$NO3.NO2)
Fish$log.PO4 <- log10(Fish$PO43.)
Fish$log.PPT <- log10(Fish$AvgPPT)
Fish$log.MeanFlow <- log10(Fish$Mean.Flow)
Fish$log.Drain <- log10(Fish$Drain.sqkm.)


newcorrelates.SI.Fish <- Fish[,c(52,54,56,17:18,53, 14:15, 12)]

aug.pairs(newcorrelates.SI.Fish)
View(newcorrelates.SI.Fish)

Fish.WaChem <- newcorrelates.SI.Fish[1,3:5]
aug.pairs(Fish.WaChem)

Fish.Clim <- Fish[52, 16:17]
aug.pairs(Fish.Clim)

Fish.Env <- Fish[52, 53, 11:15]
aug.pairs(Fish.Env)


# I made log calcs in excel and made a new csv to import for further fish diversity correlate analysis

fish2 <- read.csv("C:/Users/Sean/Desktop/LimnoData/Sp.17.Fish.W.Calcs.csv")
fish2$Depth.Width <- c(0.073583075, 0.08325887, 0.043196785, NA, 0.055064046, 0.047557067, 0.068023256, 0.059875923, NA, 0.06599932, 0.074740784, 0.054558761, 0.041471242)
colnames(fish2)
fish2.lm.1 <- fish2[,c(19,23,16,18, 26, 20:22)]
aug.pairs(fish2.lm.1)
#Remove missing y values
fish2.lm.1<-fish2.lm.1[!is.na(fish2.lm.1$Log.PPT),]

Fish.model.NH3 <- lm(Shannon.Index~ Log.NH3, data=fish2.lm.1)
summary(Fish.model.NH3)

#Residual standard error: 0.3642 on 11 degrees of freedom
#Multiple R-squared:  0.5579,	Adjusted R-squared:  0.5177 
#F-statistic: 13.88 on 1 and 11 DF,  p-value: 0.003348
full.model.fish <- lm(Shannon.Index~ Log.PPT+Avg.Temp+ Forest+ Depth.Width + Log.NH3 + Log.NO3, data=fish2.lm.1)
summary(full.model.fish)

#Residual standard error: 0.2936 on 1 degrees of freedom
#(5 observations deleted due to missingness)
#Multiple R-squared:  0.959,	Adjusted R-squared:  0.713 
#F-statistic: 3.899 on 6 and 1 DF,  p-value: 0.3694
#summary(Fish.model.NH3)

vif(full.model.fish)
#Log.PPT    Avg.Temp      Forest      Depth.Width     Log.NH3     Log.NO3 
#6.234487    7.020136     2.800343    1.684729       4.566312     2.106615 

min.model<-lm(Shannon.Index~1,data=fish2.lm.1)
forward.model <- traditional.forward(min.model,fish2.lm.1[,2:7])
summary(forward.model)

#Residual standard error: 0.3455 on 6 degrees of freedom
#Multiple R-squared:  0.6594,	Adjusted R-squared:  0.6027 
#F-statistic: 11.62 on 1 and 6 DF,  p-value: 0.01434

backward.model <- traditional.backward(full.model.fish)
summary(backward.model)

#Shannon.Index = 16.24 - .769*Avg.Temp + 20.077*Depth.Width - .334*Log.NO3
#Residual standard error: 0.1834 on 4 degrees of freedom
#Multiple R-squared:  0.936,	Adjusted R-squared:  0.888 
#F-statistic:  19.5 on 3 and 4 DF,  p-value: 0.007514

dredge(full.model.fish)
summary(fish2.lm.1)
min.model<-lm(Shannon.Index~1,data=fish2.lm.1)
summary(min.model)
full.model<-lm(Shannon.Index~.,data=fish2.lm.1)
summary(full.model)

plot(fish2.lm.1$Shannon.Index, fish2.lm.1$Log.PPT)
abline(backward.model)


##############################################################################
################################################################################

#Analyzing Invertebrate Data
Sp_17_Invert <- read.csv("C:/Users/Sean/Desktop/LimnoData/Sp.17.Invert.csv")
colnames(Sp_17_Invert)
div.inv <- Sp_17_Invert[,1:25]
Inv <- Sp_17_Invert[1:13,]

#Create Shannon Index
colnames(Inv)
Inv.Wo.Env <- Inv[,c(25:130)]
View(Inv.Wo.Env)
SI.Inv <- diversity(Inv.Wo.Env)
plot(SI.Inv)
SI.Inv


Inv$ShannonIndex <- SI.Inv
View(div.inv)
div.inv <- div.inv[,c(25,1:24)]
colnames(div)

aug.pairs(div.inv[,c(1,5,6,8,11,17:19,22,25)])
mod.var.inv.div <- div.inv[,c(1,5,6,8,11,17:19)]
#Remove missing y values
mod.var.inv.div<-mod.var.inv.div[c(1,3,5:8,10,12),]

min.model.2<-lm(Shannon.Index.of.Invertebrate.Diversity~1,data=Inv_model)
colnames(Inv_model)
full.inv.model <- lm(Shannon.Index.of.Invertebrate.Diversity~PO43.+PH+Forest+DO+Depth.Width+AvgPPT+Avg.Temp,data=mod.var.inv.div)
full.inv.model <- lm(Shannon.Index.of.Invertebrate.Diversity~PO43.+PH+DO+Depth.Width+AvgPPT+Avg.Temp,data=mod.var.inv.div)
summary(full.inv.model)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 24.07235   14.64273   1.644    0.348
#PO43.       -0.22131    0.15198  -1.456    0.383
#PH          -0.27236    0.69610  -0.391    0.763
#DO           0.05072    0.03018   1.681    0.342
#Depth.Width  8.75396   25.34978   0.345    0.788
#AvgPPT      -0.06935    0.04544  -1.526    0.369
#Avg.Temp    -0.86363    0.64032  -1.349    0.406
#
#Residual standard error: 0.2173 on 1 degrees of freedom
#Multiple R-squared:  0.9648,	Adjusted R-squared:  0.7538 
#F-statistic: 4.572 on 6 and 1 DF,  p-value: 0.3435

vif(full.inv.model)
#PO43.          PH          DO       Depth.Width      AvgPPT     Avg.Temp 
#10.667241    4.840689   20.888243   10.314634       51.752513   15.339796 

x <- c(1:8)
x$col.names <- colnames(mod.var.inv.div)
View(x)

forward.model.2 <- traditional.forward(min.model.2,mod.var.inv.div[,2:7])
summary(forward.model.2)
vif(forward.model.2)
#Shannon= -0.0744 +36.4929*Depth.Width
#Residual standard error: 0.2356 on 6 degrees of freedom
#Multiple R-squared:  0.7518,	Adjusted R-squared:  0.7104 
#F-statistic: 18.17 on 1 and 6 DF,  p-value: 0.005303
colnames(mod.var.inv.div)
Inv_model <- read.csv("C:/Users/Sean/Desktop/LimnoData/Inv.model.csv")
colnames(Inv_model)
Inv_model$SID <- Inv_model$ï..SID_Inv
Inv_model <- Inv_model[,c(9,2:8)]
min.model.3<-lm(SID~1,data=Inv_model)
backward.model.2 <- traditional.backward(full.inv.model)
summary(backward.model.2)
vif(backward.model.2)

#Coefficients:
#              Estimate    Std. Error  t value Pr(>|t|)   
#(Intercept)   26.514988     6.307888   4.203  0.02458 * 
#  PO43.       -0.235538     0.053061  -4.439  0.02125 * 
#  DO           0.057341     0.008261   6.941  0.00613 **
#  AvgPPT      -0.082152     0.012851  -6.393  0.00775 **
# Avg.Temp     -1.017958     0.268170  -3.796  0.03209 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.1353 on 3 degrees of freedom
#Multiple R-squared:  0.9591,	Adjusted R-squared:  0.9045 
#F-statistic: 17.57 on 4 and 3 DF,  p-value: 0.0202

#VIF
#   PO43.         DO          AvgPPT      Avg.Temp 
#   3.351129      4.034402    10.669024   6.934777 

dredge(full.inv.model)
options(na.action = "na.fail")






