# Sean Kinard
# 12-16-2020


library(ggplot2)
library(dplyr)
library(gridExtra)

# Import Data Files
fmat <- read.csv("sp17_data_files\\sp17_site_x_fish.csv")
imat <- read.csv("sp17_data_files\\sp17_site_x_invert.csv", fileEncoding="UTF-8-BOM")
emat <- read.csv("sp17_data_files\\sp17_site_x_env_revised.csv", fileEncoding="UTF-8-BOM")

# Merge Community x site with env x site
msterfish <- merge(fmat, emat, by = "STAID")
row.names(msterfish) <- msterfish$staid
msterfish <- Filter(is.numeric, msterfish)

msterinvert <- merge(imat,emat, by = "STAID")
row.names(msterinvert) <- msterinvert$staid
msterfish <- Filter(is.numeric, msterfish)


# ::::::::::::::::::::::::::::::::::::::::::
# Hypothesis test: Diversity Correlates with Precipitation
# :::::::::::::::::::::::::::::::::::::::::

par(mfrow=c(1,2),cex.lab=1.25,font.lab=1, cex.axis=1.2)

# diversity vs precipitation Linear Regressions
lm_fs_vs_AP <- lm(formula= shannon ~ AP, data = msterfish)
summary(lm_fs_vs_AP) # fish

lm_is_vs_AP <- lm(formula= shannon ~ AP, data = msterinvert)
summary(lm_is_vs_AP) # invert

plot0 <- ggplot(msterfish, aes(x=AP, y=shannon)) +
  geom_point(shape=19,aes(size=.1)) + 
  geom_smooth(method=lm, se=FALSE, color = "black") +
  ggtitle("Fish") +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon") +
  annotate("rect", xmin = 100, xmax = 120, ymin = .6, ymax = .9, fill="white", colour="black") +
  annotate("text", x=110, y=0.85, label = "y = 0.017x - 0.336") + 
  annotate("text", x=110, y=0.75, label = "R^2 == 0.602", parse=T) + 
  annotate("text", x=110, y=0.65, label = "alpha == 0.008", parse=T) + 
  theme_bw() + 
  theme(legend.position="none") + 
  theme(text = element_text(size = 20)) 

plot1 <- ggplot(msterinvert, aes(x=AP, y=shannon)) +
  geom_point(shape=19,aes(size=.1)) + 
  ggtitle("Invertebrate") +
  scale_x_continuous(name = "Precipitation (cm/yr)") +
  scale_y_continuous(name = "Shannon") +
  annotate("rect", xmin = 100, xmax = 120, ymin =2.0, ymax = 2.25, fill="white", colour="black") +
  annotate("text", x=110, y=2.18, label = "R^2 == 0.017", parse=T) + 
  annotate("text", x=110, y=2.07, label = "alpha == 0.717", parse=T) + 
  theme_bw() + 
  theme(legend.position="none")+ 
  theme(text = element_text(size = 20))

grid.arrange(plot0, plot1, ncol=2)

# Result: fish diversity significantly correlates with precipitation while invertebrate diversity does not