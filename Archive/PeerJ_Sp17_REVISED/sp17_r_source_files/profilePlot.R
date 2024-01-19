#  using make.profile.plot:  
#  *  the mandatory parameters are
#     *  formula:  this takes the form of y~x.factor|other.factor, where y is your y-variable,
#        x.factor is the factor you want to be on the x-axis, and other.factor is the factor
#        you want to define the separate lines
#     *  data:  the name of your dataset
#  *  optional parameters are
#     *  se.bars, which can take on three values:  use.none, use.aov (get the bar size from 
#        the residual standard error from the ANOVA), or use.data (get the bar size from
#        the std dev of each x1*x2 level, divided by the square root of same)
#     *  xlab, ylab, main, and sub:  graphical parameters for the x- and y-axis and title and subtitle
#     *  of.name:  if you want something to appear in the legend with each level of the "other factor"
#  

#  Example using the ToothGrowth dataset included with R
#  make.profile.plot(len~dose|supp,data=ToothGrowth,se.bars=use.aov,xlab="Dose of Vitamin C",
#                    ylab="Tooth growth",main="Tooth growth",sub="OJ = orange juice, VC = vitamin supplement",
#                    of.name="Vitamin C source")

use.none<-0; use.aov<-1; use.data<-2; one.rep<-3

make.profile.plot<-function(formula,data,se.bars=use.none,xlab=NA,ylab=NA,main=NA,sub=NA,of.name=NA,legend.loc="topleft",legend.cex=1) {
	var.names<-all.names(formula)
	y<-data[,var.names[2]]
	factor.x<-data[,var.names[4]]
	if (!is.factor(factor.x)) {factor.x<-factor(factor.x)}
	other.factor<-data[,var.names[5]]
	if (!is.factor(other.factor)) {other.factor<-factor(other.factor)}
	levels.x<-levels(factor.x);n.levels.x<-length(levels.x)
	other.levels<-levels(other.factor);n.other.levels<-length(other.levels)
	y.bar<-matrix(NA,n.levels.x,n.other.levels)
	for (i in 1:n.levels.x) {
		for (j in 1:n.other.levels) {
			if (sum(factor.x==levels.x[i] & other.factor==other.levels[j])>0) {
				y.bar[i,j]<-mean(y[factor.x==levels.x[i] & other.factor==other.levels[j]])
			}
		}
	}	
	se.bar<-matrix(0,n.levels.x,n.other.levels)
	if (se.bars==use.aov) {sigma<-summary(lm(y~factor.x*other.factor))$sigma}
	if (se.bars==one.rep) {sigma<-summary(lm(y~factor.x+other.factor))$sigma}
	if (se.bars!=use.none) {
		se.bar<-matrix(0,n.levels.x,n.other.levels)
		for (i in 1:n.levels.x) {
			for (j in 1:n.other.levels) {
				if (sum(factor.x==levels.x[i] & other.factor==other.levels[j])>0) {	
					if (se.bars==use.data) {sigma<-sd(y[factor.x==levels.x[i] & other.factor==other.levels[j]])}			
					se.bar[i,j]<-sigma/sqrt(sum((factor.x==levels.x[i]) & (other.factor==other.levels[j])))
				}
			}
		}
	}
	min.y<-min(y.bar-se.bar,na.rm=T); max.y<-max(y.bar+se.bar,na.rm=T)
	plot(c(0,1+n.levels.x),c(min.y,max.y),type="n",main=main,xlab=xlab,ylab=ylab,axes=F,sub=sub)
	axis(side=1,at=0:(1+n.levels.x),labels=c("",levels.x,""))
	axis(side=2)
	if (se.bars==use.none) {x.val.shifts<-rep(0,n.other.levels)} else {x.val.shifts<-(1:n.other.levels)/10-n.other.levels/10/2}
	for (j in 1:n.other.levels) {
		if (sum(factor.x==levels.x[1] & other.factor==other.levels[j])>0) {
			points(1+x.val.shifts[j],y.bar[1,j],col=j)
			if (se.bars!=use.none) {
				arrows(1+x.val.shifts[j],y.bar[1,j],1+x.val.shifts[j],y.bar[1,j]-se.bar[1,j],angle=90,col=j,length=0.1)
				arrows(1+x.val.shifts[j],y.bar[1,j],1+x.val.shifts[j],y.bar[1,j]+se.bar[1,j],angle=90,col=j,length=0.1)
			}
		}
		for (i in 2:n.levels.x) {
			if (sum(factor.x==levels.x[i] & other.factor==other.levels[j])>0) {
				points(i+x.val.shifts[j],y.bar[i,j],col=j)
				if (sum(factor.x==levels.x[i-1] & other.factor==other.levels[j])>0) {
					lines(i+x.val.shifts[j]+c(-1,0),y.bar[(i-1):i,j],col=j)
				}
				if (se.bars!=use.none) {
					arrows(i+x.val.shifts[j],y.bar[i,j],i+x.val.shifts[j],y.bar[i,j]-se.bar[i,j],angle=90,col=j,length=0.1)
					arrows(i+x.val.shifts[j],y.bar[i,j],i+x.val.shifts[j],y.bar[i,j]+se.bar[i,j],angle=90,col=j,length=0.1)
				}
			}
		}
	}
  if (legend.loc=="none") {return()}
	if (is.na(of.name)) {legend(legend.loc,legend=other.levels,lty=1,pch=1,col=(1:n.other.levels), cex=legend.cex)}
	else {legend(legend.loc,legend=paste(of.name,other.levels),lty=1,pch=1,col=(1:n.other.levels), cex=legend.cex)}
}