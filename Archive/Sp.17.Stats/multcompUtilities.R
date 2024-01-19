##  How to use multcomp.summary.to.graph:

##  For a one-way ANOVA, you just need to save the glht summary statement, then pass it
##  to multcomp.summary.to.graph.  You can set alpha to whatever you want.

##  Example:
##  library(multcomp)
##  chickwts.aov<-aov(weight~feed,data=chickwts)
##  chickwts.glht<-glht(chickwts.aov,linfct=mcp(feed="Tukey"))
##  The next step takes a while to run (six factor levels slow Westfall
##  down):
##  (chickwts.summary<-summary(chickwts.glht,test=adjusted("Westfall")))
##  multcomp.summary.to.graph(chickwts.summary,alpha=0.08)  #  0.08?  why not!

##  For a two-way ANOVA, you will also need to provide the means for the
##  main factor you're testing.

##  Example:
##  tg<-ToothGrowth
##  tg$dose<-as.factor(tg$dose)
##  tg.aov<-aov(len~dose*supp,data=tg)
##  We now pretend there's not a significant interaction, but there is!
##  tg.glht<-glht(tg.aov,linfct=mcp(dose="Tukey"))
##  It tries to warn us about the interaction, but we proceed recklessly . . .
##  (tg.summary<-summary(tg.glht,test=adjusted("Westfall")))
##  multcomp.summary.to.graph(tg.summary,y.means=tapply(tg$len,tg$dose,mean))

multcomp.summary.to.graph<-function(multcomp.summary,y.means=NULL,alpha=0.1,
                                    plot.it=T, get.letters=F) {
  tr<-multcomp.summary #  saves on typing
  if (is.null(y.means)) {
    y.means<-tr$model$coefficients
    y.means[-1]<-y.means[-1]+y.means[1]
    names(y.means)<-levels(tr$model$model[[2]])
  }
  y.means<-sort(y.means)
  n.means<-length(y.means)
  start.stop<-matrix(0,n.means,3)
  rownames(start.stop)<-names(y.means)
  for (i in 1:n.means) {
    if (i==1) {start.stop[i,1]<-y.means[i]-sd(y.means)/4} else {start.stop[i,1]<-mean(y.means[(i-1):i])}
  }
  for (i in 1:n.means) {
    if (i==n.means) {start.stop[i,2]<-y.means[i]+sd(y.means)/4} else {start.stop[i,2]<-mean(y.means[i:(i+1)])}
  }
  start.stop[,3]<-start.stop[,2]
  for (i in 1:(n.means-1)) {
    for (k in (i+1):n.means) {
      row.name<-paste(names(y.means)[i],names(y.means)[k],sep=" - ")
      if (length((1:choose(n.means,2))[attr(tr$test$coefficients,"names")==row.name]) == 0) {row.name<-paste(names(y.means)[k],names(y.means)[i],sep=" - ")}
      row.num<-(1:choose(n.means,2))[attr(tr$test$coefficients,"names")==row.name]
      if (tr$test$pvalues[row.num]>=alpha) {start.stop[i,3]<-start.stop[k,2]}
      #  		print(c(i,k,row.name,tr[[1]][row.name,4]))
    }
  }
  k<-1; for (i in 2:n.means) {if (start.stop[i,3]>start.stop[i-1,3]) {k<-k+1}}
  z.main<-paste("Plot of the results from the",if (tr$test$type=="single-step") {"Tukey"} else {tr$test$type},"test",sep=" ")
  if (plot.it) {
    plot(y.means,rep(0,n.means),ylim=c(-k/10,0.2),xlab=NA,ylab=NA,yaxt="n",main=z.main,
         sub="means connected by lines are not significantly different")
    k<-1
    text(y.means[1],0.1,names(y.means)[1],srt=90)
    lines(c(start.stop[1,1],start.stop[1,3]),c(-k/10,-k/10)); k<-k+1
    for (i in 2:n.means) {
      text(y.means[i],0.1,names(y.means)[i],srt=90)
      if (start.stop[i,3]>start.stop[i-1,3]) {lines(c(start.stop[i,1],start.stop[i,3]),c(-k/10,-k/10)); k<-k+1}
    }
    abline(h=0,lty=3)
  }
  if (get.letters) {
    if (get.letters) {
      start.stop.letters <- rep("a", nrow(start.stop))
      names(start.stop.letters) <- rownames(start.stop)
      m <- 0
      for (k in 2:nrow(start.stop)) {
        if (start.stop[k,3] > start.stop[k-1,3]) {m <- m+1} # new letter
        start.stop.letters[k] <- chr(asc("a")+m)
      }
      return(start.stop.letters)
    }
  }
}

#  model.frame.gls, model.matrix.gls, and terms.gls are not intended for
#  student use.  However, they fill in missing features of the nlme package
#  to allow the use of multcomp with gls functions.
#  The function assume nlme has already been read in, since there's a gls
#  model to use this on . . . 

model.frame.gls<-function(gls.model) {model.frame(formula(gls.model),getData(gls.model))}

model.matrix.gls<-function(gls.model) {
  model.matrix(formula(gls.model),data=getData(gls.model))
}

terms.gls<-function(gls.model) {attr(model.frame(gls.model),"terms")}
