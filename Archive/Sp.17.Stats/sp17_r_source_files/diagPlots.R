## diag.plots attempts to provide diagnostic plots to help evaluate linearity,
## normality, and homoscedasticity for commonly used linear models.  
## Currently, it supports lm, aov, gls, gam, nls, gnls, nlme, and lme models

## You MUST specify a model.  For example:

## splat <- aov(breaks~tension, data=warpbreaks)
## diag.plots(splat)

## You MAY also specify the following parameters:

## col.nos and data: if you want to plot residuals against a specific variable.
## For example, to see the residuals of splat above plotted against the wool 
## and tension variables in warpbreaks:

## colnames(warpbreaks)  ##  wool is column 2, tension is column 3
## diag.plots(splat, col.nos=c(2, 3), data=warpbreaks)

## std:  by default, you always get: 
##  *  a Residual versus Fitted plot (if your model has at least one continuous input)
##  *  a qqplot with bounds
##  *  a Cook's distances plot (if your model is lm or aov)
##  If you only want to plot residuals against variables (for example, to save space),
##  set std=FALSE:

## diag.plots(splat, col.nos=c(2, 3), data=warpbreaks, std=FALSE)

## force.RvF:  if your model has no continuous inputs, the function assumes
## that you have an ANOVA, that a linearity test is unneeded, and that you don't
## need to look at the residuals versus fitted plot.  If you want to override this,
## set force.RvF=TRUE:

## diag.plots(splat, force.RvF=TRUE)

library(nlme, quietly=T); library(mgcv, quietly=T); library(lmtest, quietly=T)

get.r <- function(model.class, model) {
  if (model.class=="lm" | model.class=="aov") {
    r <- resid(model)
  }
  if (model.class=="gls" | model.class=="lme" | model.class=="gnls" | model.class=="nlme") {
    r <- residuals(model, type="normalized")
  }
  if (model.class=="gam") {
    r <- resid(model)
    names(r) <- rownames(model$model)
  }
  if (model.class=="nls") {
    r <- resid(model)
    names(r) <- rownames(model$model)
  }
  if (model.class=="gamm") {
    r <- resid(model$lme, type="normalized")
    names(r) <- names(fitted(model$gam))
  }
  return(r)
}

check.ANOVA <- function(model.class, model) {
  if (model.class=="lm" | model.class=="aov") {
    mm <- model$model
    is.ANOVA <- T
    for (j in 2:ncol(mm)) {if (!is.factor(mm[,j])) {is.ANOVA <- F}}
  }
  if (model.class=="gls") {
    if (length(model$contrasts)==0) {is.ANOVA <- F}
    else {
      mm <- getData(model)
      input.names <- names(model$parAssign)[-1]
      if (length(input.names)==0) {is.ANOVA <- T}
      else {
        is.ANOVA <- T
        for (a.name in input.names) {  # assumes main terms present for interaction
          j <- grep(a.name, colnames(mm))
          if (length(j) > 0 && !is.factor(mm[,j])) {is.ANOVA <- F}
        }
      }
    }
  }
  if (model.class=="lme") {
    mm <- getData(model)
    input.names <- attr(model$terms, "term.labels")
    if (length(input.names)==0) {is.ANOVA <- T}
    else {
      is.ANOVA <- T
      for (a.name in input.names) {  # assumes main terms present for interaction
        j <- grep(a.name, colnames(mm))
        if (length(j) > 0 && !is.factor(mm[,j])) {is.ANOVA <- F}
      }
    }
  }
  if (model.class=="gam" | model.class == "nls" | model.class=="gnls" | model.class=="nlme" | model.class=="gamm") {is.ANOVA <- F}
  return(is.ANOVA)
}

plot.continuous <-function(f, r, f.name, r.name, model.class, data=NULL, model=NULL) {
  f <- round(f, 6)
#  if (!is.null(data)) {f <- f[rownames(data) %in% names(r)]}
  if (!is.null(model) & (model.class=="lm" | model.class=="aov")) {
    bp.p.value <- bptest(model)$p.value
  }
  else {
    whee <- lm(r~f)
    bp.p.value <- bptest(whee)$p.value
  }
  sub.text <- "too few unique x's to fit a spline"
  if (length(unique(f))>4) {
    if (length(unique(f)) < 10) {k <- round(length(unique(f))/2)} else {k <- 10}
    rf.model<-gam(r~s(f, k=k), data=data.frame(r=as.double(r),f=as.double(f)), se.fit=T)
    sub.text <- paste("spline p-val =", signif(summary(rf.model)$s.table[1,4],4))
  }
  plot(r~f, xlab=f.name, ylab=r.name, sub=sub.text,
       main=paste("B-P p-val = ", signif(bp.p.value, 4)))
  abline(h=0, col=4, lwd=2)
  if (length(unique(f))>4) {
    f.seq<-seq(min(f),max(f),(max(f)-min(f))/100)
    r.seq<-predict(rf.model,newdata=data.frame(f=f.seq), se.fit=T)
    lines(r.seq$fit ~ f.seq,col="red", lwd=2)
    lines(r.seq$fit+2*r.seq$se.fit ~ f.seq, col="red", lty=2, lwd=2)
    lines(r.seq$fit-2*r.seq$se.fit ~ f.seq, col="red", lty=2, lwd=2)    
  }
}

qqnorm.with.sim.bounds<-function(the.data,sw=T,robust=T,main=NA,legend=F) {
  
  if (is.na(main)) {main<-"Normal Q-Q Plot"}
  n<-length(the.data)
  y.mx<-matrix(0,5000,n)
  #  robust--use median and mad; !robust--use mean and sd
  if (robust) {x.center<-median(the.data)} else {x.center<-mean(the.data)}
  if (robust) {s.center<-mad(the.data)} else {s.center<-sd(the.data)}
  for (i in 1:5000) {y.mx[i,]<-sort(rnorm(n,x.center,s.center))}
  lo.bds<-apply(y.mx,2,lo.bd<-function(x) {quantile(x,0.025)})
  hi.bds<-apply(y.mx,2,hi.bd<-function(x) {quantile(x,0.975)})
  lo.lo.bds<-apply(y.mx,2,lo.lo.bd<-function(x) {quantile(x,0.005)})
  hi.hi.bds<-apply(y.mx,2,hi.hi.bd<-function(x) {quantile(x,0.995)})
  meds<-apply(y.mx,2,median)
  ideal.x<-qnorm(ppoints(n))
  if (sw) {
    plot(ideal.x,sort(the.data),ylim=c(min(c(lo.lo.bds,the.data)),max(c(the.data,hi.hi.bds))),main=main,xlab="Theoretical Quantiles",ylab="Sample Quantiles",sub=paste("SW p-value",signif(shapiro.test(the.data)$p.value,4),' - Length',length(the.data)))
  }
  else {plot(ideal.x,sort(the.data),ylim=c(min(c(lo.lo.bds,the.data)),max(c(the.data,hi.hi.bds))),main=main,xlab="Theoretical Quantiles",ylab="Sample Quantiles")}
  abline(thud<-lm(meds~ideal.x),col="blue")
  lines(ideal.x,lo.bds,col="green",lty=2)
  lines(ideal.x,hi.bds,col="green",lty=2)
  lines(ideal.x,lo.lo.bds,col="red",lty=2)
  lines(ideal.x,hi.hi.bds,col="red",lty=2)
  if (legend) legend("topleft",legend=c("median","95% bounds","99% bounds"),lty=1,col=4:2)
}

bfl.test<-function(formula,data,...) {
  yf<-all.vars(formula);y<-data[,yf[1]];f<-data[,yf[2]]
  medians<-tapply(y,f,median)
  z<-abs(y-medians[f])
  z.aov<-aov(z~f)
  boxplot(resid(aov(y~f))~f,ylab="Residuals",
          sub=paste("BFL p-value =",signif(anova(z.aov)[1,5],4)),...)
}

diag.plots <- function(model, col.nos=c(), data=NULL, std=T, force.RvF=F) {
  if (length(col.nos)>0 & is.null(data)) {
    cat("You must specify the name of the dataset used in the model, for example:")
    cat("\n")
    cat("diag.plots(my.model, col.nos=c(2, 4), data=my.data)")
    cat("\n"); cat("\n")
    return()
  }
  model.class <- class(model)[1]
  ##  Use model.class to dig out necessary data
  r <- get.r(model.class, model)
  is.ANOVA <- check.ANOVA(model.class, model)
  ##  Create space for plots
  n.plots <- length(col.nos)
  if (std) {
    n.plots <- n.plots + 2
    if (model.class=="lm") {n.plots <- n.plots + 1}
    if (is.ANOVA & !force.RvF) {n.plots <- n.plots - 1}
  }
  n.plot.cols<-trunc((n.plots+1)/2)
  if (n.plot.cols==1) {par(mfcol=c(1,2))}
  else {par(mfcol=c(2, n.plot.cols))}
  ##  If desired, create standard plots
  if (std) {
    if (model.class != "gamm") {f <- fitted(model)} else {f <- fitted(model$gam)}
    if (!is.ANOVA | force.RvF) {plot.continuous(f, r, "Fitted", "Residuals", model.class, model=model)}
#    print("here")
    if (length(r)>=5000) {qqnorm(r)} else {qqnorm.with.sim.bounds(r)}
    if (model.class=="lm") {plot(model, which=4)}
  }
  ##  Create non-standard plots
  if (length(col.nos) > 0) {
    for (j in col.nos) {
      if (is.factor(data[,j])) {
        f <- data[,j]
        df.for.bfl <- data.frame(r, f)
        bfl.test(r~f, data=df.for.bfl, xlab=colnames(data)[j])
      }
      else {plot.continuous(data[,j], r, colnames(data)[j], "Residuals", model.class, data=data)}
    }
  }
}

# cooks.d.2 computes Cook's distance when you remove TWO points instead of just one
# You should specify the two points as rows i1 and i2 in the dataset
# Right now, this only works for lm models.

# cooks.d.2.all uses cooks.d.2 on all pairs of points.

cooks.d.2 <- function(model, i1, i2) {
  p <- ncol(model.matrix(model))
  new.mm <- model$model[-c(i1, i2),]
  new.lm <- lm(new.mm)
  diffs <- fitted(model) - predict(new.lm, newdata=model$model)
  return(sum(diffs^2)/p/summary(model)$sigma^2)
}


library(ggplot2)
cooks.d.2.all <- function(model, plot.it = T, return.mx = F) {
  n <- nrow(model$model)
  if (return.mx) {out.mx <- matrix(0, nrow(model$model), nrow(model$model))}
  if (plot.it) {plot.mx <- data.frame(pt1 = sort(rep(1:n, n)), pt2 = rep(1:n, n), cooksD=0)}
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cd.ij <- cooks.d.2(model, i, j)
      if (return.mx) {out.mx[i,j] <- cd.ij; out.mx[j,i]=cd.ij}
      if (plot.it) {
        plot.mx[plot.mx$pt1==i & plot.mx$pt2==j, 3] <- cd.ij
        plot.mx[plot.mx$pt1==j & plot.mx$pt2==i, 3] <- cd.ij
      }
    }
  }
  if (plot.it) {
    g <- ggplot(data=plot.mx, aes(x=pt1, y=pt2, colour=cooksD, size=cooksD)) + 
      theme_bw() + geom_point() +
      scale_colour_gradientn(colours = rainbow(6))
    return(g)
  }
  if (return.mx) {return(out.mx)}
}