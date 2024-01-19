#  Use #1:  corrected.lme.anova(model.1, model.2)
#  corrected.lme.anova figures out on its own how many random parameters
#  each of these models has.

#  model.1 and model.2 should be models produced by either gls or lme.
#  They should be identical except differing by 1 in the number of random
#  parameters.  If they don't differ at all in the number of random 
#  parameters, no test is possible; if they differ by more than 1, a
#  test is possible but it's more complicated and I haven't had a chance
#  to write it yet.

#  Use #2:  corrected.lme.anova(model.1, model.2, df.1, df.2)
#  From the use of the simulate function in nlme, you decide what mixture
#  model to use and tell corrected.lme.anova how many degrees of freedom
#  to use for each model.

#  EXAMPLES OF USE #1:

#  library(nlme)
#  gls.00 <- gls(distance~age*Sex, data=Orthodont)
#  lme.00<-lme(distance~age*Sex,data=Orthodont,random=~1|Subject)
#  corrected.lme.anova(gls.00, lme.00)

#  The next example fails because lme.00 and lme.01 differ by _2_ random 
#  parameters.
#  lme.01<-lme(distance~age*Sex,data=Orthodont,random=~age|Subject)
#  corrected.lme.anova(lme.00,lme.01)  ##  FAILS

#  EXAMPLE OF USE #2:

#  plot(simulate(lme.00, m2=lme.01, nsim=500, method="REML"), df=c(1,2))
#  The above center plot, Mix(1,2), looks pretty straight, so use a mixture
#  model with df.1=1, df.2=2
#  corrected.lme.anova(lme.00, lme.01, df.1=1, df.2=2)

count.rand.df <- function(a.model) {
  df <- 0
  for (i in 1:length(a.model$modelStruct$reStruct)) {
    df <- df + length(a.model$modelStruct$reStruct[[1]])
  }
  return(df)
}

corrected.lme.anova<-function(model.1, model.2, df.1=NULL, df.2=NULL) {
  if (is.null(df.1[1])) { # corrected.lme.anova has to figure out df's
    if (class(model.1)=="gls") {df.1<-0} else {df.1<-count.rand.df(model.1)}
    if (class(model.2)=="gls") {df.2<-0} else {df.2<-count.rand.df(model.2)}
    if (abs(df.1-df.2)==0) {
      cat("Can't do anova, same number of random parameters.\n\n")
      return()
    }
    if (abs(df.1-df.2)>1) {
      cat("Difference of more than one in random df not implemented yet.\n\n")
      return()
    }
  }
  raw.anova<-anova(model.1,model.2)
  raw.anova[2,9]<-1-0.5*pchisq(raw.anova[2,8],df.1)-0.5*pchisq(raw.anova[2,8],df.2)
  colnames(raw.anova)[9]<-"corrected.p.value"
  return(raw.anova)
}