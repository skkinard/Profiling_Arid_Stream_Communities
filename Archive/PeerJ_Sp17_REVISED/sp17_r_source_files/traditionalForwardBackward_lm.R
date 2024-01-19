#############################################################

#  Example for the use of forward regression:  the idea is that we start
#  with a minimal model, then add variables as long as we can find something
#  significant to add.  We will use the airquality example:

#  What you give the function:
#  *  a minimal model:  usually, this is a model with only an intercept, 
#     no variables.  In R, you specify that using the notation y~1:

#     min.model<-lm(Ozone~1,data=airquality)

#  *  a matrix or data.frame with all the possible x's you might want to
#     add to the model.  Usually, the easiest way to do this is to pass
#     your entire dataset with only the y-variable removed

#  *  alpha.in:  the significance level you want to use for permitting
#     variables into the model.  Defaults to 0.05 if you don't set it

#  So putting it all together:

#  traditional.forward(min.model,airquality[,-1],0.05)

#############################################################

#  Example for the use of backward regression:  the idea is that we start
#  with a full model, then eliminate variables that aren't significant
#  What you give the function:
#  *  a full model.  The easiest way to specify this in R is using y~.
#     (that's not a period to end the sentence, the period is part of the
#     formula):
#  full.model<-lm(Ozone~.,data=airquality)

#  *  alpha.out:  the significance level you want to use to eliminate
#     variables from the model.  Defaults to 0.1 if you don't set it

#  So putting it all together:

#  traditional.backward(full.model,0.10)

#############################################################

this.call<-function(mx) {
	call.it<-paste(colnames(mx)[1],"~")
	if (dim(mx)[2]>2) {for (j in 2:(dim(mx)[2]-1)) {call.it<-paste(call.it,colnames(mx)[j],"+")}}
	call.it<-paste(call.it,colnames(mx)[dim(mx)[2]])
	return(call.it)
}
		

traditional.forward<-function(curr.model,additional.xs,alpha.in=0.05,n.loops=1000, 
                              min.print=F) {
	print(colnames(additional.xs))
	n.in<-dim(curr.model$model)[2]; n.poss<-ncol(additional.xs)
	best.t<-0  #  have to use t instead of p for cases with multiple p approx 0
	for (j in 1:n.poss) {
		this.mx<-cbind(curr.model$model,additional.xs[,j])
		colnames(this.mx)<-c(colnames(curr.model$model),colnames(additional.xs)[j])
		this.lm<-lm(this.mx)
		this.lm.coeffs<-summary(this.lm)$coefficients
		if (j==1) {out.coeffs<-this.lm.coeffs[nrow(this.lm.coeffs),]}
		else {out.coeffs<-rbind(out.coeffs,this.lm.coeffs[nrow(this.lm.coeffs),])}
		if (abs(this.lm.coeffs[n.in+1,3])>best.t) {
			best.mx<-this.mx;best.lm<-this.lm;best.j<-j
			best.t<-abs(this.lm.coeffs[n.in+1,3]); best.p<-this.lm.coeffs[n.in+1,4]
		}
	}
	if (n.poss==1) {
		out.coeffs<-data.frame(matrix(out.coeffs,1,4))
		colnames(out.coeffs)<-c("Estimate","Std. Error","t value","Pr(>|t|)")
		rownames(out.coeffs)<-colnames(additional.xs)
	}
	else {rownames(out.coeffs)<-colnames(additional.xs)}
	if (!min.print) {
	  cat("\n p-values for variables not currently in the model \n")
	  print(out.coeffs)
	}
	if (best.p>=alpha.in) {
		cat(paste("\n",this.call(curr.model$model)," cannot be improved by adding any additional variables using alpha.in = ",alpha.in,"\n",sep=""))
		return(curr.model)
		}
	else {
		cat(paste("\n","adding variable ",colnames(additional.xs)[best.j],"\n",sep=""))
		cat(paste("p.value is",signif(best.p,3),"\n"))
		if (n.loops>1) {
			if (n.poss>2) {traditional.forward(best.lm,data.frame(additional.xs[,-best.j]),alpha.in,n.loops-1, min.print)}
			else {
				stupid.R<-data.frame(additional.xs[,-best.j]); colnames(stupid.R)<-colnames(additional.xs)[-best.j]
				traditional.forward(best.lm,stupid.R,alpha.in,n.loops-1, min.print)
			}
		}
		else {
			cat(paste("\n",this.call(best.mx),"cannot be improved in the number of loops allowed","\n"))
			return(lm(best.mx))
		}
	}
}

traditional.backward<-function(curr.model,alpha.out=0.1,n.loops=1000, min.print=F) {
	lm.all.coeffs<-summary(lm(curr.model$model))$coefficients
	if (!min.print) {
	  cat("\n p-values for variables currently in the model \n")
	  print(lm.all.coeffs)
	}
	n.coeffs<-dim(lm.all.coeffs)[1]
	max.p<-max(lm.all.coeffs[2:n.coeffs,4])
	if (max.p>alpha.out) {
		out.row<-(2:n.coeffs)[lm.all.coeffs[2:n.coeffs,4]==max.p]
		cat(paste("\n","deleting variable ",rownames(lm.all.coeffs)[out.row],"\n",sep=""))
		cat(paste("p.value is ",signif(max.p,3),"\n"))
		new.mx<-curr.model$model[,-out.row]
		if (n.loops>1) {traditional.backward(lm(new.mx),alpha.out,n.loops-1, min.print)}
		else {
			cat(paste("\n",this.call(curr.model$model[,-out.row])," cannot be further reduced in the number of loops allowed","\n",sep=""))
			return(lm(curr.model))
		}
	}
	else {
		cat(paste("\n",this.call(curr.model$model)," cannot be further reduced using alpha.out = ",alpha.out,"\n",sep=""))
		return(curr.model)
	}
}

#traditional.step<-function(curr.model,additional.xs,alpha.in=0.05,alpha.out=0.1) {
#	curr.model.call<-this.call(curr.model$model)
#	forward.model<-traditional.forward(curr.model,additional.xs,alpha.in,1)
#	forward.model.call<-this.call(forward.model$model)
#	if (forward.model.call!=curr.model.call) {
#		backward.model<-traditional.backward(forward.model,alpha.out,1)
#		return(traditional.step(backward.model,additional.xs
		