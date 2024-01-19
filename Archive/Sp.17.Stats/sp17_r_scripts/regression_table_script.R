# running multiple linear regressions and exporting model outputs in a table

d <- msterfish[,-c(1,3:22)] # create data frame with dependent variable in column 1 and independent variables for linear regression in columns 2:length(d)

# make data frame for model outputs
predictor <- names(d[,-1])
estimate <- rep(0,length(d[,-1]))
df <- rep(0,length(d[,-1])
r2 <- rep(0,length(d[,-1))
f.stat <- rep(0,length(d[,-1)
p.value <- rep(0,length(d[,-1])
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

table.lm
