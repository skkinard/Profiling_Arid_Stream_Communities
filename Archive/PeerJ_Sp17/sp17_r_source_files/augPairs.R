panel.density<-function(x,...) 
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    d<-density(x,na.rm=T)
    d$y<-d$y/max(d$y)
    lines(d$x,d$y,col="blue")
}

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    namx<-cbind(x,y); namx<-na.omit(namx); x<-namx[,1]; y<-namx[,2]
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
}

panel.lm<-function (x, y, col = par("col"), bg = NA, pch = par("pch"),cex = 1, col.lm = "red",  ...) {
	points(x, y, pch = pch, col = col, bg = bg, cex = cex)
	ok <- is.finite(x) & is.finite(y)
	if (any(ok)) {abline(lm(y~x),col=col.lm)}
}

aug.pairs<-function(x,...) {pairs(x,upper.panel=panel.lm,diag.panel=panel.density,lower.panel=panel.cor)}

#  example of use:
#  head(airquality)
#  aug.pairs(airquality)