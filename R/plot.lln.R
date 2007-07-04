`plot.lln` <-
function(x,...){
# Plot functions for "lln" objects
# 	UseMethod("plot.lln","lln")
	Y <- prms$Y
	# x <- prms$Z
	flag <- prms$flag
        n <- ncol(Y)
        if (!missing(cls)) k <- max(cls)

	# Latent positions of the nodes
        plot(x,type='n',xlab='',ylab='')

	# Plot ties between nodes
        for (i in 1:(n-1)){
                for (j in (i+1):n){
                        if (Y[i,j]==1){
                                points(c(x[i,1],x[j,1]),c(x[i,2],x[j,2]),type='l',lty=2,lwd=1)
                        }
                }
        }

	# Plot nodes without colors
        if (missing(cls)) points(x,type='p',pch=19,col='black',xlab='',ylab='')

	# Names of nodes
        if (n<25 & !missing(txt)) text(x,lab=seq(1,n),pos=3,offset=0.5,font=2)

	# Plot nodes with colors
	if (!missing(cls) & !unlab){
		for (i in 1:k){
			points(x[flag==1&cls==i,1],x[flag==1&cls==i,2],type='p',pch=14+i,cex=2,col=1+i,lwd=2,xlab='',ylab='');
			points(x[flag==2&cls==i,1],x[flag==2&cls==i,2],type='p',pch=i,cex=2,col=1+i,lwd=2,xlab='',ylab='');
		}
	}

	# Plot nodes without colors for test nodes
	if (!missing(cls) & unlab){
		for (i in 1:k){
			points(x[flag==1&cls==i,1],x[flag==1&cls==i,2],type='p',pch=14+i,cex=2,col=1+i,lwd=2,xlab='',ylab='');
			points(x[flag==2&cls==i,1],x[flag==2&cls==i,2],type='p',pch=18,cex=2,col="black",lwd=2,xlab='',ylab='');
		}
	}

	# Plot decision boundaries
	if (!missing(classifier)){
		prec <- 100
		x1 <- seq(min(x[,1])*1.1,max(x[,1])*1.2,length=prec)
		x2 <- seq(min(x[,2])*1.1,max(x[,2])*1.2,length=prec)
		s <- expand.grid(x1,x2); s <- as.matrix(s)
		P <- predict(classifier,s)$posterior
		for (i in 1:k){
			T <- P[,i] - apply(P[,-i,drop = FALSE],1, max)
			contour(x1,x2,matrix(T,prec,prec),level=0,add=1,col="black",lwd=3,drawlabels=0);
		}
	}
}