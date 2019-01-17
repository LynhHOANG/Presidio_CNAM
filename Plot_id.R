Plot_id <- function(x,v1,v2,alpha){
	y=log(x/(1-x))
  	hist(x, main="Histogram of prob of re-identification", xlab="Probability", ylab="Frequency")
 	tms <- id.danger(x,v1,alpha);   tms2 <- id.danger(y,v1,alpha)
  	xi <- round(tms$shape,4); beta <- round(tms$scale,4)
  	xi2 <- round(tms2$shape,4); beta2 <- round(tms2$scale,4)
  	u <- tms$threshold; ex <- tms$value.extreme
  	xh1<-id.danger(y,v1,alpha)$POT;   xh2<-id.danger(y,v2,alpha)$POT
  	xhat1<- exp(xh1)/(1+exp(xh1))
  	xhat2<- exp(xh2)/(1+exp(xh2))
  	u1 <- id.danger(x,v1,1-v1)$threshold;
  	u2 <- id.danger(x,v2,1-v2)$threshold;
  	legend("topright",col=c(2,4,5),legend=c(paste("threshold",alpha*100,"%"),paste("threshold",(1-v)*100,"%"), paste("threshold",(1-v2)*100,"%")), lty=c(1,1,1),lwd=c(2,2,2))
  
  	legend(0.42,100, legend = paste("C(h,M)=",choose(M,h),"combinations"))
 	mean_excess_plot(x, main=paste("Threshold of",alpha*100,"% extremes values u =",round(u,4)),xlim=c(0,max(x)))
  	segments(u,0,u,1,col=2,lwd=2)
  	segments(u1,0,u1,1,col=4,lwd=2)
  	segments(u2,0,u2,1,col=5,lwd=2)
  
 	xx <- seq(from = 0, to = 1, length = 1000)
  	z <- qgpd(xx, xi, u, beta)
  	y <- pgpd(z, xi, u, beta)
  	plot(sort(ex),ppoints(sort(ex)),type="p",ylab="Fu(x-u)", 
    	   main= paste("GPD Fit u =",round(u,4)), ylim=c(0,1),
     	   xlab=paste("The",alpha*100,"% extremes values of prob of re-identification (x>=u)"))#,log="x")
  	lines(z[y >= 0], y[y >= 0], col=2, lwd=3)
  	legend("bottomright",legend=c(paste("xi =",xi),paste("beta=",beta),paste("xi_LOGIT =",xi2),paste("beta_LOGIT =",beta2)))
   
	plot(sort(x),ppoints(sort(x)),type="l",ylab="F(x)", 
     	  main= paste("POT estimator by LOGIT"), ylim=c(0,1),
       	  xlab=paste("The probability of re-identification (x)"))#,log="x")
       	legend("bottomright",col=c(4,5),lty=c(1,1) ,horiz = F,
        legend=c(paste("id_99%_LOGIT = ",round(xhat1,4)),paste("id_99.9%_ =",round(xhat2,4))))
  	segments(xhat1,0,xhat1,v,col=4,lwd=2)
  	segments(xhat1,v,0,v,col=4,lwd=2)
  	segments(xhat2,0,xhat2,v,col=5,lwd=2)
  	segments(xhat2,v2,0,v2,col=5,lwd=2)
}

