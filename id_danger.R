id.danger  <- function(x, v, alpha){
	x <- as.vector(x); nx <- length(x)
	x.sort <- sort(x)#, decreasing = T); 
	nx.ex <- round(alpha*length(x)); 
	id.u <- (nx-nx.ex+1); u <- x.sort[id.u]
	id.ex <- which(x>=u)
	x.ex <- x[id.ex]; 	Nu   <- length(x.ex)
	p.exceed <- mean(x.ex>u)
	xi   <- fit_GPD_PWM(x.ex-u)[1]; beta  <- fit_GPD_PWM(x.ex-u)[2]
	x_hat <- u + beta/xi*((nx/Nu*(1-v))^(-xi)-1);
	rl    <- list(sort=as.numeric(x.sort),
		value.extreme=as.numeric(x.ex),index.extreme=as.numeric(id.ex), 
		p.exceed=as.numeric(p.exceed), obs=nx, Nb_extremes = Nu,
		threshold=as.numeric(u),index.threshold=as.numeric(id.u),  
		shape=as.numeric(xi),scale=as.numeric(beta),
		POT=as.numeric(x_hat))
	return(rl)
}

id.danger.logit <- function(x,v1,v2,alpha){
	p99 <- id.danger(x,v1,alpha)$POT
	p999 <- id.danger(x,v2,alpha)$POT
	xi <-  id.danger(x,v1,alpha)$shape
	beta <-  id.danger(x,v1,alpha)$scale
	xx <- log(x/(1-x))
	pi99 <- id.danger(xx,v1,alpha)$POT 
	pi999 <- id.danger(xx,v2,alpha)$POT 
	p99_logit <- exp(pi99)/(1+exp(pi99))
	p999_logit <- exp(pi999)/(1+exp(pi999))
	xi_logit <-  id.danger(xx,v1,alpha)$shape
	beta_logit <-  id.danger(xx,v1,alpha)$scale
	 	  rl    <- list(xi=as.numeric(xi), beta=as.numeric(beta),
	 			xi_logit=as.numeric(xi_logit), beta_logit=as.numeric(beta_logit),
	 			id_99=as.numeric(p99),id_999=as.numeric(p999),
	 			id_99_logit=as.numeric(p99_logit),id_999_logit=as.numeric(p999_logit))
	return(rl)
}
