prob_re <- function(inp, h){
	N=dim(inp)[1]; M=dim(inp)[2]; 
	C <- choose(M,h); 
	idh <- as.matrix(t(combn(M,h))); 
	outp <- matrix(NA,N,C)
	for (ns in 1:N){
 	 	for (c in 1:C){
 			outp[ns,c] <- length(which(count(inp[,idh[c,]])$freq==ns))
 	 	}
	}
	outp2<- colSums(outp)/N
	rl <- list(Ni=outp, prob=outp2, obs=N,att=M,key=h,combinatoire=C)
	return(rl)
}
