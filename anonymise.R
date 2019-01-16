anonymise <- function(inp,k){
		dataB <- inp; 
		N=dim(inp)[1]; M=dim(inp)[2]
		dataS <- apply(dataB, 2, function(x) x[order(x)]); 	
		dataCode <- dataScode <- matrix(NA, N, M)
		for (c in 1:M){
			  t1 <-(dataS[,c])
			  kq <- c(); i = 1;  sPT = 1; group = 1
			  kq[i] = group; n= length(t1);   
	 	while (i < n) {
		    	i = i + 1; sPT = sPT + 1;    
		    	if (sPT <=k) kq[i]=group
		   	else {
 		   	  if(t1[i]==t1[i-1]) kq[i]=group
  	    	  else {
        			group = group + 1;
        			kq[i]=group;
        			sPT = 1}
    			}}
  		if (kq[n-1]!=kq[n-2]) {kq[n-1]=kq[n-2]; kq[n]=kq[n-2];}
  		if (kq[n]!=kq[n-1]) {kq[n]=kq[n-2];}  
  		dataScode[,c]<-kq   
  		a1 <- data[,c]; b1 <- kq
  		a2 <- order(a1)
 			a3 <- c()
  		for (i in 1:N){a3[a2[i]]=i;}
  		dataCode[,c] <- b1[a3]
		}
		rl <- dataCode
		return(rl)
}