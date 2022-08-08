n <- 5000;
mu <- array(runif(n^2),c(n,n));
mu2 <-sqrt(12)*(mu+t(mu)-1)/sqrt(2*n);
lambdau <- eigen(mu2, symmetric=T, only.values = T);
eu <- lambdau$values;
hist(eu,breaks=seq(-2.05,2.05,.02),main=NA,xlab="Eigenvalues",freq=F)
eu <- lambdau$values;
histeu<-hist(eu,breaks=seq(-2.01,2.01,0.02),main=NA, xlab="Eigenvalues",freq=F)