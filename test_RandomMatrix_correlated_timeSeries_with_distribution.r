t <- 500;
m <- 100;

Q <- t/m;

h <- array(rnorm(m*t),c(m,t)); # Time series in rows
e <- h %*% t(h)/t; # Form the correlation matrix
lambdae <- eigen(e, symmetric=T, only.values = T);
ee <- lambdae$values;
hist(ee,breaks=seq(0.01,3.01,.10),
main=NA,xlab="Eigenvalues",freq=F)

Variance = 1
LambdaMin = Variance*(1-sqrt(1/Q))^2
LambdaMax = Variance*(1+sqrt(1/Q))^2


Lambda = (ceiling(LambdaMin*100):floor(LambdaMax*100))/100


Rho = Q/(2*pi*Variance)*(sqrt(LambdaMax - Lambda)*(Lambda-LambdaMin))/Lambda