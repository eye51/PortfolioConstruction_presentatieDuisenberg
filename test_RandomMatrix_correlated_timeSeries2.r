t <- 5000;
m <- 1000;


Q <- t/m;

h <- array(rnorm(m*t),c(m,t)); # Time series in rows
e <- h %*% t(h)/t; # Form the correlation matrix


minX <- 0.01
maxX <- 3.01


lambdae <- eigen(e, symmetric=T, only.values = T);
ee <- lambdae$values;
histEigVal <- hist(ee,breaks=seq(minX,maxX,.10), plot=FALSE)

maxY <- max(round(histEigVal$density,1) + 0.1)
minY <- 0

par(lwd=2)

histEigVal <- hist(ee,breaks=seq(minX,maxX,.10),main=NA,xlab="Eigenvalues",freq=F, ylim=c(minY,maxY))




Variance = 1
LambdaMin = Variance*(1-sqrt(1/Q))^2
LambdaMax = Variance*(1+sqrt(1/Q))^2


Lambda = (ceiling(LambdaMin*100):floor(LambdaMax*100))/100


Rho = (Q/(2*pi*Variance))*(sqrt((LambdaMax - Lambda)*(Lambda-LambdaMin)))/Lambda

par(new=TRUE)	# do not clean drawing area

plot(Lambda, Rho, type='l',col='red', xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")