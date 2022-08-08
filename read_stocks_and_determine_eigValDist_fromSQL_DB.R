path <- 'G:/Users/Bastiaan de Geeter/';
file <- 'daily_data_100_2.csv';

FileName <- paste(path, file, sep="");

inputFile <- file (FileName, "r");


aa <- read.csv(inputFile, header = TRUE)

close(inputFile)



NoFunds <- floor(dim(aa)[2]/2);
NoObs <-  dim(aa)[1];

NoObs <-  1*252;

headers <- attr(aa,'names')
funds <- headers[2*(1:NoFunds)-1]
prices <- as.matrix(aa[2*(1:NoFunds)])		# convert datafrom from reading CSV file to matrix format
returns <- log(prices[2:NoObs,] / prices[1:(NoObs-1),])


#
#	calculate variance of individual stocks
#	and scale returns so that vol = 1
#

covarStocks <- var(returns)				# determine covariance matrix
varStocks <- diag(covarStocks)				# variance of stocks is diagonal of covariance matrix

adjReturns <- matrix(0,nrow=NoObs-1,ncol = NoFunds)	# declare space for adj-returns


meanReturns <- numeric(NoFunds)
for (i in 1:NoFunds)
{
	meanReturns[i] <- mean(returns[,i])
	adjReturns[,i] <- (returns[,i] - meanReturns [i]) / sqrt(varStocks[i])
}




varAdjStocks <- var(adjReturns[1])
meanAdjStocks <- mean(adjReturns[1])

for (i in 1:NoFunds)
{
	varAdjStocks <- c(varAdjStocks,var(adjReturns[i]))
	meanAdjStocks <- c(meanAdjStocks ,mean(adjReturns[i]))

}

#
#
#

Q <- (NoObs-1)/ NoFunds;

#h <- array(rnorm(NoFunds *NoObs),c(NoFunds ,NoObs)); # Time series in rows
#e <- h %*% t(h)/t; # Form the correlation matrix



e <- var(adjReturns)
lambdae <- eigen(e, symmetric=T, only.values = T);
ee <- lambdae$values;

minX <- 0.1
maxX <- ceiling(max(ee))



histEigVal <- hist(ee,breaks=seq(minX,maxX,.10), plot=FALSE)




maxY <- max(round(histEigVal$density,1) + 0.1)
minY <- 0

histEigVal <- hist(ee,breaks=seq(minX,maxX,.10),main=NA,xlab="Eigenvalues",freq=F, ylim=c(minY,maxY))




Variance = 1
LambdaMin = Variance*(1-sqrt(1/Q))^2
LambdaMax = Variance*(1+sqrt(1/Q))^2


Lambda = (ceiling(LambdaMin*100):floor(LambdaMax*100))/100


Rho = Q/(2*pi*Variance)*(sqrt((LambdaMax - Lambda)*(Lambda-LambdaMin))/Lambda)

par(new=TRUE)	# do not clean drawing area

plot(Lambda, Rho, type='l',col='red', xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")