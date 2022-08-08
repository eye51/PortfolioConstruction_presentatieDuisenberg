#path <- 'S:/SC_AM/Afdeling/PRMTP/Employees/Bastiaan/documents/data/historical daily data_indices/';
#file <- 'DAX_daily.csv';


rm(list=ls())

path <- 'G:/Users/Bastiaan de Geeter/Documents/Rprogr/covariance/';
file <- 'daily_data_100_2.csv';
FileName <- paste(path, file, sep="");

inputFile <- file (FileName, "r");


aa <- read.csv(inputFile, header = TRUE)

close(inputFile)



NoFunds <- dim(aa)[2]/2;
NoObs <-  dim(aa)[1];

NoObs <-  125;

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

#h <- array(rnorm(NoFunds *noObs),c(NoFunds ,noObs)); # Time series in rows
#e <- h %*% t(h)/t; # Form the correlation matrix




e <- var(adjReturns)
lambdae <- eigen(e, symmetric=T, only.values = T);

minX <- 0.01
maxX <- ceiling(max(lambdae$values))+1.01


ee <- lambdae$values;
histEigVal <- hist(ee,breaks=seq(minX,maxX,.10), plot=FALSE)

maxY <- max(round(histEigVal$density,1) + 0.1)
minY <- 0

histEigVal <- hist(ee,breaks=seq(minX,maxX,.10),main=NA,xlab="Eigenvalues",freq=F, ylim=c(minY,maxY))




Variance <- 1
LambdaMin <- Variance*(1-sqrt(1/Q))^2
LambdaMax <- Variance*(1+sqrt(1/Q))^2


Lambda <- (ceiling(LambdaMin*100):floor(LambdaMax*100))/100


Rho <- Q/(2*pi*Variance)*(sqrt(LambdaMax - Lambda)*(Lambda-LambdaMin))/Lambda

par(new=TRUE)	# do not clean drawing area

plot(Lambda, Rho, type='l',col='red', xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")



#
#	input fit:
#

VarianceFit <- 1
QFit <- (NoObs-1)/ NoFunds;

LambdaMinFit = Variance*(1-sqrt(1/QFit))^2
LambdaMaxFit = Variance*(1+sqrt(1/QFit))^2

minXFit <- 0.01
maxXFit <- ceiling(max(lambdae$values))+1.01

#
#	distribution measured eig-values
#

histEigValFit <- hist(ee,breaks=seq(minXFit,maxXFit,.10), plot=FALSE)
maxYFit <- max(round(histEigValFit$density,1) + 0.1)
minYFit <- 0

#
#	find values between Lambda_min & Lambda_max
#

indicator <- (LambdaMin < histEigVal$mids)  * (histEigVal$mids < LambdaMax);
indicator <- indicator * 1:length(indicator);
mids <- histEigVal$mids[indicator];
Rhos_fit = Q/(2*pi*Variance)*(sqrt(LambdaMax - mids)*(mids-LambdaMin))/mids

#
#	calc cost-function
#

NoObsFit <- length(Rhos_fit)

costFunction <- 0
NoSampleFit <- 0
for (i in 1:NoObsFit)
{
	
	if (histEigValFit$density[i] > 0)
	{
		costFunction <- costFunction + (histEigValFit$density[i] - Rhos_fit[i])^2
		NoSampleFit <- NoSampleFit + 1
	}
		
}


costFunction <- sqrt(costFunction)/NoSampleFit

par(new=TRUE)	# do not clean drawing area

plot(mids , Rhos_fit, type='o',col='green', xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")
