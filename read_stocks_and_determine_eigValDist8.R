#path <- 'S:/SC_AM/Afdeling/PRMTP/Employees/Bastiaan/documents/data/historical daily data_indices/';
#file <- 'DAX_daily.csv';


rm(list=ls())

testFun <- function(Q,Variance)
{
	tst <- Q * Variance;
	
}


MaxLQMarcenkoPastur <- function(parameters,observations)
{
	
	Q_LQ <- abs(parameters[1]);
	Variance_LQ <- abs(parameters[2]);
	
	print("tst")
	print(parameters)
	
	noObs <- length(observations)

	
	LambdaMinLQ = Variance_LQ*(1-sqrt(1/Q_LQ))^2
	LambdaMaxLQ = Variance_LQ*(1+sqrt(1/Q_LQ))^2
	
	probLQ <- 1

	for (i in 1:noObs)
	{
		
		if ((observations[i] < LambdaMinLQ) || (observations[i] > LambdaMaxLQ))
		{
		probLQ <- probLQ * 0.000001
		}
		else
		{
			probLQ <- probLQ * Q_LQ/(2*pi*Variance_LQ)*(sqrt((LambdaMaxLQ - observations[i])*(observations[i]-LambdaMinLQ))/observations[i])
			
		}
		
	}
	
	
	maxLQ <- -log(probLQ)
	
}


SQErr <- function(inputs,EigenValsIn)
{

#
#	input fit:
#

Qin <- inputs[1]
Variancein <- inputs[2]

print(c(Qin,Variancein))

if ((Qin <0) || (Variancein < 0))
	{
		costFunction <- 9999
	}
	else
	{
		
		
	LambdaMinFit = Variancein*(1-sqrt(1/Qin))^2
	LambdaMaxFit = Variancein*(1+sqrt(1/Qin))^2
	
	minXFit <- 0.01
	maxXFit <- ceiling(max(EigenValsIn))+1.01
	#print(c(LambdaMinFit,LambdaMaxFit))

#print(EigenValsIn)

#
#	distribution measured eig-values
#
	
	histEigValFit <- hist(EigenValsIn,breaks=seq(minXFit,maxXFit,.10), plot=FALSE)
	maxYFit <- max(round(histEigValFit$density,1) + 0.1)
	minYFit <- 0

#print(c(minYFit,maxYFit))
#print(histEigVal$mids)
#
#	find values between Lambda_min & Lambda_max
#

	mids <- histEigValFit$mids;
	NoObsFit <- length(mids)
	Rhos_fit <- numeric(NoObsFit)

	for (i in 1:NoObsFit)
	{
		if ((mids[i] > LambdaMinFit) && (mids[i] < LambdaMaxFit))
		{
			Rhos_fit[i] = Qin/(2*pi*Variancein)*(sqrt((LambdaMaxFit - mids[i])*(mids[i]-LambdaMinFit))/mids[i])
		}
		else
		{
			Rhos_fit[i] <- 0;
		}
	}

#
#	calc cost-function
#
#	print(Rhos_fit)
	
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

#par(new=TRUE)	# do not clean drawing area

#plot(mids , Rhos_fit, type='o',col='green', xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")

#print(sqrt(costFunction)/NoSampleFit)

	costFunction <- 1000*sqrt(costFunction)/NoSampleFit
	}

}


####
#					MAIN
####

path <- 'S:/SC_AM/Afdeling/PRMTP/Employees/Bastiaan/projects Bastiaan/presentation Duisenberg/';

#path <- 'G:/Users/Bastiaan de Geeter/Documents/Rprogr/covariance/';
file <- 'daily_data_100_2.csv';
FileName <- paste(path, file, sep="");
inputFile <- file (FileName, "r");
aa <- read.csv(inputFile, header = TRUE)

close(inputFile)


NoFunds <- dim(aa)[2]/2;
NoObs <-  dim(aa)[1];

NoObs <-  250;

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

e <- var(adjReturns)
cc <- cor(returns)

lambdae <- eigen(e, symmetric=T, only.values = T);

minX <- 0.01
maxX <- ceiling(max(lambdae$values))+1.01


ee <- lambdae$values;
EigenVals <- ee;

histEigVal <- hist(ee,breaks=seq(minX,maxX,.10), plot=FALSE)

maxY <- max(round(histEigVal$density,1) + 0.1)
minY <- 0

histEigVal <- hist(ee,breaks=seq(minX,maxX,.10),main=NA,xlab="Eigenvalues",freq=F, ylim=c(minY,maxY),lwd=2)




Variance <- 1
LambdaMin <- Variance*(1-sqrt(1/Q))^2
LambdaMax <- Variance*(1+sqrt(1/Q))^2


Lambda <- (ceiling(LambdaMin*100):floor(LambdaMax*100))/100


Rho <- Q/(2*pi*Variance)*(sqrt((LambdaMax - Lambda)*(Lambda-LambdaMin))/Lambda)

par(new=TRUE)	# do not clean drawing area

plot(Lambda, Rho, type='l',col='red', lwd=2, xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")



out <- nlm(SQErr, p = c(Q, Variance), EigenVals=EigenVals, hessian =FALSE)


QOptim <- out$estimate[1]
VarianceOptim <- out$estimate[2]

LambdaMinOptim = VarianceOptim*(1-sqrt(1/QOptim))^2
LambdaMaxOptim = VarianceOptim*(1+sqrt(1/QOptim))^2
LambdaOptim <- (ceiling(LambdaMinOptim*1000):floor(LambdaMaxOptim*1000))/1000

RhosOptim = QOptim/(2*pi*VarianceOptim)*(sqrt((LambdaMaxOptim - LambdaOptim)*(LambdaOptim-LambdaMinOptim))/LambdaOptim)

par(new=TRUE)	# do not clean drawing area

plot(LambdaOptim , RhosOptim, type='l',col='green', xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")



out2 <- nlm(MaxLQMarcenkoPastur, p = c(3, Variance), observations=EigenVals, hessian =FALSE)


QOptim2 <- abs(out2$estimate[1])
VarianceOptim2 <- abs(out2$estimate[2])

#QOptim2 <- 2.90
#VarianceOptim2  <- 0.73


LambdaMinOptim2 = VarianceOptim2*(1-sqrt(1/QOptim2))^2
LambdaMaxOptim2 = VarianceOptim2*(1+sqrt(1/QOptim2))^2
LambdaOptim2 <- (ceiling(LambdaMinOptim2*1000):floor(LambdaMaxOptim2*1000))/1000

RhosOptim2 = QOptim2/(2*pi*VarianceOptim2)*(sqrt((LambdaMaxOptim2 - LambdaOptim2)*(LambdaOptim2-LambdaMinOptim2))/LambdaOptim2)

par(new=TRUE)	# do not clean drawing area

plot(LambdaOptim2 , RhosOptim2, type='l',col='blue', lwd=2, xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")


#par(new=TRUE)	# do not clean drawing area

#plot(LambdaOptim2 , RhosOptim2, type='l',col='green', lwd=2, xlim = c(minX, maxX),ylim=c(minY,maxY),ylab="",xlab="")


print("ready.")
