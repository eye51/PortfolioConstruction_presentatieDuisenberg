SQErr <- function(Q,Variance,EigenVals)
{

#
#	input fit:
#


LambdaMinFit = Variance*(1-sqrt(1/Q))^2
LambdaMaxFit = Variance*(1+sqrt(1/Q))^2

minXFit <- 0.01
maxXFit <- ceiling(max(lambdae$values))+1.01

#
#	distribution measured eig-values
#

histEigValFit <- hist(EigenVals,breaks=seq(minXFit,maxXFit,.10), plot=FALSE)
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


SQErr <- costFunction
}
