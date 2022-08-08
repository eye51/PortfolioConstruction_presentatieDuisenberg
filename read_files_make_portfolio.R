
rm(list=ls());      # empty workspace
path <- 'S:/SC_AM/Afdeling/PRMTP/Employees/Bastiaan/projects Bastiaan/presentation Duisenberg/';
#path <- 'G:/Users/Bastiaan de Geeter/';
file <- 'daily_data_100_2.csv';

FileName <- paste(path, file, sep="");

inputFile <- file (FileName, "r");


aa <- read.csv(inputFile, header = TRUE)

close(inputFile)



NoFunds <- floor(dim(aa)[2]/2);
NoObs <-  dim(aa)[1];


headers <- attr(aa,'names')
funds <- headers[2*(1:NoFunds)-1]
prices <- as.matrix(aa[2*(1:NoFunds)])		# convert datafrom from reading CSV file to matrix format
returns <- log(prices[2:NoObs,] / prices[1:(NoObs-1),])
datesPX <-  as.Date(aa[,1],"%d-%b-%y")
noDaysBetweenReweigting <- 20;      
noReweighting <- floor(NoObs / noDaysBetweenReweigting);



#
#   Create equally weigthed portfolio. Porfolio 'value' at start = 1
#   
#


underlyingWeights <- 1/ (NoFunds*prices[,]);   # daily rebalancing

for (i in 0:(noReweighting-1))
{
    
    
    #
    #   rebalance every xx days
    #
    
    for (j in 1:NoFunds)
    {
        underlyingWeights[(i*noDaysBetweenReweigting+1) :((i+1)*noDaysBetweenReweigting),j] <- 1/ (NoFunds*prices[(i*noDaysBetweenReweigting+1),j]);
    }
    
}



#
#   Plot for debugging purpose
#


maxY=max(underlyingWeights[,NoFunds]/underlyingWeights[1,NoFunds]);
minY=min(underlyingWeights[,NoFunds]/underlyingWeights[1,NoFunds]);


plot(underlyingWeights[,NoFunds]/underlyingWeights[1,NoFunds],type='l',ylim=c(minY,maxY),ylab="",xlab="")
par(new=TRUE)
plot((1/prices[,NoFunds])/(1/prices[1,NoFunds]),type='l',col='red',ylim=c(minY,maxY),ylab="",xlab="")

par(new=TRUE)
 
plot(underlyingWeights[,1]/underlyingWeights[1,1],type='l',ylim=c(minY,maxY),ylab="",xlab="")
par(new=TRUE)
plot((1/prices[,1])/(1/prices[1,1]),type='l',col='blue',ylim=c(minY,maxY),ylab="",xlab="")
 
 
par(new=FALSE)
 
 plot(as.Date(aa[1:(NoObs-1),1],"%d-%b-%y"),returns[,1],type="l",ylab="Returns",xlab="Year") 
 
 port <- underlyingWeights*prices
 
 port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,]             # immediately after reweighting
 port[noDaysBetweenReweigting*(1:noReweighting),]                   # just before reweighting
 
 
##  
#           Calculate Returns of each stock for periods between rebalancing  
##
 
 stockReturns = (port[noDaysBetweenReweigting*(1:noReweighting),] )/port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,];
 
 
 portReturns <- numeric(noReweighting);
 reinvest <- numeric(NoObs);
 reinvest[1:noDaysBetweenReweigting] <- 1
 for (i in 1:noReweighting)
{
    i
 portReturns[i] = sum(stockReturns[i,])/NoFunds
 reinvest[((i*noDaysBetweenReweigting)+1):((i+1)*noDaysBetweenReweigting)] <- reinvest[(i*noDaysBetweenReweigting)] * portReturns[i]
 }
 
 reinvest <- reinvest[1:NoObs]
 
 weigthsWithReinvesting <- underlyingWeights * reinvest
 
 
 portVal <- numeric(NoObs)  # value of the portfolio at all time-steps
 
 for (i in 1:NoObs)
 {
    portVal[i] <- prices[i,] %*% weigthsWithReinvesting[i,]
 }
 
 returns1NPort  <- log(portVal[2:NoObs] / portVal[1:(NoObs-1)])
 
 plot(as.Date(aa[1:NoObs,1],"%d-%b-%y"),portVal,type="l",ylab="Portfolio Value",xlab="Year")
 