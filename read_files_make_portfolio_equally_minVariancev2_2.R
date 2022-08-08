
rm(list=ls());      # empty workspace

# set date/time setting -> to prevent conversion errors
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

#
#   read file with stock prices. Structure of file is:
#   bbg_ticker fund1, px_bbg_ticker fund1   , ... , bbg_ticker fundN, px_bbg_ticker fundN
#   date  1         , price fund 1 at date 1, ... , date            , price fund N at date 1
#           .       ,           .           ,  .  ,         .       ,           .
#           .       ,           .           ,  .  ,         .       ,           .
#           .       ,           .           ,  .  ,         .       ,           .
#   date  T         , price fund 1 at date T, ... , date            , price fund N at date T
#

#path <- 'S:/SC_AM/Afdeling/PRMTP/Employees/Bastiaan/projects Bastiaan/presentation Duisenberg/';
path <- 'G:/Users/Bastiaan de Geeter/';
file <- 'daily_data_100_2.csv';
FileName <- paste(path, file, sep="");
inputFile <- file (FileName, "r");
aa <- read.csv(inputFile, header = TRUE)        # file is comma seperated. Read in table form. header constist of BBG_Tickers
close(inputFile)

#
#   declare variables   
#

NoFunds <- floor(dim(aa)[2]/2);
NoObs <-  dim(aa)[1];

Tcovariance <- 100;                                         # no days of history used to calculate Covariance matrix
Tstart <- Tcovariance+1;                                    # from which date to start with back-testing

headers <- attr(aa,'names')
funds <- headers[2*(1:NoFunds)-1]
prices <- as.matrix(aa[2*(1:NoFunds)])		            # convert datafrom from reading CSV file to matrix format
returns <- log(prices[2:NoObs,] / prices[1:(NoObs-1),])     # calculate log-returns from prices
datesPX <-  as.Date(aa[,1],"%d-%b-%y")                      # string-format dates -> date format R
noDaysBetweenReweigting <- 20;                              # how often is reweighted in simulation
noReweighting <- floor((NoObs-Tstart) / noDaysBetweenReweigting);    # number of reweightings over whole time T
firstTimeReweighting <- Tcovariance / noDaysBetweenReweigting;

#
#   Create Min Variance portfolio
#



underlyingWeights <- matrix(0,NoObs,NoFunds);

testWeights <- numeric(noReweighting);
In <- numeric(NoFunds);
In[1:NoFunds] <- 1;
for (i in 0:(noReweighting-1))
{
    
    CovMatrix <- cov(returns[(i*noDaysBetweenReweigting+1):(Tcovariance+i*noDaysBetweenReweigting),])
    
    #
    #   rebalance every xx days. In between no. Stocks held stay the same.
    
    underlyingWeightsMinVar = solve(CovMatrix,In) /( In %*% solve(CovMatrix,In));
    testWeights[i+1] <- sum(underlyingWeightsMinVar);
    
    for (j in 1:NoFunds)
    {
        underlyingWeights[(Tcovariance+i*noDaysBetweenReweigting+1) :(Tcovariance+(i+1)*noDaysBetweenReweigting),j] <- underlyingWeightsMinVar[j]/ prices[(Tcovariance+i*noDaysBetweenReweigting),j];
    }
    
}



##
##   Plot for debugging purpose
##


#maxY=max(underlyingWeights[,NoFunds]/underlyingWeights[1,NoFunds]);
#minY=min(underlyingWeights[,NoFunds]/underlyingWeights[1,NoFunds]);

#plot(underlyingWeights[,NoFunds]/underlyingWeights[1,NoFunds],type='l',ylim=c(minY,maxY),ylab="",xlab="")
#par(new=TRUE)
#plot((1/prices[,NoFunds])/(1/prices[1,NoFunds]),type='l',col='red',ylim=c(minY,maxY),ylab="",xlab="")

#par(new=TRUE)
 
#plot(underlyingWeights[,1]/underlyingWeights[1,1],type='l',ylim=c(minY,maxY),ylab="",xlab="")
#par(new=TRUE)
#plot((1/prices[,1])/(1/prices[1,1]),type='l',col='blue',ylim=c(minY,maxY),ylab="",xlab="")
 
 
#par(new=FALSE)
 
# plot(as.Date(aa[1:(NoObs-1),1],"%d-%b-%y"),returns[,1],type="l",ylab="Returns",xlab="Year") 
 
 port <- underlyingWeights*prices
 
 port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,]             # immediately after reweighting
 port[noDaysBetweenReweigting*(1:noReweighting),]                   # just before reweighting
 
 
##  
#           Calculate Returns of each stock for periods between rebalancing  
##
 
 stockReturns = (port[noDaysBetweenReweigting*(1:noReweighting),] )/port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,];
 
 
 portReturns <- numeric(noReweighting);
 portReturns[] <- 1 
 reinvest <- numeric(NoObs);
 reinvest[1:noDaysBetweenReweigting] <- 1
 for (i in (firstTimeReweighting+1):noReweighting)
 {
     portReturns[i] = sum(stockReturns[i,])/NoFunds
    reinvest[((i*noDaysBetweenReweigting)+1):((i+1)*noDaysBetweenReweigting)] <- reinvest[(i*noDaysBetweenReweigting)] * portReturns[i]
 }
 
 reinvest <- reinvest[1:NoObs]                              # cutt off at end time
 weigthsWithReinvesting <- underlyingWeights * reinvest     # every time weights are adjusted, full amount is invested. Need to take P/L of period
                                                            # into account
 
 
 portVal <- numeric(NoObs)  # value of the portfolio at all time-steps
 
 for (i in 1:NoObs)
 {
    portVal[i] <- prices[i,] %*% weigthsWithReinvesting[i,];        # this is correct, becuase weigthsWithReinvesting is the number of stocks held
 }
 
 returns1NPort  <- log(portVal[2:NoObs] / portVal[1:(NoObs-1)])
 
 plot(as.Date(aa[1:NoObs,1],"%d-%b-%Y"),portVal,type="l",ylab="Portfolio Value",xlab="Year")
 
 
 # return to original date/time setting
#Sys.setlocale("LC_TIME", lct)
