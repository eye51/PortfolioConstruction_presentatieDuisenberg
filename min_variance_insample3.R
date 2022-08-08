##
#
#           Create min variance portfolio, in sample. So use complete price history to calc. Covariance Matrix and
#           use complete history to minimize variance of portfolio
#
##


## 28-3-2010 -> check !

rm(list=ls());                                                      # empty workspace
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

path <- 'S:/SC_AM/Afdeling/PRMTP/Employees/Bastiaan/projects Bastiaan/presentation Duisenberg/';

#path <- 'G:/Users/Bastiaan de Geeter/';
file <- 'daily_data_50_3.csv';
FileName <- paste(path, file, sep="");
inputFile <- file (FileName, "r");
aa <- read.csv(inputFile, header = TRUE)        # file is comma seperated. Read in table form. header constist of BBG_Tickers
close(inputFile)

#
#   declare variables   
#

NoFunds <- floor(dim(aa)[2]/2); 
NoObs <-  dim(aa)[1];

Tcovariance <- (NoObs-1);                                         # no days of history used to calculate Covariance matrix
                                                                # because we are looking completely in-sample -> T = no. of returns


# get price informatino from read input data

headers <- attr(aa,'names')
funds <- headers[2*(1:NoFunds)-1]
prices <- as.matrix(aa[2*(1:NoFunds)])		            # convert datafrom from reading CSV file to matrix format
returns <- log(prices[2:NoObs,] / prices[1:(NoObs-1),])     # calculate log-returns from prices
datesPX <-  as.Date(aa[,1],"%d-%b-%y")                      # string-format dates -> date format R


#
#   Create Min Variance portfolio
#

underlyingWeights <- matrix(0,NoObs,NoFunds);               # clear covariance matrix

In <- numeric(NoFunds);                                     # Identity Vector
In[1:NoFunds] <- 1;
CovMatrix <- cov(returns[1:Tcovariance,])
    
underlyingWeightsMinVar = solve(CovMatrix,In) /( In %*% solve(CovMatrix,In));

    
for (j in 1:NoFunds)
{
    underlyingWeights[,j] <- underlyingWeightsMinVar[j]/ prices[1,j];
}
    



#port <- prices %*% underlyingWeights[1,]
port2 <- returns %*% underlyingWeightsMinVar;

portVal2 <- numeric(NoObs)
portVal2[1] <- 1

for (i in 2:NoObs)
{
    portVal2[i] <- portVal2[i-1]*exp(port2[i-1]);
}
 
portReturns <- log(portVal2[2:NoObs] / portVal2[1:(NoObs-1)])
varPort = var(portReturns)*252
 
 
plot(as.Date(aa[1:NoObs,1],"%d-%b-%Y"),portVal2,type="l",ylab="Portfolio Value",xlab="Year")
 


 
##  
#           Calculate Returns of each stock for periods between rebalancing  
##
 
# stockReturns = (port[noDaysBetweenReweigting*(1:noReweighting),] )/port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,];
 
 
 
 
 
#varPort = var(portReturns)
  
# return to original date/time setting
Sys.setlocale("LC_TIME", lct)

print("Ready.")