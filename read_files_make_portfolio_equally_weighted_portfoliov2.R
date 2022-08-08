
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

## 28-3-2010 -> check !


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

NoStocks <- floor(dim(aa)[2]/2);
NoObs <-  dim(aa)[1];


# get price information from readed input file

headers <- attr(aa,'names')
funds <- headers[2*(1:NoStocks)-1]
prices <- as.matrix(aa[2*(1:NoStocks)])		            # convert datafrom from reading CSV file to matrix format
returns <- log(prices[2:NoObs,] / prices[1:(NoObs-1),])     # calculate log-returns from prices
datesPX <-  as.Date(aa[,1],"%d-%b-%y")                      # string-format dates -> date format R


##
#       Reweighting Settings
##

noDaysBetweenReweigting <- 50;                              # how often is reweighted in simulation
noReweighting <- floor(NoObs / noDaysBetweenReweigting);    # number of reweightings over whole time T

#
#   Create equally weigthed portfolio. Porfolio 'value' at start = 1   
#   underlyingWeights = number of stocks held at time t, to creaty equaly weighted portfolio. Weight of each stock is 1/NoStocks

underlyingWeights <- 1/ (NoStocks*prices[,]);   # daily rebalancing -> no stocks in portfolio to create equally weighted portfolio

for (i in 0:(noReweighting-1))
{
    
    
    #
    #   rebalance every xx days. In between no. Stocks held stay the same.
  
    
    for (j in 1:NoStocks)
    {
        underlyingWeights[(i*noDaysBetweenReweigting+1) :((i+1)*noDaysBetweenReweigting),j] <- 1/ (NoStocks*prices[(i*noDaysBetweenReweigting+1),j]);
    }
    
}



##
##   Plot for debugging purpose
##


#maxY=max(underlyingWeights[,NoStocks]/underlyingWeights[1,NoStocks]);
#minY=min(underlyingWeights[,NoStocks]/underlyingWeights[1,NoStocks]);

#plot(underlyingWeights[,NoStocks]/underlyingWeights[1,NoStocks],type='l',ylim=c(minY,maxY),ylab="",xlab="")
#par(new=TRUE)
#plot((1/prices[,NoStocks])/(1/prices[1,NoStocks]),type='l',col='red',ylim=c(minY,maxY),ylab="",xlab="")

#par(new=TRUE)
 
#plot(underlyingWeights[,1]/underlyingWeights[1,1],type='l',ylim=c(minY,maxY),ylab="",xlab="")
#par(new=TRUE)
#plot((1/prices[,1])/(1/prices[1,1]),type='l',col='blue',ylim=c(minY,maxY),ylab="",xlab="")
 
 
#par(new=FALSE)
 
# plot(as.Date(aa[1:(NoObs-1),1],"%d-%b-%y"),returns[,1],type="l",ylab="Returns",xlab="Year") 
 
 port <- underlyingWeights*prices
 
 port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,]             # immediately after reweighting -> weights 1/N
 port[noDaysBetweenReweigting*(1:noReweighting),]                   # just before reweighting
 
 
##  
#           Calculate Returns of each stock for periods between rebalancing  
##
 
 
 
 stockReturns = (prices[noDaysBetweenReweigting*(1:noReweighting),] )/prices[noDaysBetweenReweigting*(0:(noReweighting-1))+1,];
 
 
 portReturns <- numeric(noReweighting);
 reinvest <- numeric(NoObs);
 reinvest[1:noDaysBetweenReweigting] <- 1
 for (i in 1:noReweighting)
 {
    portReturns[i] = sum(stockReturns[i,])/NoStocks         # dit werkt alleen voor Equally weighted stocks!!
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

 yMin <- min(portVal)
 yMax <- max(portVal)
 ytest<-numeric(NoObs);
 
 
 for (i in 0:(noReweighting-1))
 {
    
    ytest[(i*noDaysBetweenReweigting):(i+1)*noDaysBetweenReweigting] <- yMin + (ceiling((i+1)/2)-(i+1)/2)*(yMax-yMin)

 
 }
  
 returns1NPort  <- log(portVal[2:NoObs] / portVal[1:(NoObs-1)])
  varPort = var(returns1NPort)*252
 
 
 par(new=FALSE)
 plot(as.Date(aa[1:NoObs,1],"%d-%b-%Y"),portVal,type="l",ylab="Portfolio Value",xlab="Year")
  par(new=TRUE)
 plot(as.Date(aa[1:NoObs,1],"%d-%b-%Y"),ytest[1:NoObs],type="l",ylab="Portfolio Value",xlab="Year",ylim=c(yMin,yMax),col='red')
 par(new=TRUE)
 plot(as.Date(aa[1:NoObs,1],"%d-%b-%Y"),reinvest[1:NoObs],type="l",ylab="Portfolio Value",xlab="Year",ylim=c(yMin,yMax),col='green')
 
 # return to original date/time setting
#Sys.setlocale("LC_TIME", lct)

print("Ready.")
 