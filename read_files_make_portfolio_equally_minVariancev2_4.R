
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


## 28-3-2010 -> nog checken -> lijkt OK.

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

Tcovariance <- 500;                                         # no days of history used to calculate Covariance matrix
Tstart <- Tcovariance+1;                                    # from which date to start with back-testing

headers <- attr(aa,'names')
funds <- headers[2*(1:NoStocks)-1]
prices <- as.matrix(aa[2*(1:NoStocks)])		            # convert datafrom from reading CSV file to matrix format
returns <- log(prices[2:NoObs,] / prices[1:(NoObs-1),])     # calculate log-returns from prices
datesPX <-  as.Date(aa[,1],"%d-%b-%y")                      # string-format dates -> date format R
noDaysBetweenReweigting <- 100;                              # how often is reweighted in simulation
noReweighting <- floor((NoObs-Tstart) / noDaysBetweenReweigting);    # number of reweightings over whole time T
firstTimeReweighting <- Tcovariance / noDaysBetweenReweigting;

#
#   Create Min Variance portfolio
#

underlyingNoStocks <- matrix(0,NoObs,NoStocks);             # position in each stock, in number of stocks held in portfolio
portVal <- numeric(NoObs)                                   # portfolio value at each timestep t

portVal[1:Tcovariance] <- 1;    # first Tcovariance there is no covariance matrix available -> no investing


testWeights <- numeric(noReweighting);
In <- numeric(NoStocks);                     # identity vector
In[1:NoStocks] <- 1;

for (i in 0:(noReweighting-1))
{
    
    CovMatrix <- cov(returns[(i*noDaysBetweenReweigting+1):(i*noDaysBetweenReweigting + Tcovariance),])
    print(c(i,(i*noDaysBetweenReweigting+1),(i*noDaysBetweenReweigting + Tcovariance)))
    #
    #   rebalance every xx days. In between no. Stocks held stay the same.
    
    underlyingWeigthsMinVar = solve(CovMatrix,In) /( In %*% solve(CovMatrix,In));
    testWeights[i+1] <- sum(underlyingWeigthsMinVar);
    
    for (j in 1:NoStocks)    # keep no. stocks constant in between reweighting
    {
        underlyingNoStocks[(Tcovariance+i*noDaysBetweenReweigting+1) :(Tcovariance+(i+1)*noDaysBetweenReweigting),j] <- underlyingWeigthsMinVar[j]/ prices[(Tcovariance+i*noDaysBetweenReweigting),j];
    }
}

#
#   no stocks for time steps after last reweighting
#

for (i in (Tcovariance+noReweighting*noDaysBetweenReweigting+1):NoObs)
{
   underlyingNoStocks[i,] <- underlyingNoStocks[(i-1),]        
}

port <- underlyingNoStocks*prices
 
 port[noDaysBetweenReweigting*(0:(noReweighting-1))+1,]             # immediately after reweighting -> weights 1/N
 port[noDaysBetweenReweigting*(1:noReweighting),]                   # just before reweighting
 
 
##  
#           Calculate Returns of each stock for periods between rebalancing  
##
 
stockReturns = (prices[(Tcovariance+noDaysBetweenReweigting*(1:noReweighting)),] )/prices[(Tcovariance+noDaysBetweenReweigting*(0:(noReweighting-1))+1),];
 
 
 portReturns <- numeric(noReweighting);
 reinvest <- numeric(NoObs);
 reinvest[1:((firstTimeReweighting+1)*noDaysBetweenReweigting+1)] <- 1
 
 for (i in (firstTimeReweighting+1):(noReweighting+firstTimeReweighting))
 {
    
    portReturns[i-firstTimeReweighting] = sum(stockReturns[i-firstTimeReweighting,])/NoStocks
    reinvest[((i*noDaysBetweenReweigting)+1):((i+1)*noDaysBetweenReweigting)] <- reinvest[(i*noDaysBetweenReweigting)] * portReturns[i-firstTimeReweighting]
 }
 
 reinvest <- reinvest[1:NoObs]                              # cutt off at end time
 
 weigthsWithReinvesting <- matrix(0,NoObs,NoStocks); 
 
 for (j in 1:NoStocks)
 {
 weigthsWithReinvesting[,j] <- underlyingNoStocks[,j] * reinvest     # every time weights are adjusted, full amount is invested. Need to take P/L of period
                                                            # into account
 
 }

 
 portVal <- numeric(NoObs)  # value of the portfolio at all time-steps
 
 for (i in 1:NoObs)
 {
    portVal[i] <- prices[i,] %*% weigthsWithReinvesting[i,];        # this is correct, becuase weigthsWithReinvesting is the number of stocks held
 }
 
 returns1NPort  <- log(portVal[2:NoObs] / portVal[1:(NoObs-1)])
  varPort = var(returns1NPort[Tstart:(NoObs-1)])*252
 
 yMin <- min(portVal[Tstart:NoObs])
 yMax <- max(portVal[Tstart:NoObs])
 
 ytest<-numeric(NoObs);
 
 
 for (i in 0:(noReweighting-1))
 {
    
    ytest[(Tstart+(i*noDaysBetweenReweigting)):(Tstart+(i+1)*noDaysBetweenReweigting)] <- yMin + (ceiling((i+1)/2)-(i+1)/2)*(yMax-yMin)

 
 }
 
 par(new=FALSE)
 plot(as.Date(aa[Tstart:NoObs,1],"%d-%b-%Y"),portVal[Tstart:NoObs],type="l",ylab="Portfolio Value",xlab="Year",ylim=c(yMin,yMax))
 par(new=TRUE)
 plot(as.Date(aa[Tstart:NoObs,1],"%d-%b-%Y"),ytest[Tstart:NoObs],type="l",ylab="Portfolio Value",xlab="Year",ylim=c(yMin,yMax),col='red')
 par(new=TRUE)
 plot(as.Date(aa[Tstart:NoObs,1],"%d-%b-%Y"),reinvest[Tstart:NoObs],type="l",ylab="Portfolio Value",xlab="Year",ylim=c(yMin,yMax),col='green')
 
 # return to original date/time setting
#Sys.setlocale("LC_TIME", lct)
print("Ready.")