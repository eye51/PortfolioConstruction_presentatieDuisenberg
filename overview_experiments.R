
rm(list=ls())

path <- 'G:/Users/Bastiaan de Geeter/Documents/Rprogr/presentatie Duisenberg/';
file <- 'experiments_overview_n=100.csv';
FileName <- paste(path, file, sep="");
inputFile <- file (FileName, "r");
aa <- read.csv(inputFile, header = TRUE)        # file is comma seperated. Read in table form. header constist of BBG_Tickers
close(inputFile)

#
#   declare variables   
#

NoExp <- floor(dim(aa)[2]/2);
NoObs <-  dim(aa)[1];


# get price information from readed input file

headers <- attr(aa,'names')

Tcov <- aa[,1]
sigT100 <- aa[,2]
sigT50 <- aa[,3]
sigT5 <- aa[,4]
sigInSampl <- aa[,5]
sig1N <- aa[,6]

minY <- 0.1
maxY <- max(sigT100,sigT50,sigT5)


plot(Tcov,sigT100,type='l',lwd=2,ylim=c(minY,maxY),main="Volatility of Portfolio as function of Tcov",ylab="Vol.",xlab="Tcovariance")

lines(Tcov, sigT50,type='l',col='blue',lwd=2)
lines(Tcov, sigT5,type='l',col='red',lwd=2)
lines(Tcov, sigInSampl,type='l',lty='twodash',col='green',lwd=3)
lines(Tcov, sig1N,type='l',lty='twodash',col='green',lwd=3)

legend("topright", legend = c("Trew=100", "Trew=50", "Trew=5", "in sample", "1/N"), col = c('black','blue','red','green', 'green'),lwd = c(2,2,2,2,2),lty = c('solid', 'solid', 'solid', 'twodash','twodash'), merge = TRUE)
#+                text.width = strwidth("1,000,000"),
#+               lty = 1:2, xjust = 1, yjust = 1,
#+                title = "Experiment")


#temp <- legend("topright", legend = c(" ", " "),
#+                text.width = strwidth("1,000,000"),
#+               lty = 1:2, xjust = 1, yjust = 1,
#+                title = "Experiment")
# text(temp$rect$left + temp$rect$w, temp$text$y,
#+      c("1,000", "1,000,000"), pos=2)
#par(new=TRUE)
#plot(Tcov,sigT50,type='l',col='blue',lwd=2,ylim=c(minY,maxY),ylab="",xlab="")

#par(new=TRUE)
#plot(Tcov,sigT5,type='l',col='red',lwd=2,ylim=c(minY,maxY),ylab="",xlab="")

#par(new=TRUE)
#plot(Tcov,sigInSampl,type='l',lty='twodash',col='green',lwd=3,ylim=c(minY,maxY),ylab="",xlab="")

#par(new=TRUE)
#plot(Tcov,sig1N,type='l',lty='twodash',col='green',lwd=3,ylim=c(minY,maxY),ylab="",xlab="")



print("Ready.")