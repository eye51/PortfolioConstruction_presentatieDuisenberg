

minY <- min(underlyingNoStocks[,1:3])
maxY <- max(underlyingNoStocks[,1:3])


par(mfrow = c(3,1))
plot(underlyingNoStocks[,4],type='l',lwd=2,main="Reweighting of portfolio",ylab="Position",xlab="T")
par(new = FALSE)
plot(underlyingNoStocks[,2],type='l',col='red',lwd=2,ylab="Position",xlab="T")

par(new = FALSE)
plot(underlyingNoStocks[,9],type='l',col='blue',lwd=2,ylab="Position",xlab="T")


#lines(sigInSampl,type='l',lty='twodash',col='green',lwd=3)
#lines(sig1N,type='l',lty='twodash',col='green',lwd=3)

#legend("topright", legend = c("Trew=100", "Trew=50", "Trew=5", "in sample", "1/N"), col = c('black','blue','red','green', 'green'),lwd = c(2,2,2,2,2),lty = c('solid', 'solid', 'solid', 'twodash','twodash'), merge = TRUE)


print("Ready.")