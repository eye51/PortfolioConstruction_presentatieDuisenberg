t <- 5000;
m <- 100;
h <- array(rnorm(m*t),c(m,t)); # Time series in rows
e <- h %*% t(h)/t; # Form the correlation matrix
lambdae <- eigen(e, symmetric=T, only.values = T);
ee <- lambdae$values;
hist(ee,breaks=seq(0.01,3.01,.10),
main=NA,xlab="Eigenvalues",freq=F)