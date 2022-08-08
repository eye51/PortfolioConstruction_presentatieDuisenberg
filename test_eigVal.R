
In <- numeric(2)                     # identity vector
In[1:2] <- 1;


AA <- matrix(c(2,2,3,1),2,2,dimnames = list(c("row1", "row2"), c("C.1", "C.2")))
EigAA <- eigen(AA, symmetric = FALSE, only.values = FALSE, EISPACK =TRUE) 

EigenVals <- matrix(0,2,2)

EigenVec <- EigAA$vectors



diag(EigenVals) <- EigAA$values




testCorr <- EigenVec %*% EigenVals %*% solve(EigenVec,In)

testA1 <- AA %*% EigenVec
testA2 <- EigenVals * EigenVec


 sm <- eigen(AA)
 V <- sm$vectors
 t(V) %*% V
 V %*% diag(sm$values) %*% t(V)
