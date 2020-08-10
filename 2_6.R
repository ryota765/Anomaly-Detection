library(MASS)
X <- road / road$drivers
X <- as.matrix(log(X[,-2] +1))
mx <- colMeans(X)
Xc <- X - matrix(1,nrow(X),1) %*% mx
Sx <- t(Xc) %*% Xc / nrow(X)
a <- rowSums((Xc %*% solve(Sx)) * Xc)/ncol(x)
plot(a,xlab="index",ylab="anomaly score",ylim=c(-1,30)/ncol(X))
lines(0:30,rep(1,length(0:30)),col="red",lty=2)

xc_prime <- Xc["Calif",]
SN1 <- 10*log10(xc_prime^2/diag(Sx))
barplot(SN1)
                      