install.packages("car")
library(car)
data(Davis)
Davis
hist(Davis$weight, xlim=c(35,105),breaks=14)

X <- cbind(Davis$weight, Davis$height)
plot(X[,1],X[,2],pch=16,xlab="weight",ylab="height")

mx <- colMeans(X)
Xc <- X - matrix(1,nrow(X),1) %*% mx
Sx <- t(Xc) %*% Xc / nrow(X)
a <- rowSums((Xc %*% solve(Sx)) * Xc)
plot(a,xlab="index",ylab="anomaly score")
lines(0:200,rep(th,length(0:200)),col="red",lty=2)