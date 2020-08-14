Dtr <- nottem[1:120]; xi <- nottem[121:240]
Tt <- length(xi)
ar.model <- ar(Dtr); print(ar.model)
r <- ar.model$order
alpha <- ar.model$ar
xmean <- ar.model$x.mean; sig2 <- ar.model$var.pred; N <- Tt - r
X <- t(embed(xi-xmean, r))[,1:N]
ypred <- t(X) %*% alpha + xmean
y <- xi[(1+r):Tt]
a <- (y - as.numeric(ypred))^2/sig2
plot(a,ylab="anomaly score",type="l")
