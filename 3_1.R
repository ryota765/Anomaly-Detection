install.packages("car")
library(car)
library(MASS)

data(Davis)
Davis

N <- length(Davis$weight)
mu <- mean(Davis$weight)

# モーメント法による推定
si <- sd(Davis$weight)*(N-1)/N
kmo <- (mu/si)^2
smo <- si^2/mu

# 最尤法による推定
ml <- fitdistr(Davis$weight,"gamma")
kml <- ml$estimate["shape"]
sml <- 1/ml$estimate["rate"]

a <- Davis$weight/smo - (kmo-1)*log(Davis$weight/smo)
th <- order(a,decreasing=T)[0.01*N]
plot(a,ylab="anomaly score")
lines(0:200,rep(a[th],length(0:200)),col="red",lty=2)