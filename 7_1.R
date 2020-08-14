library(FNN)

X <- read.table(file="../ECG_data/qtdbsel102.txt")
w <- 100; nk <- 1 # 窓幅と近傍数
Xtr <- X[1:3000,2]; Dtr <- embed(Xtr,w)
X <- X[3001:6000,2]; D <- embed(X,w)
d <- knnx.dist(Dtr,D,k=nk); a <- d[,1]
plot(a,ylab="anomaly score",type="l")
