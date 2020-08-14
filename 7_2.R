dt <- read.table(file="../ECG_data/qtdbsel102.txt")
xi <- dt[3001:6000,2]
w <- 50; m <- 2; k <- w/2; L <- k/2; Tt <- length(xi)
score <- rep(0,Tt)

for(t in (w+k):(Tt-L+1)){
  tstart <- t-w-k+1; tend <- t-1
  X1 <- t(embed(xi[tstart:tend],w))
  X1 <- X1[w:1,]
  
  tstart <- t-w-k+1+L; tend <- t-1+L
  X2 <- t(embed(xi[tstart:tend],w))
  X2 <- X2[w:1,]
  
  U1 <- svd(X1)$u[,1:m]
  U2 <- svd(X2)$u[,1:m]
  sig1 <- svd(t(U1)%*%U2)$d[1]
  score[t] <- 1 - sig1^2
}

plot(score,ylab="anomaly score",type="l")
