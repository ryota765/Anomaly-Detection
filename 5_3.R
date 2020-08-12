library(MASS)

# 固有値を表示
cc <- c("Min.Price","Price","Max.Price","MPG.city","MPG.highway","EngineSize","Horsepower","RPM",
        "Rev.per.mile","Fuel.tank.capacity","Length","Wheelbase","Width","Turn.circle","Weight")
mask <- is.element(colnames(Cars93),cc)
Xc <- t(scale(Cars93[,mask]))
S <- Xc %*% t(Xc); evd <- eigen(S)
plot(evd$values,type="b",xlab="index",ylab="eigenvalue")

# 異常度の計算（散布行列）
# エルボー則によりm=2で以降の計算を行う
m <- 2
x2 <- t(evd$vectors[,1:m]) %*% Xc
a1 <- colSums(Xc*Xc) - colSums(x2*x2)
idx <- order(a1,decreasing=T)[1:6]; print(a1[idx])

# 異常度の計算（グラム行列）
G <- t(Xc) %*% Xc; evd <- eigen(G)
Lam_12 <- diag(evd$values[1:m]^(-1/2))
xx2 <- Lam_12 %*% t(evd$vectors[,1:m]) %*% t(Xc) %*% Xc
aa1 <- colSums(Xc*Xc) - colSums(xx2*xx2)
idx <- order(aa1,decreasing=T)[1:6]; print(aa1[idx])