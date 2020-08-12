library(mclust); library(car)

# 確率分布の計算
X <- Davis[-12,c("weight","height")] # 12番目の異常標本を除去
result <- Mclust(X)
print(summary(result,parameters=TRUE))
plot(result)

# 異常度の計算
pi <- result$parameters$pro
X <- Davis[,c("weight","height")]
XX <- cdens(modelName=result$modelName,X,parameters=result$parameters)
a <- -log(as.matrix(XX) %*% as.matrix(pi))
plot(a,ylab="anomaly score")