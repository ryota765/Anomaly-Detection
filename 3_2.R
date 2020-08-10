# Generate data
N <- 1000
pi0 <- 0.6
pi1 <- 0.4
mu0 <- 3
mu1 <- 0
sig0 <- 0.5
sig1 <- 3

attr <- sample(0:1,N,replace=T,prob=c(pi0,pi1))
x <- rep(-99,N)
x[which(attr ==0)] <- rnorm(length(which(attr ==0)),mu0,sig0)
x[which(attr ==1)] <- rnorm(length(which(attr ==1)),mu1,sig1)

# Model presumption
pi0 <- 0.5
pi1 <- 0.5
mu0 <- 5
mu1 <- -5
sig0 <- 1
sig1 <- 5

for(iteration in 1:10){
  piN0 <- pi0*dnorm(x,mu0,sig0); piN1 <- pi1*dnorm(x,mu1,sig1)
  qn0 <- piN0/(piN0+piN1); qn1 <- piN1/(piN0+piN1)
  pi0 <- sum(qn0)/N; pi1 <- sum(qn1)/N
  mu0 <- sum(qn0*x)/(N*pi0); mu1 <- sum(qn1*x)/(N*pi1)
  sig0 <- sqrt(sum(qn0*(x-mu0)*(x-mu0))/(N*pi0))
  sig1 <- sqrt(sum(qn1*(x-mu1)*(x-mu1))/(N*pi1))
}