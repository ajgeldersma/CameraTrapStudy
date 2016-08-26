# Anna Moeller
# SCR simulation from Royle et al. 2014, p 44
# 10/5/2015

set.seed(36372)
area <- 1
x <- cbind(rep(seq(.1, .9, .2), each = 5), 
           rep(seq(.1, .9, .2), times = 5))
p0 <- 0.3
sigma <- 0.05
mu <- 50
N <- rpois(1, mu*area)
s <- cbind(runif(N, 0, 1),
           runif(N, 0, 1))
K <- 5
y <- matrix(NA, N, nrow(x))
for(i in 1:N){
  d.ij <- sqrt((x[, 1] - s[i, 1])^2 +
                 (x[, 2] - s[i, 2])^2)
  p.ij <- p0*exp(-d.ij^2/(2*sigma^2))
  y[i, ] <- rbinom(nrow(x), K, p.ij)
}