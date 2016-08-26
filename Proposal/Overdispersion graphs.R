# Anna Moeller
# Overdispersion 
# 10/4/2015

# Load packages
library(dplyr)

# Set working directory
setwd("C:/Users/anna.moeller/Documents/Camera Trap Study/Research Proposal")

# For now, let's make them up:
rpois.od <- function (n, lambda, d = 1) {
  if (d == 1){
    rpois(n, lambda)
  } else {
    rnbinom(n, size=(lambda/(d - 1)), mu = lambda)
  }
}

# Run it
v <- rpois.od(100, lambda = 5, d = 1)
v2 <- rpois.od(100, lambda = 5, d = 3)

#
jpeg("not_overdispersed.jpg")

# Not overdispersed Histogram
hist(v, prob = T, main = "Count Data",
     xlab = "Number of Elk", ylim = c(0, 0.2))
legend(x = "topright", legend = c("Normal", "Poisson"), col = c("blue", "red"), 
       lty = c(1, 0), pch = c(NA_integer_, 19))

# Normal curve
curve(dnorm(x, mean = mean(v), sd = sd(v)), add = T, col = "blue")

# Poisson points
x <- sort(rpois(10000, lambda = 5))
y <- dpois(x, lambda = 5)
points(y ~ x, col = "red", pch = 19)

#
dev.off()
##################

#
jpeg("overdispersed.jpg")

# Overdispersed histogram
hist(v2, prob = T, main = "Overdispersed Count Data",
     xlab = "Number of Elk", ylim = c(0, 0.2))
legend(x = "topright", legend = c("Normal", "Poisson"), col = c("blue", "red"), 
       lty = c(1, 0), pch = c(NA_integer_, 19))

# Normal curve
curve(dnorm(x, mean = mean(v2), sd = sd(v2)), add = T, col = "blue")

# Poisson points
points(y ~ x, col = "red", pch = 19)

#
dev.off()


#########################################################################

# Let's make it not overdispersed! 
p <- rpois(100, lambda = 5)

hist(p, prob = T, main = "Count Data", xlab = "Number of Elk")
legend(x = "topright", legend = c("Normal", "Poisson"), col = c("blue", "red"), 
       lty = c(1, 0), pch = c(NA_integer_, 1))

# Normal curve
curve(dnorm(x, mean = mean(v), sd = sd(v)), add = T, col = "blue")

# Poisson points
x <- sort(rpois(10000, lambda = 5))
y <- dpois(x, lambda = 5)
points(y~x, col = "red")

jpeg("count.jpg")
dev.off()

####################################################################
p <- rpois(100, lambda = 1)
hist(p, breaks = 4, prob = T, main = "D", xlab = "possible values of D", ylab = "Probability")
legend("topright", "mu = 1")

# Poisson "curve"
x <- sort(rpois(10000, lambda = 5))
y <- dpois(x, lambda = 5)
lines(x, y, lty = 2, type = "o", col = "red")

####################################################################

# Read in raw aerial survey data
raw <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Research Proposal/Query1.csv")

# Look at 1992 data
raw2 <- filter(raw, Bio.Year == 1992)

# Are any records all zeros? 
raw2[which(raw2$Total == 0), ] # yes, 4 of them...

# Histogram of the raw counts in stratum 2
# Histogram of counts less than 50
# Histogram of counts only in middle
hist(raw2$Total[raw2$Stratum == 2], prob = T)
hist(raw2$Total[raw2$Stratum == 2 & raw2$Total < 50], prob = T)
hist(raw2$Total[raw2$Stratum == 2 & raw2$Total < 50 & raw2$Total > 15], prob = T)

# Well let's work with the last one for now...
xx <- raw2[raw2$Stratum == 2 & raw2$Total < 50 & raw2$Total > 15, ]
# ya hmmm there are only 14 records here

lines(density(rnorm(1000000, mean=mean(xx$Total, na.rm=TRUE), sd=sd(xx$Total, na.rm=TRUE))), col="blue")
#########################################################################

# For now, let's make them up: 
v <- rnorm(1, 30, 10)
for (i in 2:100){
  v <- c(v, 0.5*v[i - 1] + 0.5*rnorm(1, 30, 10))
  v <- round(v)
} 

# Histogram
hist(v, prob = T)
curve(dnorm(x, mean = mean(v), sd = sd(v)), add = T, col = "blue")



# For now, let's make them up:
N <- 100
rho <- .6
log.lambda <- 1 + arima.sim(model = list(ar = rho), n = N)
y <- rpois(N, lambda = exp(log.lambda))

# Histogram
hist(y, prob = T)
curve(dnorm(x, mean = mean(y), sd = sd(y)), add = T, col = "blue")



# For now, let's make them up: 
raw <- c(rnorm(50, 100, 30), rnorm(20, 200, 10))
raw0 <- c(raw, rep(0, times = 30))

# Put graphs on same page
par(mfrow = c(2,1))

# Plot with excess zeros
hist(raw0, prob = T)
# Add normal curve
curve(dnorm(x, mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)), add = T, col = "red")
# Add a negative binomial curve
curve(dnbinom(x, mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)), add = T, col = "red")

# Plot without excess zeros
hist(raw, prob = T)
# Add normal curve
curve(dnorm(x, mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)), add = T, col = "red")
