# Understanding PDF, likelihood, and logL
# Anna Moeller
# 8/11/2016


### What's up with the likelihood not working correctly with the product? 

# Understanding likelihoods
# Think of a general exponential distribution
# PDF: P(T = t | lambda = l) = l*exp(-l*t)

# Think of an exponential distribution with lambda = 0.5
# PDF: P(T = t | lambda = 0.5) = 0.5*exp(-0.5*t)

# Draw realizations of this distribution
xx <- rexp(10000, rate = 0.5)
hist(xx, prob = T)

# The probability that any realization will be a 2 is
#  P(T = 2|lambda = 0.5) = 0.5*exp(-0.5*2)
0.5*exp(-0.5*2)
dexp(2, 0.5)

# The probability (yy) of drawing each realization (xx) we did is
yy <- dexp(xx, 0.5)
plot(xx, yy)

# Likelihood
# Likelihood that lambda = 0.5 when we drew a 2
#  L(lambda = 0.5 | T = 2) = 0.5*exp(-0.5*2)
0.5*exp(-0.5*2)

# The likelihood for more than one observation is 
# L(lambda = 0.5 | T = 2, 3, n) = 0.5*exp(-0.5*2) * 0.5*exp(-0.5*3) * 0.5*exp(-0.5*n)
sum(0.5*exp(-0.5*xx))
sum(dexp(xx, 0.5, log = F))

# Then look at that value over all the values of l
out <- NA
l <- (1:10000)/100
for(i in seq_along(l)) {
  out[i] <- sum(l[i] * exp(-l[i]*xx))
}
plot(l, out, xlab = "l", ylab = "logL(lambda = l)")

# With the R function
out <- NA
l <- (1:10000)/100
for(i in seq_along(l)) {
  out[i] <- sum(dexp(xx, l[i], log = F))
}
plot(l, out, xlab = "l", ylab = "logL(lambda = l)")

### BUT why does this work with the sum, not the product????? ####

###############################################  
# Log Likelihood
# General log Likelihood for one observation
# logL(lambda = l | T = t) = log(l) - l*t

# The logL that lambda is 0.5 when we drew a 2 is
# logL(lambda = 0.5 | T = 2) = log(0.5) - 0.5*2
log(0.5) - 0.5*2

# General logL for multiple observations
# logL(lambda = l | T = t) = sum(log(l) - l*t[i]) over i

# For more than one observation, 
#  logL(lambda = 0.5 | T = xx) = 
sum(log(0.5) - 0.5*xx)
sum(dexp(xx, 0.5, log = T))

# Then you look at the logL value at all the possible values of lambda
out <- NA
l <- (1:10000)/1000
for(i in seq_along(l)) {
  out[i] <- sum(log(l[i]) - l[i]*xx)
}
plot(l, out, xlab = "l", ylab = "logL(lambda = l)", xlim = c(0, 3))

# Then you look at the logL value at all the possible values of lambda
out <- NA
l <- (1:10000)/1000
for(i in seq_along(l)) {
  out[i] <- sum(dexp(xx, l[i], log = T))
}
plot(l, out, xlab = "l", ylab = "logL(lambda = l)", xlim = c(0, 3))

