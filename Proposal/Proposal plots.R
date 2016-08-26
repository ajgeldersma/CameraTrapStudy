# Making plots for proposal
# Anna Moeller
# 7/7/2015

#####################
# Poisson/Exponential
#####################
lambda <- 0.03

par(mfrow = c(2,1))

# Which goes along with Poisson, t = 200
hist(rpois(100000, lambda = lambda * 200), prob = T, 
     xlab = "Number of events in time t",
     main = "Histogram of C ~ Pois(mu * t)")

# Exponential Time to Event
hist(rexp(100000, rate = lambda), prob = T, xlab = "Hours to First Event", 
     main = "Histogram of T ~ Exp(mu)")
curve(dexp(x, rate = lambda), add = T, col = "blue")


###################
# Negative Binomial 
###################
# In n experiments of Bernoulli(prob) trials, 
# What is the probability of observing x-axis failures before we get the 'size'th success?
hist(rnbinom(n = 1000, size = 150, prob = .2), prob = T)
