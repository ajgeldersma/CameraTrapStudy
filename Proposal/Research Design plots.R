# Making plots for proposal
# Anna Moeller
# 7/7/2015


# For previous
hist(rexp(100000, rate = lambda), prob = T, 
     xaxt = "n", xlim = c(1, 200), xlab = "Time to First Elk Detection",
     yaxt = "n",
     main = "H1: Individuals")
curve(dexp(x, rate = lambda), add = T, col = "blue")

###################
# Negative Binomial 
###################
# In n experiments of Bernoulli(prob) trials, 
# What is the probability of observing x-axis failures before we get the 'size'th success?
hist(rnbinom(n = 1000, size = 150, prob = .2), prob = T)

###########################################################################
# For new
lambda <- 0.03
# make some RVs
rv <- rexp(100000, rate = lambda)

par(mfrow = c(2,1))

# Set working directory
setwd("C:/Users/anna.moeller/Documents/School/Research Design")

####################
# Hypothesis 1
jpeg("H1.jpg")
par(mfrow = c(2,1))

# Time to event
plot(dexp(rv) ~ rv, 
     xlim = c(0, 15), xaxt = "n", xlab = "Time to First Elk Detection",
     yaxt = "n", ylab = "Probability Density",
     main = "H1: Individuals")

# Group Size
plot(rep(50, 100) ~ seq(1, 100), type = "l", xaxt = "n", yaxt = "n",
     xlab = "Abundance", ylab = "Group Size")

dev.off()
################
# Hypothesis 2
jpeg("H2.jpg")
par(mfrow = c(2,1))

# Time to event
plot(dexp(rv) ~ rv, 
     xlim = c(0, 15), xaxt = "n", xlab = "Time to First Group Detection",
     yaxt = "n", ylab = "Probability Density",
     main = "H2: More Groups")

# Group Size
plot(rep(50, 100) ~ seq(1, 100), type = "l", xaxt = "n", yaxt = "n",
     xlab = "Abundance", ylab = "Group Size")

dev.off()
##################3
# Hypothesis 3
jpeg("H3.jpg")
par(mfrow = c(2,1))

# Time to event
plot(rep(50, 100) ~ seq(1, 100), type = "l", 
     xaxt = "n", xlab = "Time to First Group Detection", 
     yaxt = "n", ylab = "Probability Density",
     main = "H3: Larger Groups")

# Group Size
plot(seq(1, 100) ~ seq(1, 100), type = "l", xaxt = "n", yaxt = "n",
     xlab = "Abundance", ylab = "Group Size")

dev.off()
#################
# Hypothesis 4
jpeg("H4.jpg")
par(mfrow = c(2,1))

# Time to event
plot(rep(50, 100) ~ seq(1, 100), type = "l", 
     xaxt = "n", xlab = "Time to First Elk Detection", 
     yaxt = "n", ylab = "Probability Density",
     main = "H4: No Single Strategy")

# Group Size
plot(rep(50, 100) ~ seq(1, 100), type = "l", xaxt = "n", yaxt = "n",
     xlab = "Abundance", ylab = "Group Size")

dev.off()

########################################
# Summary graph
par(mfrow = c(1,1))

jpeg("Variance.jpg")

# How close does the variance have to be to what it's supposed to be? 
plot(c(rep(10, 5),11) ~ c(seq(1, 5), 6), type = "l", 
     xlab = " Variance", xaxt = "n", 
     ylab = "Abundance Estimate", yaxt = "n")
dev.off()
