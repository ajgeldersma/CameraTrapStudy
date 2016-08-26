# Load packages
library(circular)

# Function...
walk.fn <- function(max.steps = max.steps, cam.min, cam.max){
  # make weibull distributed steps
  steps <- rweibull(max.steps, 2, 1)

  # make clustered turning angles
  theta <- rwrappedcauchy(max.steps, mu = 0, rho = .8)

  # cumulative angle (absolute orientation)
  phi <- cumsum(theta)

  # step length components
  dX <- steps*cos(phi)
  dY <- steps*sin(phi)

  # actual X-Y values
  X <- cumsum(dX)
  Y <- cumsum(dY)
  
  # plot that puppy
  #plot(X, Y, type="l")
  
  # When does it first hit camera?
  xcam <- which(round(X) >= cam.min & round(X) <= cam.max)
  ycam <- which(round(Y) >= cam.min & round(Y) <= cam.max)
  xcam[min(which(xcam %in% ycam))]
}

### Okay, this simulates a population of 1 animal. Now I need it to simulate
# a population of X ~ pois(10) animals (10 is a random number I chose)
# and spit out the first time when any of the animals falls in the hole
start.pop <- rpois(1000, 10)
# I need to create a path ___ times (start.pop[1], start.pop[2]...) and pull out the min time it hits the camera
# and then pull out the min of all of those. 





# Call function
max.steps <- 1000 # length of walk
reps <- 1:1000 # number of reps
steps <- sapply(reps, walk.fn, cam.min = 5, cam.max = 10)

# Plot
freq <- hist(steps, prob = T, breaks = 20)
hist(steps, prob = T)

# Get lambda
avgtime <- mean(freq$counts) # average time to event, = 1/lambda
lambda <- 1/avgtime # average number of events per unit time

# Plot exponential curve
x <- 1:max(freq$breaks) - 1
y <- dexp(x, lambda)
lines(y~x, col = "red")

# calculate the expected number of events in an hour (let's say 1 step = 1 hr)
lambda.1 <- lambda
# calc num events in a day
lambda.24 <- lambda *24
# calc num events in 3 days
lambda.72 <- lambda * 72

# Let's say our sampling period is 3 days. What counts should we see? 
p <- rpois(1000, lambda.72)
hist(p)

# Throw a Poisson line on there # yeah this doesn't work...
a <- 0:10
b <- dpois(a, lambda.72)
lines(b~a, col = "red")