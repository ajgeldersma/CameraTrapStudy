# camera trap simulation
# Paul Lukacs, Jon Horne, Anna Moeller

# Load packages
library(R2jags)
library(coda) # as.mcmc()
library(mcmcplots) # mcmcplot

##################### poisson/exponential distribution simulation ###########
nplot <- 400 					# number of plots sampled
ncam <- 4				      # number of cameras per plot

# True lambda for high and low density 
#   Mean number of animals per cell for high and low density cells
lambda <- c(10, 5)

# Make a random 0/1 covariate for high or low density
#   (randomly assign each row as high or low density) (used later)
lamcov <- rbinom(nplot, 1, .2)

# Calculate your true population size
trueN <- length(which(lamcov == 0))*lambda[1] + length(which(lamcov == 1))*lambda[2]

# Initialize things
pictime <- matrix(NA, nplot, ncam)

# Pull random photo times from either high or low density distribution
#   Each row (plot) has a high or low density
#   Each camera (column) gets a random (exp) timetoevent based on that density
#   Each timetoevent corresponds with some expected number of animals
for(i in 1:nplot){
  pictime[i, ] <- rexp(ncam, lambda[lamcov[i] + 1]) # for whole row, draw rexp from high or low density beta
}

# Specify model in JAGS
sink("model.jags")
cat("model {

  # Priors
  beta0 ~ dnorm(0, 0.1)
  beta1 ~ dnorm(0, 0.1)

  for(i in 1:nplot){
    beta[i] <- exp(beta0 + beta1*lamcov[i])
  }

  for(i in 1:nplot){
    for(j in 1:ncam){
      pictime[i, j] ~ dexp(beta[i]) 
    }
  }

  for(i in 1:nplot){
    x[i] ~ dpois(beta[i])
  }

  N <- sum(x)

  }", fill = TRUE)
sink()

# Gather data
sp.data = list(pictime = pictime,
               lamcov = lamcov,
               nplot = nplot,
               ncam = ncam)

# Specify the parameters to be monitored
sp.params = c("beta0", "beta1", "N")  # jags

# Specify the initial values   omegaGuess = runif(1, n/(n+nzeroes), 1) #
sp.inits = function() {
  list(x = rpois(nplot, 2))
}

# Run the model and call the results after
out <- jags(sp.data, sp.inits, sp.params, "model.jags", 
            n.chains = 1, n.iter = 10000, n.burnin = 2000, n.thin = 1,
            working.directory = getwd())

# Compare the estimated N to trueN
trueN

# Look at diagnostics
out2 <- as.mcmc(out)
mcmcplot(out2)