# Yet another occupancy script
# Anna Moeller
# 7/12/2016

# Try building an open (dynamic) occupancy model

#########
# Load packages
library(dplyr)
library(R2jags)
library(lattice) # xyplot and densityplot
library(coda) # as.mcmc()

# Specify things
SAsize <- 100 # Number of grid cells in study area
nplot <- 20 # Number of grid cells sampled with cameras
ncam <- 9
nseas <- 10 # Number of occasions (seasons)
alpha.psi <- -1
beta.psi <- 2

alpha.p <- 1
beta.p <- 1

range.phi = c(0.6, 0.8) # bounds for survival and colonization probability
range.gamma = c(0, 0.1)

# Set up some required arrays
plot <- 1:nplot					    # Plots
season <- 1:nseas				    # Seasons
psi <- array(dim = c(SAsize, nseas))				# Occupancy probability
muZ <- z <- array(dim = c(SAsize, nseas))	# Expected and realized occurrence
y <- array(NA, dim = c(nplot, ncam, nseas))	# Detection histories

# 1. Make a "map" of abundance and psi for the whole study area
# z ~ Bern(psi)
# logit(psi) = A0 + A1*cov

# Create some landscape covariates
set.seed(34)
X <- runif(n = SAsize, -1, 1) # Grid cell level
X2 <- array(runif(n = nplot*ncam, -1, 1), dim = c(nplot, ncam)) # Camera level

# Determine initial occupancy and demographic parameters
psi[, 1] <- plogis(alpha.psi + beta.psi*X)	
phi <- runif(n = nseas-1, min = range.phi[1], max = range.phi[2])
gamma <- runif(n = nseas-1, min = range.gamma[1], max = range.gamma[2])

# Simulate z in each grid cell
  # First season
  set.seed(1)
  z[, 1] <- rbinom(SAsize, 1, psi[, 1])		# Initial occupancy state
  
  # Later seasons
  for(i in 1:SAsize){				# Loop over all grid cells
    for(k in 2:nseas){				# Loop over seasons
      muZ[, k] <- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1] # Prob for occ.
      z[i, k] <- rbinom(1, 1, muZ[, k])
    }
  }

# 2. Randomly put cameras in some of the grid cells
set.seed(87)
sites <- sample(1:SAsize, nplot)

# Calculate true occupancy in each season, for checking later
total.z <- apply(z, 2, sum)
sampled.z <- apply(z[sites, ], 2, sum)

# 3. Give each camera a different detection probability, based on a covariate
p <- plogis(alpha.p + beta.p*X2) 

# 4. Simulate observation data from the cameras
# p is constant across seasons
y <- array(dim = c(nplot, ncam, nseas))
for (i in 1:nplot){
  for (j in 1:ncam){
    for (t in 1:nseas){
      y[i, j, t] <- rbinom(1, 1, z[i, t]*p[i, j])
    }
  }
}

# Compute annual population occupancy
for (k in 2:nseas){
  psi[, k] <- psi[, k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
}

# 5. Run a model to get psi_hat 
# Model for JAGS
load.module("glm")
sink("Dynocc.jags")
cat("model {
    
    # Specify priors
    alpha.psi ~ dunif(-10, 10)
    beta.psi ~ dunif(-10, 10)
    alpha.p ~ dunif(-10, 10)
    beta.p ~ dunif(-10, 10)
    for (k in 1:(nseas - 1)){
      phi[k] ~ dunif(0, 1)
      gamma[k] ~ dunif(0, 1)
    }

    # Ecological submodel: Define state conditional on parameters
    for (i in 1:nplot){
      logit(psi[i, 1]) <- alpha.psi + beta.psi * X[i]
      z[i, 1] ~ dbern(psi[i, 1])             # True occupancy z at site i
      for (k in 2:nseas){
        muZ[i, k]<- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1]
        z[i, k] ~ dbern(muZ[i, k])
      } #k
    } #i
    
    # Observation model
    for (i in 1:nplot){
      for (j in 1:ncam){
        logit(p[i, j]) <- alpha.p + beta.p * X2[i, j]
        for (k in 1:nseas){
          p.eff[i, j, k] <- z[i, k] * p[i, j]
          y[i, j, k] ~ dbern(p.eff[i, j, k])
        } #k
      } #j
    } #i
    
    # Derived parameters: Sample and population occupancy, growth rate and turnover
    for (i in 1:nplot) {
      for (k in 2:nseas){
        psi[i, k] <- psi[i, k-1]*phi[k-1] + (1-psi[i, k-1])*gamma[k-1]
        growthr[i, k-1] <- psi[i, k]/psi[i, k-1]         
        turnover[i, k-1] <- (1 - psi[i, k-1]) * gamma[k-1]/psi[i, k]
      }   
    }
    for (k in 1:nseas){
      n.occ[k] <- sum(z[1:nplot, k])
    }
  }", fill = TRUE)
sink()
  
  # Bundle data
  win.data <- list(y = y, X = X[sites], X2 = X2, nplot = nplot, ncam = ncam, nseas = nseas)
  
  # Initial values
  zst <- apply(y, c(1, 3), max)   # Good inits for latent states essential
  inits <- function(){list(z = zst, 
                           alpha.psi = runif(1, -3, 3), 
                           beta.psi = runif(1, -3, 3), 
                           alpha.p = runif(1, -3, 3), 
                           beta.p = runif(1, -3, 3))}
  
  # Parameters monitored
  params <- c("alpha.psi", "beta.psi", "alpha.p", "beta.p", "n.occ")
  # "turnover", "psi", "phi", "p", "growthr", "gamma"
  
  # Call JAGS from R
  out <- jags(win.data, inits, params, "Dynocc.jags", n.chains = 3,
              n.thin = 4, n.iter = 10000, n.burnin = 3000, working.directory = getwd())
  
  # Summarize posteriors
  print(out, dig = 2)
  
  # Look at graphs
  traceplot(out)
  out2 <- as.mcmc(out)
  densityplot(out2)
