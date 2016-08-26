# Simulating occupancy
# Anna Moeller
# 7/8/2016

# Simulate occupancy observations with:
#   Closure in each site
#   Constant detection probability across time at a camera
#   Different detection probability at each camera

# The following code is from http://www.r-bloggers.com/multi-species-dynamic-occupancy-model-with-r-and-jags/
#   It is for simulating occupancy observations that:
#   change from year to year (Markov) (can get rid of this, or put back in later)
#   For different species (can get rid of this)
#     Detection probability varies across species but not time or space (can change this) 

# These are my changes.
# This is just for the Beaverhead
#   What I want to do is get rid of the different species,
#   just make some some common occupancy probability across all sites
#   draw occupancy states. These should be constant across cameras and time
#   Make different detection probabilities for cameras
#   Lose nspec, nsite = 9, nrep = cameras, nyear = different weeks
#   With closure, occupancy does not have to change across weeks, but we will start with this code

# Define logit and logistic fns for ease later
logit <- function(x) {
  log(x/(1 - x))
}

antilogit <- function(x) {
  exp(x)/(1 + exp(x))
}

# Define survey occasions
nsite <- 10
nocc <- 3 # Surveys across time
ncam <- 3 # Repeated observations at one time in one site

# Initial occupancy states in year 0
rho0 <- 0.7 ## Occ prob (site survival) over the whole area in year 0
z0 <- rbinom(nsite, 1, rho0) ## True occupancy at each site in year 0

gam <- .1 # colonization probability


# Subsequent occupancy, calculated from occupancy in year 0
z <- array(dim = c(nsite, nocc)) # True site occupancy
lpsi <- array(dim = c(nsite, nocc)) # Logit(site occupancy probability) # Not really necessary step
psi <- array(dim = c(nsite, nocc)) # Site occupancy probability
for (j in 1:nsite) {
  for (t in 1:nocc) {
    if (t == 1) {
      psi[j, t] <- antilogit(gam * z0[j])
    } else {
      psi[j, t] <- antilogit(gam * z[j, t - 1])
    }
    z[j, t] <- rbinom(1, 1, psi[j, t])
  }
}

# Define detection probability
p_p <- 0.7
mup <- logit(p_p)
sdp <- 1.5
set.seed(222)
lp <- rnorm(nspec, mup, sdp) # p is different for different species, but is drawn from some common distribution
p <- antilogit(lp)

# Simulate observation data (seen yes/no with probability p*z)
x <- array(dim = c(nsite, nspec, nyear, nrep))
for (j in 1:nsite) {
  for (i in 1:nspec) {
    for (t in 1:nyear) {
      for (k in 1:nrep) {
        x[j, i, t, k] <- rbinom(1, 1, p[i] * z[j, i, t])
      }
    }
  }
}


##########################################################################################
# The following code is from http://www.r-bloggers.com/multi-species-dynamic-occupancy-model-with-r-and-jags/
#   It is for simulating occupancy observations that:
#   change from year to year (Markov) (can get rid of this, or put back in later)
#   For different species (can get rid of this)
#     Detection probability varies across species but not time or space (can change this) 
#   

# Define these for ease later
logit <- function(x) {
  log(x/(1 - x))
}

antilogit <- function(x) {
  exp(x)/(1 + exp(x))
}

# Define survey occasions
nsite <- 20
nspec <- 2
nyear <- 3
nrep <- 3 # Surveys within a year, occupancy is constant

# Community level hyperparameters
# Beta is the site colonization probability (
p_beta = 0.7
mubeta <- logit(p_beta)
sdbeta <- 2

# Rho is the site survival probability (exists this year|existed last year)
#   In year 0, this is just the occupancy probability
p_rho <- 0.8 
murho <- logit(p_rho)
sdrho <- 1

# Species-specific random effects
# Each species has occupancy probability that is based on
#   a baseline occ prob (beta) + site colonization prob (rho) 
set.seed(1)  # for reproducibility
beta <- rnorm(nspec, mubeta, sdbeta) # Baseline occupancy probability
set.seed(1008)
rho <- rnorm(nspec, murho, sdrho) # colonization probability

# initial occupancy states in year 0
set.seed(237)
rho0 <- runif(nspec, 0, 1) ## Occ prob (site survival) over the whole area in year 0
z0 <- array(dim = c(nsite, nspec))
for (i in 1:nspec) {
  z0[, i] <- rbinom(nsite, 1, rho0[i]) ## True occupancy for each species at each site in year 0
}

# Subsequent occupancy, calculated from occupancy in year 0
z <- array(dim = c(nsite, nspec, nyear)) # True species occupancy
lpsi <- array(dim = c(nsite, nspec, nyear)) # Logit(species occupancy probability) # Not really necessary step
psi <- array(dim = c(nsite, nspec, nyear)) # Species occupancy probability
for (j in 1:nsite) {
  for (i in 1:nspec) {
    for (t in 1:nyear) {
      if (t == 1) {
        lpsi[j, i, t] <- beta[i] + rho[i] * z0[j, i]
        psi[j, i, t] <- antilogit(lpsi[j, i, t])
        z[j, i, t] <- rbinom(1, 1, psi[j, i, t])
      } else {
        lpsi[j, i, t] <- beta[i] + rho[i] * z[j, i, t - 1]
        psi[j, i, t] <- antilogit(lpsi[j, i, t])
        z[j, i, t] <- rbinom(1, 1, psi[j, i, t])
      }
    }
  }
}

# Define detection probability
p_p <- 0.7
mup <- logit(p_p)
sdp <- 1.5
set.seed(222)
lp <- rnorm(nspec, mup, sdp) # p is different for different species, but is drawn from some common distribution
p <- antilogit(lp)

# Simulate observation data (seen yes/no with probability p*z)
x <- array(dim = c(nsite, nspec, nyear, nrep))
for (j in 1:nsite) {
  for (i in 1:nspec) {
    for (t in 1:nyear) {
      for (k in 1:nrep) {
        x[j, i, t, k] <- rbinom(1, 1, p[i] * z[j, i, t])
      }
    }
  }
}