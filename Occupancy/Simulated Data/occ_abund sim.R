# Simulate occupancy to abundance with aerial sampling
# Anna Moeller
# 7/11/2016

# Occupancy in Beaverhead
# Closed plots, with repeated observations

##### Next steps
# Does this N <- rpois(SAsize, lambda*z) work? 
# relationship between psi & lambda
# Think about the divisions for psi-strata (.2, .4, .7)
# Add p<1 for aerial survey
# Open occupancy model? 
# Try simulation with fewer cameras, longer/shorter time, etc. 

#########
# Set working directory
setwd("GitHub/Camera-trap-study/Simulation/Occupancy")

# Load packages
library(dplyr)
library(sampling) # for strata()
library(R2jags)
library(lattice) # xyplot and densityplot
library(coda) # as.mcmc()
library(mcmcplots) # mcmcplot

# Specify things
SAsize <- 100 # Number of grid cells in study area
nsite <- 20 # Number of grid cells sampled with cameras
ncam <- 9
nseas <- 5
alpha.psi <- -1
beta.psi <- 3

alpha.p <- 1
beta.p <- 1

# 1. Make a "map" of abundance and psi for the whole study area
  # N ~ Pois(lambda)
  # log(lambda) = B0 + B1*cov

  # z ~ Bern(psi)
  # logit(psi) = A0 + A1*cov

  # Create some landscape covariates
  set.seed(34)
  X <- runif(n = SAsize, min = -1, max = 1) # Grid cell level
  X2 <- array(runif(n = nsite*ncam, min = -1, max = 1), dim = c(nsite, ncam)) # Camera level
  
  # Create psi
  psi <- plogis(alpha.psi + beta.psi*X)	
  
  # Make lambda vary linearly with psi
  lambda <- psi*100
  
  # Simulate N and z in each grid cell
  # Lambda is 0 if the cell is not occupied 
  set.seed(1)
  z <- rbinom(SAsize, 1, psi) # This is the same as doing it in a loop, I checked
  N <- rpois(SAsize, lambda*z)
  
# 2. Randomly put cameras in some of the grid cells
  set.seed(87)
  sites <- sample(1:SAsize, nsite)

  # Calculate true occupancy, for checking later
  total.z <- sum(z)
  sampled.z <- sum(z[sites])
  
# 3. Give each camera a different detection probability, based on a covariate
  p <- plogis(alpha.p + beta.p*X2) 

# 4. Simulate observation data from the cameras
  y <- array(dim = c(nsite, ncam, nseas))
  for (i in 1:nsite){
    for (j in 1:ncam){
      for (t in 1:nseas){
        y[i, j, t] <- rbinom(1, 1, z[i]*p[i, j])
      }
    }
  }

# 5. Run model to get psi_hat 
  # Bundle data
  win.data <- list(y = y, X = X, X2 = X2, nsite = nsite, ncam = ncam, nseas = nseas)
  
  # Initial values
  zst <- apply(y, 1, max)   # Good inits for latent states essential
  inits <- function(){list(z = zst, 
                           alpha.psi = runif(1, -3, 3), 
                           beta.psi = runif(1, -3, 3), 
                           alpha.p = runif(1, -3, 3), 
                           beta.p = runif(1, -3, 3))}
  
  # Parameters monitored
  params <- c("alpha.psi", "beta.psi", "alpha.p", "beta.p")
  
  # Call JAGS
  out <- jags(win.data, inits, params, "Models/occ_abund.txt", n.chains = 3, n.thin = 1, 
              n.iter = 20000, n.burnin = 1000, working.directory = getwd())

  # Summarize posteriors
  print(out, dig = 2)
  
  # Look at graphs
  out2 <- as.mcmc(out)
  mcmcplot(out2)

# 6. Make a map of psi_hat
  psi_hat <- plogis(out$BUGSoutput$mean$alpha.psi + out$BUGSoutput$mean$beta.psi*X)
  
# 7. Draw strata based on psi_hat
  strat1 <- which(psi_hat >= 0.5)
  strat2 <- which(psi_hat >= 0.2 & psi_hat < 0.5)
  strat3 <- which(psi_hat < 0.2)
  
# 8. Used stratified random sampling to do aerial counts
  tmp <- data.frame(cellnum = c(strat1, strat2, strat3), 
                    stratum = c(rep(1, length(strat1)), 
                                rep(2, length(strat2)),
                                rep(3, length(strat3))))     
  samp_int <- c(7, 5, 5)
  s <- strata(tmp, "stratum", size = c(7, 5, 5), method = "srswor")
  s2 <- getdata(tmp, s)
  
  # Do aerial counts with perfect detection
  mean1 <- mean(N[s2$cellnum[s2$stratum == 1]])
  mean2 <- mean(N[s2$cellnum[s2$stratum == 2]])
  mean3 <- mean(N[s2$cellnum[s2$stratum == 3]])
  
  var1 <- var(N[s2$cellnum[s2$stratum == 1]])
  var2 <- var(N[s2$cellnum[s2$stratum == 2]])
  var3 <- var(N[s2$cellnum[s2$stratum == 3]])

# 9. Estimate abundance across the entire study area based on those counts
  estN <- mean1*length(strat1) + mean2*length(strat2) + mean3*length(strat3)

  # Compare with true N
  estN
  sum(N)
  
# 10. Estimate precision of estN
  var_estN <- length(strat1)*(length(strat1) - samp_int[1])*var1/samp_int[1] +
              length(strat2)*(length(strat2) - samp_int[2])*var2/samp_int[2] +
              length(strat3)*(length(strat3) - samp_int[3])*var3/samp_int[3]
  SD_estN <- sqrt(var_estN)
  