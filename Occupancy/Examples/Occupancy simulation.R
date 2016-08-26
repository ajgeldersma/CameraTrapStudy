# Occupancy simulation
# Anna Moeller
# 6/21/2016

# Load packages
library(R2jags)

# 13.5. Dynamic (multi-season) site-occupancy models
# 13.5.1. Generation and analysis of simulated data
data.fn <- function(R = 9, J = 9, K = 10, psi1 = 0.4, range.p = c(0.2, 0.4),
                    range.phi = c(0.6, 0.8), range.gamma = c(0, 0.1)) {
  # Function to simulate detection/nondetection data for dynamic site-occ model
  # Annual variation in probabilities of patch survival, colonization and 
  # detection is specified by the bounds of a uniform distribution.
  
  # Function arguments:
  # R - Number of plots
  # J - Number of replicate cameras
  # K - Number of seasons
  # psi1 - occupancy probability in first season
  # range.p - bounds of uniform distribution from which annual p drawn 
  # range.psi and range.gamma - same for survival and colonization probability
  
  # Set up some required arrays
  plot <- 1:R					    # Plots
  season <- 1:K				    # seasons
  psi <- rep(NA, K)				# Occupancy probability
  muZ <- z <- array(dim = c(R, K))	# Expected and realized occurrence
  y <- array(NA, dim = c(R, J, K))	# Detection histories
  
  # Determine initial occupancy and demographic parameters
  psi[1] <- psi1				# Initial occupancy probability
  p <- runif(n = K, min = range.p[1], max = range.p[2])
  phi <- runif(n = K-1, min = range.phi[1], max = range.phi[2])
  gamma <- runif(n = K-1, min = range.gamma[1], max = range.gamma[2])
  
  # Generate latent states of occurrence
  # First season
  z[,1] <- rbinom(R, 1, psi[1])		# Initial occupancy state
  # Later seasons
  for(i in 1:R){				  # Loop over plots
    for(k in 2:K){				# Loop over seasons
      muZ[k] <- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1] # Prob for occ.
      z[i,k] <- rbinom(1, 1, muZ[k])
    }
  }
  
  # Plot realised occupancy
  plot(season, apply(z, 2, mean), type = "l", xlab = "Season", 
       ylab = "Occupancy or Detection prob.", col = "red", xlim = c(0,K+1), 
       ylim = c(0,1), lwd = 2, lty = 1, frame.plot = FALSE, las = 1)
  lines(season, p , type = "l", col = "red", lwd = 2, lty = 2)
  
  # Generate detection/nondetection data
  for(i in 1:R){
    for(k in 1:K){
      prob <- z[i,k] * p[k]
      for(j in 1:J){
        y[i,j,k] <- rbinom(1, 1, prob)
      }
    }
  }
  
  # Compute annual population occupancy
  for (k in 2:K){
    psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
  }
  
  # Plot apparent occupancy
  psi.app <- apply(apply(y, c(1,3), max), 2, mean)
  lines(season, psi.app, type = "l", col = "black", lwd = 2)
  text(0.85*K, 0.06, labels = "red solid - true occupancy\n red dashed - 
       detection\n black - observed occupancy")
  
  # Return data
  return(list(R = R, J = J, K = K, psi = psi, psi.app = psi.app, z = z, 
              phi = phi, gamma = gamma, p = p, y = y))
}

data <- data.fn(R = 9, J = 9, K = 10, psi1 = 0.6, range.p = c(0.1, 0.9), 
                range.phi = c(0.7, 0.9), range.gamma = c(0.1, 0.5))

# Specify model in BUGS language
sink("Dynocc.jags")
cat("model {
      
      # Specify priors
      psi1 ~ dunif(0, 1)
      for (k in 1:(nseas-1)){
        phi[k] ~ dunif(0, 1)
        gamma[k] ~ dunif(0, 1)
        p[k] ~ dunif(0, 1) 
      }
      p[nseas] ~ dunif(0, 1)
      
      # Ecological submodel: Define state conditional on parameters
      for (i in 1:nplot){
        z[i,1] ~ dbern(psi1)
        for (k in 2:nseas){
          muZ[i,k]<- z[i,k-1]*phi[k-1] + (1-z[i,k-1])*gamma[k-1]
          z[i,k] ~ dbern(muZ[i,k])
        } #k
      } #i
      
      # Observation model
      for (i in 1:nplot){
        for (j in 1:ncam){
          for (k in 1:nseas){
            muy[i,j,k] <- z[i,k]*p[k]
            y[i,j,k] ~ dbern(muy[i,j,k])
          } #k
        } #j
      } #i
      
      # Derived parameters: Sample and population occupancy, growth rate and turnover
      psi[1] <- psi1
      n.occ[1]<-sum(z[1:nplot,1])
      for (k in 2:nseas){
        psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
        n.occ[k] <- sum(z[1:nplot,k])
        growthr[k-1] <- psi[k]/psi[k-1]                         # originally we had growthr[k]. JAGS seem to dislike vectoring going from 2..K.
        turnover[k-1] <- (1 - psi[k-1]) * gamma[k-1]/psi[k]
      }
    }", fill = TRUE)
sink()

# Bundle data
win.data <- list(y = data$y, nplot = dim(data$y)[1], ncam = dim(data$y)[2], 
                 nseas = dim(data$y)[3])

# Initial values
zst <- apply(data$y, c(1, 3), max)	# Observed occurrence as inits for z
inits <- function(){ list(z = zst)}

# Parameters monitored
params <- c("psi", "phi", "gamma", "p", "n.occ", "growthr", "turnover") 

# Call JAGS from R (BRT 3 min)
out <- jags(win.data, inits, params, "Dynocc.jags", n.chains = 3,
            n.thin = 4, n.iter = 2500, n.burnin = 500, working.directory = getwd())

# Summarize posteriors
print(out, dig = 2)
psiall <- paste("psi[", 1:data$K, "]", sep = "")
print(cbind(data$psi, out$BUGSoutput$summary[psiall, c(1, 2, 3, 7)]), dig = 3)
phiall <- paste("phi[", 1:(data$K-1), "]", sep = "")
print(cbind(data$phi, out$BUGSoutput$summary[phiall, c(1, 2, 3, 7)]), dig = 3)
gammaall <- paste("gamma[", 1:(data$K-1), "]", sep="")
print(cbind(data$gamma, out$BUGSoutput$summary[gammaall, c(1, 2, 3, 7)]), dig = 3)
pall <- paste("p[", 1:data$K, "]", sep = "")
print(cbind(data$p, out$BUGSoutput$summary[pall, c(1, 2, 3, 7)]), dig = 3)

plot(1:data$K, data$psi, type = "l", xlab = "Year", ylab = "Occupancy probability", 
     col = "red", xlim = c(0,data$K+1), ylim = c(0,1), lwd = 2, lty = 1,
     frame.plot = FALSE, las = 1)
lines(1:data$K, data$psi.app, type = "l", col = "black", lwd = 2)
points(1:data$K, out$BUGSoutput$mean$psi, type = "l", col = "blue", lwd = 2)
segments(1:data$K, out$BUGSoutput$summary[psiall,3], 1:data$K, 
         out$BUGSoutput$summary[psiall,7], col = "blue", lwd = 1)

###################################################################################
# Simulating dynamic occupancy with p that is constant across time but 
#   different for each camera, based on a linear model

data.fn <- function(R = 9, J = 9, K = 10, psi1 = 0.4, alpha.p = 1, beta.p = -3,
                    range.phi = c(0.6, 0.8), range.gamma = c(0, 0.1)) {
  # Function to simulate detection/nondetection data for dynamic site-occ model
  # Annual variation in probabilities of patch survival, colonization and 
  # detection is specified by the bounds of a uniform distribution.
  
  # Function arguments:
  # R - Number of plots
  # J - Number of replicate cameras
  # K - Number of seasons
  # psi1 - occupancy probability in first season
  # range.p - bounds of uniform distribution from which annual p drawn 
  # range.psi and range.gamma - same for survival and colonization probability
  
  # Set up some required arrays
  plot <- 1:R					    # Plots
  season <- 1:K				    # seasons
  psi <- rep(NA, K)				# Occupancy probability
  muZ <- z <- array(dim = c(R, K))	# Expected and realized occurrence
  y <- array(NA, dim = c(R, J, K))	# Detection histories
  
  # Make up covariate values for p
  X <- array(runif(n = R*J, min = -1, max = 1), dim = c(R, J))
  
  # Determine initial occupancy and demographic parameters
  psi[1] <- psi1				# Initial occupancy probability
  p <- plogis(alpha.p + beta.p*X)
  phi <- runif(n = K-1, min = range.phi[1], max = range.phi[2])
  gamma <- runif(n = K-1, min = range.gamma[1], max = range.gamma[2])
  
  # Generate latent states of occurrence
  # First season
  z[,1] <- rbinom(R, 1, psi[1])		# Initial occupancy state
  # Later seasons
  for(i in 1:R){				  # Loop over plots
    for(k in 2:K){				# Loop over seasons
      muZ[k] <- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1] # Prob for occ.
      z[i,k] <- rbinom(1, 1, muZ[k])
    }
  }
  
  # # Plot realised occupancy
  # plot(season, apply(z, 2, mean), type = "l", xlab = "Season", 
  #      ylab = "Occupancy or Detection prob.", col = "red", xlim = c(0,K+1), 
  #      ylim = c(0,1), lwd = 2, lty = 1, frame.plot = FALSE, las = 1)
  # lines(season, p , type = "l", col = "red", lwd = 2, lty = 2)
  # 
  # Generate detection/nondetection data
  for(i in 1:R){
    for(k in 1:K){
      for(j in 1:J){
        prob <- z[i,k] * p[i,j]
        y[i,j,k] <- rbinom(1, 1, prob)
      }
    }
  }
  
  # Compute annual population occupancy
  for (k in 2:K){
    psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
  }
  
   # Plot apparent occupancy
   psi.app <- apply(apply(y, c(1,3), max), 2, mean)
  # lines(season, psi.app, type = "l", col = "black", lwd = 2)
  # text(0.85*K, 0.06, labels = "red solid - true occupancy\n red dashed - 
  #      detection\n black - observed occupancy")
  
  # Return data
  return(list(R = R, J = J, K = K, psi = psi, psi.app = psi.app, z = z, 
              phi = phi, gamma = gamma, p = p, y = y))
}

data <- data.fn(R = 9, J = 9, K = 3, psi1 = 0.6, alpha.p = 1, beta.p = -3, 
                range.phi = c(0.7, 0.9), range.gamma = c(0.1, 0.5))

# Specify model in BUGS language
sink("Dynocc.jags")
cat("model {
    
    # Specify priors
    psi1 ~ dunif(0, 1)
    for (k in 1:(nseas-1)){
      phi[k] ~ dunif(0, 1)
      gamma[k] ~ dunif(0, 1)
    }
    for (i in 1:nplot) {
      for (j in 1:ncam) { 
        p[i,j] ~ dunif(0, 1)
      }
    }

    # Ecological submodel: Define state conditional on parameters
    for (i in 1:nplot){
      z[i,1] ~ dbern(psi1)
      for (k in 2:nseas){
        muZ[i,k]<- z[i,k-1]*phi[k-1] + (1-z[i,k-1])*gamma[k-1]
        z[i,k] ~ dbern(muZ[i,k])
      } #k
    } #i
    
    # Observation model
    for (i in 1:nplot){
      for (j in 1:ncam){
        for (k in 1:nseas){
          muy[i,j,k] <- z[i,k]*p[i,j]
          y[i,j,k] ~ dbern(muy[i,j,k])
        } #k
      } #j
    } #i
    
    # Derived parameters: Sample and population occupancy, growth rate and turnover
    psi[1] <- psi1
    n.occ[1]<-sum(z[1:nplot,1])
    for (k in 2:nseas){
      psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
      n.occ[k] <- sum(z[1:nplot,k])
      growthr[k-1] <- psi[k]/psi[k-1]                         # originally we had growthr[k]. JAGS seem to dislike vectoring going from 2..K.
      turnover[k-1] <- (1 - psi[k-1]) * gamma[k-1]/psi[k]
    }
  }", fill = TRUE)
sink()


# Bundle data
win.data <- list(y = data$y, nplot = dim(data$y)[1], ncam = dim(data$y)[2], 
                 nseas = dim(data$y)[3])

# Initial values
zst <- apply(data$y, c(1, 3), max)	# Observed occurrence as inits for z
inits <- function(){ list(z = zst)}

# Parameters monitored
params <- c("psi", "phi", "gamma", "p", "n.occ", "growthr", "turnover") 

# Call JAGS from R (BRT 3 min)
out <- jags(win.data, inits, params, "Dynocc.jags", n.chains = 3,
            n.thin = 4, n.iter = 2500, n.burnin = 500, working.directory = getwd())

# Summarize posteriors
print(out, dig = 2)
psiall <- paste("psi[", 1:data$K, "]", sep = "")
print(cbind(data$psi, out$BUGSoutput$summary[psiall, c(1, 2, 3, 7)]), dig = 3)
phiall <- paste("phi[", 1:(data$K-1), "]", sep = "")
print(cbind(data$phi, out$BUGSoutput$summary[phiall, c(1, 2, 3, 7)]), dig = 3)
gammaall <- paste("gamma[", 1:(data$K-1), "]", sep="")
print(cbind(data$gamma, out$BUGSoutput$summary[gammaall, c(1, 2, 3, 7)]), dig = 3)
pall <- paste("p[", 1:data$K, "]", sep = "")
print(cbind(data$p, out$BUGSoutput$summary[pall, c(1, 2, 3, 7)]), dig = 3)

plot(1:data$K, data$psi, type = "l", xlab = "Year", ylab = "Occupancy probability", 
     col = "red", xlim = c(0,data$K+1), ylim = c(0,1), lwd = 2, lty = 1,
     frame.plot = FALSE, las = 1)
lines(1:data$K, data$psi.app, type = "l", col = "black", lwd = 2)
points(1:data$K, out$BUGSoutput$mean$psi, type = "l", col = "blue", lwd = 2)
segments(1:data$K, out$BUGSoutput$summary[psiall,3], 1:data$K, 
         out$BUGSoutput$summary[psiall,7], col = "blue", lwd = 1)
