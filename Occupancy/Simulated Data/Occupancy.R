# Occupancy
# Anna Moeller
# 6/13/2016


### At the end play with 
par(mar = c(3,2,1,2))
plot(out2)
        
rmat <- as.matrix(out2)
head(rmat)

plot(density(rmat[, "psi"]))
rug(rmat[, "psi"])
#curve(dgamma(x, 634.001, 26.001), from = 20, to = 30, lwd = 2, col = "red", add = T)



# Load packages for RMark
library(RMark)
# Load packages for 13.3.1
library(lattice) # xyplot and densitylplot
library(coda) # as.mcmc()
library(R2jags)

# Set working directory
setwd("C:/Users/anna.moeller/Documents/Github/Camera-trap-study/Models/Occupancy")

# Bring in the encounter history I made
source("Occupancy encounter history.R")
# This creates an object called occ

##################################################################
# Occupancy model in MARK

# Keep a list of the plots because I'm about to delete them
plot <- occ$plot

# Add and subtract things to the encounter history
elk <- mutate(occ, 
              site = as.factor(ifelse(grepl("BH", plot), "Beaverhead", "St. Joe"))) %>%
  select(-plot)
# MARK doesn't like dplyr tbls
elk <- as.data.frame(elk)

# Step 1. Make processed data
elk.proc = process.data(elk, model = "Occupancy", groups = "site")

# Step 2. Make design data
elk.ddl = make.design.data(elk.proc)

# Step 3. Write a function to run models
elk.analysis <- function() {
  
  # Define Psi models
  Psi.dot <- list(formula = ~1)
  Psi.site <- list(formula = ~site)
  
  # Define p models
  p.dot <- list(formula = ~1)
  p.site <- list(formula = ~site)
  
  # Create model list
  cml <- create.model.list("Occupancy")
  
  # Run and return marklist of models
  mark.wrapper(cml, data = elk.proc, ddl = elk.ddl, output = F)
}

# Step 4. Run models and look at stuff
elk.results = elk.analysis()
elk.results
summary(elk.results[[3]])
# elk.results[[3]] opens a MARK output file
elk.results[[3]]$design.matrix

# Don't have to use model.matrix but here is what it does
model.matrix(~site, elk)

##################################################################
# Bayesian from Kery and Schaub
# 13.3.1. The simplest possible site-occupancy model

# Load packages
library(lattice) # xyplot and densitylplot
library(coda) # as.mcmc()
library(R2jags)

# Set working directory
setwd("C:/Users/anna.moeller/Documents/Github/Camera-trap-study/Models/Occupancy")

# Bring in occ from "Occupancy encounter history.R"
y <- as.matrix(occ[, 2:10])

# Bundle data
win.data <- list(y = y, plots = nrow(y), cams = ncol(y))

# Specify model in BUGS language
sink("model.jags")
cat("
    model {
      
      # Priors
      psi ~ dunif(0, 1)
      p ~ dunif(0, 1)
      
      # Likelihood
      # Ecological model for true occurrence
      for (i in 1:plots) {
        z[i] ~ dbern(psi)
        p.eff[i] <- z[i] * p
        
        # Observation model for replicated detection/nondetection observations
        for (j in 1:cams) {
          y[i,j] ~ dbern(p.eff[i])
        } #j
      } #i
      
      # Derived quantities
      occ.fs <- sum(z[])       # Number of occupied plots out of the 18 plots
    }", fill = TRUE)
sink()

# Initial values
zst <- apply(y, 1, function(x){
  ifelse(any(!is.na(x)), max(x, na.rm = T), NA)
  })		# Observed occurrence as starting values for z
inits <- function() list(z = zst)

# Parameters monitored
params <- c("psi", "p", "occ.fs")

# Call JAGS from R (BRT < 1 min)
out <- jags(win.data, inits, params, "model.jags", n.chains = 3, n.thin = 2,
            n.iter = 1200, n.burnin = 200, working.directory = getwd())

# Summarize posteriors
print(out, dig = 2)

# Look at graphs
# plot(out) # I have no idea what this is
# traceplot(out)
out2 <- as.mcmc(out)
xyplot(out2)
densityplot(out2)

############################################################
# Site-occupancy model for constant p across cameras in a site 
# Basically exactly the same as example 13.3.2
# Take info to run it from example 13.3.2

# Build data
y <- as.matrix(occ[, 2:10])
X <- # some covariate value of length 9 (for p)
XX <- # some covariate value of length 9 (for psi)

# Specify model in BUGS language
sink("model.jags")
cat("
    model {
    
    # Priors
    alpha.occ ~ dunif(-10, 10)
    beta.occ ~ dunif(-10, 10)
    alpha.p ~ dunif(-10, 10)
    beta.p ~ dunif(-10, 10)
    
    # Likelihood
    for (i in 1:nplot) {
      # True state model for the partially observed true state
      # True occupancy z at site i
      z[i] ~ dbern(psi[i])             
      logit(psi[i]) <- alpha.occ + beta.occ * XX[i]
      
      for (j in 1:ncam) {
        # Observation model for the actual observations
        # Detection-nondetection at i and j
        y[i,j] ~ dbern(p.eff[i,j])    
        p.eff[i,j] <- z[i] * p[i,j]
        logit(p[i,j]) <- alpha.p + beta.p * X[i]
      } #j
    } #i
    
    # Derived quantities
    occ.fs <- sum(z[])       # Number of occupied sites among those studied
    }
    ", fill = TRUE)
sink()

# Bundle data
win.data <- list(y = y, X = sodata$X, nplot = nrow(y), ncam = ncol(y))

####################################################################
# Site-occupancy model for different p across cameras in a site 

# Build data
y <- as.matrix(occ[, 2:10])
X <- # some covariate value in a 9*9 matrix (for p)
XX <- # some covariate value of length 9 (for psi)
  
# Specify model in BUGS language
sink("model.jags")
cat("
    model {
    
    # Priors
    alpha.occ ~ dunif(-10, 10)
    beta.occ ~ dunif(-10, 10)
    alpha.p ~ dunif(-10, 10)
    beta.p ~ dunif(-10, 10)
    
    # Likelihood
    for (i in 1:nplot) {
      # True state model for the partially observed true state
      # True occupancy z at site i
      z[i] ~ dbern(psi[i])             
      logit(psi[i]) <- alpha.occ + beta.occ * XX[i]
      
      for (j in 1:ncam) {
        # Observation model for the actual observations
        # Detection-nondetection at i and j
        y[i,j] ~ dbern(p.eff[i,j])    
        p.eff[i,j] <- z[i] * p[i,j]
        logit(p[i,j]) <- alpha.p + beta.p * X[i,j]
      } #j
    } #i
    
    # Derived quantities
    occ.fs <- sum(z[])       # Number of occupied sites among those studied
    }
    ", fill = TRUE)
sink()

# Bundle data
win.data <- list(y = y, X = sodata$X, nplot = nrow(y), ncam = ncol(y))

########################################################################### 
# 13.5. Dynamic (multi-season) site-occupancy models
  # Annual variation in probabilities of patch survival, colonization and 
  # detection is specified by the bounds of a uniform distribution.

  # R - Number of sites
  # J - Number of replicate surveys
  # K - Number of years
  # psi1 - occupancy probability in first year
  # range.p - bounds of uniform distribution from which annual p drawn 
  # range.psi and range.gamma - same for survival and colonization probability

# Specify model in BUGS language
sink("Dynocc.jags")
cat("model {
    
    # Specify priors
    psi1 ~ dunif(0, 1)
    for (k in 1:(nyear-1)){
      phi[k] ~ dunif(0, 1)
      gamma[k] ~ dunif(0, 1)
      p[k] ~ dunif(0, 1) 
    }
    p[nyear] ~ dunif(0, 1)
    
    # Ecological submodel: Define state conditional on parameters
    for (i in 1:nsite){
      z[i,1] ~ dbern(psi1)
      for (k in 2:nyear){
        muZ[i,k]<- z[i,k-1]*phi[k-1] + (1-z[i,k-1])*gamma[k-1]
        z[i,k] ~ dbern(muZ[i,k])
      } #k
    } #i
    
    # Observation model
    for (i in 1:nsite){
      for (j in 1:nrep){
        for (k in 1:nyear){
          muy[i,j,k] <- z[i,k]*p[k]
          y[i,j,k] ~ dbern(muy[i,j,k])
        } #k
      } #j
    } #i
    
    # Derived parameters: Sample and population occupancy, growth rate and turnover
    psi[1] <- psi1
    n.occ[1]<- sum(z[1:nsite,1])
    for (k in 2:nyear){
      psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
      n.occ[k] <- sum(z[1:nsite,k])
      growthr[k-1] <- psi[k]/psi[k-1]                         # originally we had growthr[k]. JAGS seem to dislike vectoring going from 2..K.
      turnover[k-1] <- (1 - psi[k-1]) * gamma[k-1]/psi[k]
      }
  }", fill = TRUE)
sink()

################################################################################
# Test something out
elk <- mutate(occ, 
              site = ifelse(grepl("BH", plot), "Beaverhead", "St. Joe"),
              test = factor(c(rep(1,6), rep(2,6), rep(3,6)))) ### factor is necessary!!
# Look at the different parameterizations
model.matrix(~test + site, elk) 
model.matrix(~test-1 + site, elk)