load.module("glm")

sink("model.jags")
cat("
    model {
    
    # Priors
    #  Occupancy
    alpha.occ ~ dnorm(0, 0.3)T(-5,5)
    for(i in 1:nocc_cov){
      b_occ[i] ~ dnorm(0, 0.3)T(-5,5)
    }
    
    #  Detection
    alpha.p ~ dnorm(0, 0.3)T(-5,5)
    for(i in 1:ndet_cov){
      b_det[i] ~ dnorm(0, 0.3)T(-5,5)
    }
    
    # Likelihood
    for (i in 1:nplot) {
      # True state model for the partially observed true state
      # True occupancy z at site i
      logit(psi[i]) <- alpha.occ + b_occ[1:ncov] %*% XX[i,1:ncov]
      z[i] ~ dbern(psi[i])             
  
      for (j in 1:ncam) {
        # Observation model for the actual observations
        # Detection-nondetection at i and j
        logit(p[i,j]) <- alpha.p + b_det[] %*% X[i,]
        p.eff[i,j] <- z[i] * p[i,j]
        y[i,j] ~ dbern(p.eff[i,j])    
      } #j
    } #i
    
    # Derived quantities
    occ.fs <- sum(z[])       # Number of occupied sites among those studied
    }", fill = TRUE)
sink()
