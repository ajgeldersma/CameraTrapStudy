# Jessie example of nested indexing


## Logistics of sampling
n.sites <- 10
#  Number of visits to each site
n.reps <- 5
#  Number of sp
n.sp <- 4
#  Number of observations
n.obs <- n.sites * n.reps *n.sp

#  Indices for long format
# Generate site info
site <- rep(1:n.sites, each = n.reps*n.sp)

# Generate survey replicate information
reps <- rep(rep(1:n.reps, n.sites), n.sp)

# Generate species information
sp <-rep(1:n.sp, each = n.sites*n.reps)

#  Detection probability by species
p <-c(0.10,0.3,0.5,0.8)

##  Biological Parameters
#  Mean abundance across sites, one for each species 
#  The numbers are meant to be very different so we can see how the model handles them
lambda <- c(20, 150, 300, 1000)

##  Simulation proper
#  Initialize matrices to hold values of abundance corrected for availability
N <- matrix(NA, nrow = n.sites, ncol = n.sp)
# Initialize matrix to hold counts and detection probability (cp)
y <- cp <- matrix(NA, nrow = n.obs, ncol= 1)

#  Abundance corrected for availability during each survey rep
N <- matrix(NA, n.sites, n.sp)
for(i in 1:n.sites){
  N[i,] <- rpois(n.sp, lambda)
}

# Number observed
for(i in 1:n.obs){
  cp[i,] <- c(p[sp[i]])
  y[i,] <- rbinom(1, N[site[i], sp[i]], cp[i])
}

#  Put the data together in long format
input <- data.frame(cbind(y, site, reps, sp, p))
colnames(input) <- c("count","site","reps","sp","p")

###################################JAGS model#############################

#  JAGS model to estimate parameters
sink("model_multisp_nosharing_sim.txt")
cat("
    model{
    #  Priors
    #  Linear predictor on abundance, setup for species variation only,
    #  abundance assumed the same at every site
    for(i in 1:n.sp){
      log.n[i] ~ dnorm(0, 0.001)
      mu.lambda[i] <- exp(log.n[i])
    }
    #  Population size of each species at each site
    for(i in 1:n.sites){
      for(k in 1:n.sp){
        N[i,k] ~ dpois(mu.lambda[k])
      }
    }
    
    #  Observation process
    #  Detection varies by species, no information shared
    for(i in 1:n.sp){
      p[i] ~ dbeta(1, 1)
    }
    for(i in 1:n.obs){
      cp[i,1] <- p[sp[i]]
      y[i,] ~ dbin(p[sp[i]], round(N[site[i],sp[i]]))
    }
  }
    ", fill = T)
sink()
################################################################################
#Set up jags data to run
data <- list("y" = input$count,
             "n.obs" = nrow(input),
             "n.sites" = length(unique(input$site)),
             "site" = input$site,
             "n.sp"=length(unique(input$sp)),
             "sp"=input$sp)
inits <- function(){list(
  log.n = log(lambda),
  p = c(0.10,0.3,0.5,0.8),
  N = N*2 )}
parms <- c("p", "N", "mu.lambda")
out <- jags(data=names(data), 
            inits, parms, 
            "model_multisp_nosharing_sim.txt", 
            3, 10000, 1000, 1)