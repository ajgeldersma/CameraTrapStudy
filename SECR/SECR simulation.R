# SECR simulation from Royle et al. SECR p. 482
# Anna Moeller
# 7/14/2016

# Load packages
library(R2jags)
library(lattice) # xyplot and densityplot
library(coda) # as.mcmc()
library(mcmcplots) # mcmcplot

# Create coordinates for 100 traps
tr <- seq(15, 85, length = 10)
X <- cbind(rep(tr, each = length(tr)), rep(tr, times = length(tr)))

# Create S, a 100x100 square
set.seed(10)
xlim <- c(0, 100)
ylim <- c(0, 100)
A <- (xlim[2]-xlim[1])*(ylim[2]-ylim[1])/1e4

# Set density to mu, create N as a random variable from density
mu <- 50                 # population density
N <- rpois(1, mu*A)

# Create N activity centers
s <- cbind(runif(N, xlim[1], xlim[2]), runif(N, ylim[1], ylim[2]))

# Set other parameters
sigma <- 5                # Gaussian scale parameter
lam0 <- 0.4               # Baseline encounter rate
J <- nrow(X)              # Number of traps
K <- 5                    # number of repeat observations

# Generate latent encounter data for each animal at each trap on each occasion
y <- array(NA, c(N, J, K))
for(j in 1:J) {
  dist <- sqrt((X[j, 1] - s[, 1])^2 + (X[j, 2] - s[, 2])^2) # distance between each trap and animal
  lambda <- lam0*exp(-dist^2/(2*sigma^2))   # half-normal detection function
  for (k in 1:K) {
    y[, j, k] <- rpois(N, lambda)
  }
}

# Because this is unmarked, we can't observe y. 
#   So create n (count data at each trap on each occasion)
n <- apply(y, c(2, 3), sum)
dimnames(n) <- list(paste0("trap", 1:J), paste0("night", 1:K))

# This model takes forever
# Write the model for JAGS
load.module("glm")
sink("model.jags")
cat("
model {
  sigma ~ dunif(0, 200)
  lam0 ~ dunif(0, 5)
  psi ~ dbeta(1, 1)
  for (i in 1:M){
    z[i] ~ dbern(psi)
    s[i, 1] ~ dunif(xlim[1], xlim[2])
    s[i, 2] ~ dunif(ylim[1], ylim[2])
    for(j in 1:J){
      distsq[i, j] <- (s[i, 1] - X[j, 1])^2 + (s[i, 2] - X[j, 2])^2
      lam[i, j] <- lam0*exp(-distsq[i, j]/(2*sigma^2))
      for (k in 1:K){
        y[i, j, k] ~ dpois(lam[i, j]*z[i])
      }
    }
  }
  for (j in 1:J){
    for (k in 1:K){
      n[j, k] ~ dsum(y[1, j, k], y[2, j, k], y[3, j, k], y[4, j, k], y[5, j, k], y[6, j, k], y[7, j, k], y[8, j, k], y[9, j, k], y[10, j, k], y[11, j, k], y[12, j, k], y[13, j, k], y[14, j, k], y[15, j, k], y[16, j, k], y[17, j, k], y[18, j, k], y[19, j, k], y[20, j, k], y[21, j, k], y[22, j, k], y[23, j, k], y[24, j, k], y[25, j, k], y[26, j, k], y[27, j, k], y[28, j, k], y[29, j, k], y[30, j, k], y[31, j, k], y[32, j, k], y[33, j, k], y[34, j, k], y[35, j, k], y[36, j, k], y[37, j, k], y[38, j, k], y[39, j, k], y[40, j, k], y[41, j, k], y[42, j, k], y[43, j, k], y[44, j, k], y[45, j, k], y[46, j, k], y[47, j, k], y[48, j, k], y[49, j, k], y[50, j, k], y[51, j, k], y[52, j, k], y[53, j, k], y[54, j, k], y[55, j, k], y[56, j, k], y[57, j, k], y[58, j, k], y[59, j, k], y[60, j, k], y[61, j, k], y[62, j, k], y[63, j, k], y[64, j, k], y[65, j, k], y[66, j, k], y[67, j, k], y[68, j, k], y[69, j, k], y[70, j, k], y[71, j, k], y[72, j, k], y[73, j, k], y[74, j, k], y[75, j, k], y[76, j, k], y[77, j, k], y[78, j, k], y[79, j, k], y[80, j, k], y[81, j, k], y[82, j, k], y[83, j, k], y[84, j, k], y[85, j, k], y[86, j, k], y[87, j, k], y[88, j, k], y[89, j, k], y[90, j, k], y[91, j, k], y[92, j, k], y[93, j, k], y[94, j, k], y[95, j, k], y[96, j, k], y[97, j, k], y[98, j, k], y[99, j, k], y[100, j, k], y[101, j, k], y[102, j, k], y[103, j, k], y[104, j, k], y[105, j, k], y[106, j, k], y[107, j, k], y[108, j, k], y[109, j, k], y[110, j, k], y[111, j, k], y[112, j, k], y[113, j, k], y[114, j, k], y[115, j, k], y[116, j, k], y[117, j, k], y[118, j, k], y[119, j, k], y[120, j, k], y[121, j, k], y[122, j, k], y[123, j, k], y[124, j, k], y[125, j, k], y[126, j, k], y[127, j, k], y[128, j, k], y[129, j, k], y[130, j, k], y[131, j, k], y[132, j, k], y[133, j, k], y[134, j, k], y[135, j, k], y[136, j, k], y[137, j, k], y[138, j, k], y[139, j, k], y[140, j, k], y[141, j, k], y[142, j, k], y[143, j, k], y[144, j, k], y[145, j, k], y[146, j, k], y[147, j, k], y[148, j, k], y[149, j, k], y[150, j, k], y[151, j, k], y[152, j, k], y[153, j, k], y[154, j, k], y[155, j, k], y[156, j, k], y[157, j, k], y[158, j, k], y[159, j, k], y[160, j, k], y[161, j, k], y[162, j, k], y[163, j, k], y[164, j, k], y[165, j, k], y[166, j, k], y[167, j, k], y[168, j, k], y[169, j, k], y[170, j, k], y[171, j, k], y[172, j, k], y[173, j, k], y[174, j, k], y[175, j, k], y[176, j, k], y[177, j, k], y[178, j, k], y[179, j, k], y[180, j, k], y[181, j, k], y[182, j, k], y[183, j, k], y[184, j, k], y[185, j, k], y[186, j, k], y[187, j, k], y[188, j, k], y[189, j, k], y[190, j, k], y[191, j, k], y[192, j, k], y[193, j, k], y[194, j, k], y[195, j, k], y[196, j, k], y[197, j, k], y[198, j, k], y[199, j, k], y[200, j, k])
    }
  }

  N <- sum(z[])
  A <- (xlim[2] - xlim[1])*(ylim[2] - ylim[1])
  D <- N/A
  ED <- (M*psi)/A
}", fill = T)
sink()

# Bind data
M <- 200 # Imaginary, possible animals
dat1 <- list(n = n, X = X, J = J, K = K, M = M, xlim = xlim, ylim = ylim)

# Starting values
init1 <- function() {
  yi <- array(0, c(dat1$M, dat1$J, dat1$K))
  for (j in 1:dat1$J){
    for (k in 1:dat1$K){
      yi[sample(1:dat1$M, dat1$n[j, k]), j, k] <- 1
    }
  }
  list(sigma = runif(1, 1, 2), lam0 = runif(1), y = yi, z = rep(1, dat1$M))
} 

pars1 <- c("lam0", "sigma", "N", "mu") 

# Call JAGS from R
out <- jags(dat1, init1, pars1, "model.jags", n.chains = 3,
            n.thin = 4, n.iter = 1000, n.burnin = 300, working.directory = getwd())

# Summarize posteriors
print(out, dig = 2)

############################################ 
# Run time: 23:00

# Write the model for JAGS
load.module("glm")
sink("model.jags")
cat("
model {
  sigma ~ dunif(0, 200)
  lam0 ~ dunif(0, 5)
  psi ~ dbeta(1, 1)
  for (i in 1:M){
    z[i] ~ dbern(psi)
    s[i, 1] ~ dunif(xlim[1], xlim[2])
    s[i, 2] ~ dunif(ylim[1], ylim[2])
    for(j in 1:J){
      distsq[i, j] <- (s[i, 1] - X[j, 1])^2 + (s[i, 2] - X[j, 2])^2
      lam[i, j] <- lam0*exp(-distsq[i, j]/(2*sigma^2))
    }
  }
  for (j in 1:J){
    bigLambda[j] <- sum(lam[, j])
    for (k in 1:K){
      n[j, k] ~ dpois(bigLambda[j])
    }
  }
  N <- sum(z[])
}", fill = T)
sink()

# Bind data
M <- 200
dat1 <- list(n = n, X = X, J = J, K = K, M = M, xlim = xlim, ylim = ylim)

# Starting values
init1 <- function() {
  yi <- array(0, c(dat1$M, dat1$J, dat1$K))
  for (j in 1:dat1$J){
    for (k in 1:dat1$K){
      yi[sample(1:dat1$M, dat1$n[j, k]), j, k] <- 1
    }
  }
  list(sigma = runif(1, 1, 2), lam0 = runif(1), y = yi, z = rep(1, dat1$M))
} 

pars1 <- c("lam0", "sigma", "N", "mu") # Where is mu?

# Call JAGS from R
out <- jags(dat1, init1, pars1, "model.jags", n.chains = 3,
            n.thin = 4, n.iter = 1000, n.burnin = 300, working.directory = getwd())

# Summarize posteriors
print(out, dig = 2)

out2 <- as.mcmc(out)
mcmcplot(out2)
# Oh, dear god. 
# save it anyway
save(out, file = "GitHub/Camera-trap-study/Simulation/SECR/unmarkSECR_jagsoutput_20160715.RData")

#######################
# Another option (runtime 4 minutes 2000 iters)
library(scrbook) 

# Set a clock
ptm <- Sys.time()

# Use scrbook function
out <- scrUN(n = n, X = X, M = 300, niter = 2000, xlims = xlim, ylims = ylim, 
             inits = list(lam0 = 0.3, sigma = rnorm(1, 5, 0.1)), updateY = T,
             tune = c(0.004, 0.09, 0.35))

# Stop the clock
Sys.time() - ptm

### I'm not sure how to look at the mcmc output