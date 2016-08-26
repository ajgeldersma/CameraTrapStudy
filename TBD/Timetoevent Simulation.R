# Simulating time-to-event data
# Anna Moeller
# 2/29/2016

# Game Plan
# 1. Simulate time-to-event for elk at 1 site that walk around completely
#   independently
#  - use rexp(num.sampling.per, photo.rate)
#  - make this photo.rate change as N changes
#  - make this photo.rate change as p changes
# 2. Somehow build a model that tells me how many elk there are based on observed
#   times-to-event. 
#  - This probably has something to do with dpois(obs, ???) # dpois = PMF
#  - Somehow make this tell me p at the same time
# Make this more complex: 
#  - Make it work for multiple sites at a time
#  - account for covariates that influence N and p
#  - account for group size if elk don't move independently
#  - account for elk leaving/entering plot?

# Simulate/model??? the number of animals in a grid cell
# N_j ~ Pois(1, avg.num.per.cell)
# log(avg.num.per.cell) = B0 + B1 (elevation) + B2 (slope) + B3 (aspect) + 
#   B4 (snow depth) + B5 (forage quality) + random effect_j

# Simulate p at each camera
# log(p_i) = A0 + A1 (trail width) + A2 (flash type) + A3 (temperature) + 
#   A4 (group size) + A5 (time of day) + A6 (behavioral ) + random effect_i

# Simulate how the camera trapping rate changes as p and N change
### What is this relationship???

########################################################################
# Okay, let's take a stab at it...

# Simulate how many elk are at one site
N <- rpois(1, lambda = 50)
# These elk live in this plot and never leave and walk around independently of 
#   each other

# now we try to observe these elk
# This is the time until we see 1 elk, during 10 sampling periods.
timeint <- rexp(10, rate = r)
# r is the rate of events
# we want r to increase as N increases
### What is this relationship??? Am I supposed to make it up? 
r <- m*N + b# the most basic possible relationship
m <- #??
b <- #??

