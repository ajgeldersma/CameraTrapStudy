# TIA check
# Anna Moeller 
# 6/27/2015

### Beaverhead: 
# function(x){}
x <- pretty

# every pic should have something in the elk.present column (except earlier 
# versions won't have anything where op.state = "setup")
any(x$elk.present == "" & !(x$op.state == "Setup" | x$op.state == "Takedown"))
# Stop if true

# Is any count blank
any(x[, 22:44]) == ""
# Stop if true

# is there a difference between 01 and 1?

# check that counts are mostly 1's and 2's (not 10 and 20)

# Let's add a censor column
# If op.state != "operating" or "" or "setup" or "takedown", check it out
# for the first ones, blank is operating
any(x$op.state != "" & x$op.state != "Operating" & x$op.state != "Setup" & x$op.state != "Takedown")
# for later, blank is something to look into.
any(x$op.state == "")
any(x$op.state != "Operating" & x$op.state != "Setup" & x$op.state != "Takedown")
# Check if view has shifted ### This isn't in any pictures I've done so far
any(x$op.state == "Wrong direction")
# Censor if snow, Wrong direction

# What do I do if it is tilted? 
any(x$op.state == "Tilted")
# if true, do something

# calculate the number of camera days
# From the last "setup" 
if (any(x$op.state == "Setup")){
  first <- max(which(x$op.state == "Setup"))
} else {
  first <- 1
}
# to the first "Takedown"
if (any(x$op.state == "Takedown")){
  last <- min(which(x$op.state == "Takedown"))
} else {
  last <- length(x$op.state)
}
# Take out all the censored photos
subset <- x[first:last, ]
subset2 <- subset[subset$op.state != "Snow", ]
camdays <- unique(subset2$dateLST)

# check that sampler is filled in for every picture
any(x$sampler == "")
# if True, do something

