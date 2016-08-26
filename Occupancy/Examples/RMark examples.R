# Occupancy example
# Jay Rotella MSU

library(RMark)

# you can ignore warning about final line issue that might appear
weta = convert.inp("http://www.montana.edu/rotella/documents/502/wetaTwoGroups.inp", 
                   group.df = data.frame(Browse = c("Browsed", "Unbrowsed")), 
                   covariates = c("obs11", "obs12", "obs13", "obs14", "obs15",
                                  "obs21", "obs22", "obs23", "obs24", "obs25"))

# store the number of sites
n.sites = length(weta$ch)
# store the maximum number of visits
n.visits = max(nchar(weta$ch))

# make a factor that records whether observer 1, 2, or 3 visited a site on
# each visit. A '9' was used to indicate that no observer visited the site.
# So, if there's a 9 for either observer 1 or 2 on a given date, we assign a
# '.' for that day
obs = matrix(NA, n.sites, n.visits)
for (i in 1:n.sites) {
  obs[i, 1] = ifelse(weta$obs11[i] == 1, 1, ifelse(weta$obs21[i] == 1, 2, 
                                                   ifelse(weta$obs11[i] == 9, ".", 3)))
  obs[i, 2] = ifelse(weta$obs12[i] == 1, 1, ifelse(weta$obs22[i] == 1, 2, 
                                                   ifelse(weta$obs12[i] == 9, ".", 3)))
  obs[i, 3] = ifelse(weta$obs13[i] == 1, 1, ifelse(weta$obs23[i] == 1, 2, 
                                                   ifelse(weta$obs13[i] == 9, ".", 3)))
  obs[i, 4] = ifelse(weta$obs14[i] == 1, 1, ifelse(weta$obs24[i] == 1, 2, 
                                                   ifelse(weta$obs14[i] == 9, ".", 3)))
  obs[i, 5] = ifelse(weta$obs15[i] == 1, 1, ifelse(weta$obs25[i] == 1, 2, 
                                                   ifelse(weta$obs15[i] == 9, ".", 3)))
}

# can drop obs11, obs12, ..., obs25 from weta since not used
weta = weta[, -c(4:13)]
# add 'Obs' values for each day to 'weta'
weta$Obs1 = as.factor(obs[, 1])
weta$Obs2 = as.factor(obs[, 2])
weta$Obs3 = as.factor(obs[, 3])
weta$Obs4 = as.factor(obs[, 4])
weta$Obs5 = as.factor(obs[, 5])

# Create function to fit the 18 models in the book
fit.weta.models = function() {
  # use 'make.time.factor' to create time-varying dummy variables Obs1 and
  # Obs2 observer 3 is used as the intercept to match what we did in MARK
  weta = make.time.factor(weta, "Obs", 1:5, intercept = 3)
  
  # Process data
  weta.process = process.data(weta, model = "Occupancy", groups = "Browse")
  weta.ddl = make.design.data(weta.process)
  
  # time factor variable copied to 'Day' to match name used in the lab
  weta.ddl$p$Day = weta.ddl$p$time
  
  # Define p models
  p.dot = list(formula = ~1)
  p.browse = list(formula = ~Browse)
  p.day = list(formula = ~Day)
  p.obs = list(formula = ~Obs1 + Obs2)
  p.day.obs = list(formula = ~Day + Obs1 + Obs2)
  p.day.browse = list(formula = ~Day + Browse)
  p.obs.browse = list(formula = ~Obs1 + Obs2 + Browse)
  p.day.obs.browse = list(formula = ~Day + Obs1 + Obs2 + Browse)
  # Define Psi models
  Psi.dot = list(formula = ~1)
  Psi.browse = list(formula = ~Browse)
  # Create model list
  cml = create.model.list("Occupancy")
  # Run and return marklist of models
  return(mark.wrapper(cml, data = weta.process, ddl = weta.ddl))
}

weta.models = fit.weta.models()
weta.models

# Store and view a model table that uses AIC rather than AICc
AIC.table = weta.models
AIC.table$model.table = model.table(weta.models, use.AIC = TRUE)
AIC.table

# View the model names
names(weta.models)

# examine the output from top-ranked model (#7) and store top model in
# object with short name
top = weta.models$p.day.obs.Psi.browse

# look at summary of top model's output
summary(top, showall = FALSE)

Mod.Avg.Psi = model.average(weta.models, "Psi")
Mod.Avg.Psi



#######################################################
# Dipper example from MARK
data(dipper)
head(dipper)

# Step 1. Make processed data
dipper.proc=process.data(dipper, model="CJS",
                         groups="sex", begin.time=1981)
names(dipper.proc)
dipper.proc$nocc # Number of occassions, calculated from data
dipper.proc$time.intervals # Default value

# Step 2. Make design data
dipper.ddl=make.design.data(dipper.proc)
# Makes a list of 3 dataframes: phi, p, and pimtypes
names(dipper.ddl)
head(dipper.ddl$Phi)
# Add a covariate for flood year yes/no
dipper.ddl$Phi$flood=ifelse(dipper.ddl$Phi$time%in%1982:1983,1,0)

# Consider a model with time-varying survival, and make a design matrix for it
dm=model.matrix(~time,dipper.ddl$Phi)
head(dm)
# Contrast this with dm=model.matrix(~Time,dipper.ddl$Phi). why this difference?
# You can make any year the intercept
dipper.ddl$Phi$time=relevel(dipper.ddl$Phi$time,"1986")
dm=model.matrix(~time,dipper.ddl$Phi)

# Make a design matrix for an additive model
dm=model.matrix(~sex+flood,dipper.ddl$Phi)
dm[c(1:4,37:40),]

# Step 3. Write a function for all the models
#   specify all your models
#   create.model.list
#   mark.wrapper (calls mark() for each of the models)
# specify output = F in mark.wrapper
# Create function with parameter specifications to fit models
 dipper.analysis=function()
   {
     # Create specifications for Phi and p
       Phi.1=list(formula=~time)
       Phi.2=list(formula=~-1+time,link="sin")
       Phi.3=list(formula=~sex+weight)
       Phi.4=list(formula=~flood)
       p.1=list(formula=~1)
       p.2=list(formula=~time)
       p.3=list(formula=~Time)
       p.4=list(formula=~sex)
       # Create a list of combinations of parameter specifications;
         # the argument "CJS" tells it to look for CJS parameters
         cml=create.model.list("CJS")
         # Call mark.wrapper; the arguments are cml and then like with
           # the mark function the processed data and ddl are specified,
           # but they must be named data= and ddl= because they are passed
           # through to the mark function; I've also set output=FALSE
           # so it doesn't show a summary for the 16 models but it will
           # show the model being fitted and any notations or errors.
           mark.wrapper(cml,data=dipper.proc,ddl=dipper.ddl,output=FALSE)
   }

# Step 4. call function. Read analysis
dipper.results = dipper.analysis()
dipper.results
summary(dipper.results[[13]]) # Look at the best model

dipper.results[[11]]$design.matrix
# ???? This is supposed to have weight as a covariate???
