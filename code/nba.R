# Simulate data
n = 10
mu = 1
y = rpois(n,mu)

# Poisson regression
summary(mod <- glm(y~1, poisson))
exp(confint(mod))

# Data/inits/model for BUGS/JAGS
dat = list(n=n,y=y)
inits = NULL
parms = c("mu")
model.file= "poisson.txt"

# WinBUGS
require(R2WinBUGS)
o1 = bugs(dat,NULL,parms,model.file) # Error, but appears to run

require(BRugs)
modelCheck("poisson.txt")
bugsData(dat,"data.txt")
modelData("data.txt")
modelCompile(numChains=2)
modelGenInits()
modelUpdate(1000)
samplesSet(parms)
modelUpdate(1000)
samplesStats("*")










