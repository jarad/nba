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

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3)
samps = coda.samples(mod, parms, 1000)
gelman.diag(samps)
summary(samps)








