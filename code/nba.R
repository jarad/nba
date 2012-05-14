# Simulate data
n = 10
x = cbind(1,rnorm(n))
b = c(0,.1)
y = rpois(n,exp(x%*%b))

# Poisson regression
summary(freq <- glm(y~x-1, poisson))
confint(freq)

# Data/inits/model for BUGS/JAGS
dat = list(n=n,y=y,x=x,p=ncol(x))
parms = c("b")
model.file= "poisReg.txt"

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3)
samps = coda.samples(mod, parms, 1000)
gelman.diag(samps)
summary(samps)








