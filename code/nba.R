# Simulate data
n = 100 # number of replicates
m = 100 # number of individuals

# Fixed effect
x = cbind(1,rnorm(n*m))
b = c(0,.1)

# Random effect
a = rnorm(m)
z = rep(1:m,each=n)
y = rpois(n*m,exp(x%*%b+a[z]))

# Poisson regression
require(lme4)
summary(freq <- glmer(y~x[,-1]+(1|z), family=poisson))
plot(a,ranef(freq)$z[,1])

# Data/inits/model for BUGS/JAGS
dat = list(n=n,y=y,x=x,p=ncol(x),m=m,z=z)
parms = c("b","a","sigma")
model.file= "poisMixed.txt"

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3)
samps = coda.samples(mod, parms, 1000)
gelman.diag(samps)
summary(samps)








