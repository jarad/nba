# Pedigree random effects
# manually create pedigree
n.sire = 10
n.dam.only = 20
sire = rep(1:n.sire,each=n.dam.only)
dam  = rep(n.sire+(1:n.dam.only),n.sire)
id   = seq(from=n.sire+n.dam.only+1,length=length(dam),by=1)
maxparity = sample(4, length(id), replace=T)
n    = sum(maxparity)

a = rnorm(n.sire+n.dam.only,0,1)
for (i in 1:n) a[id[i]] = rnorm(1,.5*a[sire[i]]+.5*a[dam[i]],.1)

# Fixed effects
id = rep(id,maxparity)
x = matrix(rnorm(n),ncol=1)
b = .1

# Response
y = rpois(n,exp(2.5+x%*%b+a[id]))
hist(y,seq(-.5,max(y)+.5,by=1))

# Data frame
d = data.frame(y=y, id=id, x=x,
               sire = rep(pedigree$sire, maxparity)
               dam  = rep(pedigree$dam , maxparity)

# Mixed effect Poisson regression (no pedigree)
require(lme4)
summary(freq <- glmer(y~x+(1|id), family=poisson))
#plot(a,ranef(freq)$z[,1])

# Data/inits/model for BUGS/JAGS
dat = list(n=n,y=y,x=cbind(1,x),p=ncol(x)+1,m=length(a),id=id, sire=d$sire, dam=d$dam, 
           n.sire=length(unique(sire)), n.dam.only=length(unique(dam)), sire=sire, dam=dam)
parms = c("b","a","sigma","pp")
model.file= "poisHier.txt"

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3, n.adapt=1e5)
samps = coda.samples(mod, parms, 1e4, thin=10)
gelman.diag(samps)
summary(samps)








