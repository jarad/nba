# Pedigree random effects
require(pedigree)
# manually create pedigree
n.sire = 10
n.dam.only = 20
sire = rep(1:n.sire,each=n.dam.only)
dam  = rep(n.sire+(1:n.dam.only),n.sire)
id   = seq(from=n.sire+n.dam.only+1,length=length(dam),by=1)
pedigree = data.frame(id=id,dam=dam,sire=sire)
maxparity = sample(4, nrow(pedigree), replace=T)

a = rnorm(n.sire+n.dam.only,0,1)
for (i in 1:nrow(pedigree)) a[id[i]] = rnorm(1,.5*a[sire[i]]+.5*a[dam[i]],.1)

# Fixed effects
n = sum(maxparity)
id = rep(pedigree$id,maxparity)
x = matrix(rnorm(n),ncol=1)
b = .1

# Response
y = rpois(n,exp(x%*%b+a[id]))

# Mixed effect Poisson regression (no pedigree)
require(lme4)
summary(freq <- glmer(y~x+(1|id), family=poisson))
plot(a,ranef(freq)$z[,1])

# Data/inits/model for BUGS/JAGS
dat = list(n=n,y=y,x=cbind(1,x),p=ncol(x)+1,m=length(a),id=id, idrow=id-(n.sire+n.dam.only), 
           n.sire=length(unique(sire)), n.dam.only=length(unique(dam)), sire=sire, dam=dam)
parms = c("b","a","sigma")
model.file= "poisHier.txt"

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3)
samps = coda.samples(mod, parms, 1e4)
gelman.diag(samps)
summary(samps)








