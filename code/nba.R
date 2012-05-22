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
for (i in 1:length(id)) a[id[i]] = rnorm(1,.5*a[sire[i]]+.5*a[dam[i]],.1)

# Fixed effects
id = rep(id,maxparity)
x = matrix(rnorm(n),ncol=1)
b = .1

# Response
y = rpois(n,exp(2.5+x%*%b+a[id]))
hist(y,seq(-.5,max(y)+.5,by=1))

# Data frame
d = data.frame(y=y, id=id, x=x,
               sire = rep(sire, maxparity),
               dam  = rep(dam , maxparity))

# Mixed effect Poisson regression (no pedigree)
require(lme4)
summary(freq <- glmer(y~x+(1|id), family=poisson))
#plot(a[-c(1:30)],ranef(freq)$id[,1])

# Data/inits/model for BUGS/JAGS
dat = list(n=n,y=y,x=cbind(1,x),p=ncol(x)+1,m=length(a),id=id, sire=sire, dam=dam, 
           n.sire=length(unique(sire)), n.dam.only=length(unique(dam)))
parms = c("b","a","sigma")
model.file= "poisHier.txt"

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3, n.adapt=1e4)
samps = coda.samples(mod, parms, 1e4, thin=10)
gelman.diag(samps)
summary(samps)

a.med = apply(as.matrix(samps[,1:230]),2,median)
plot(a,a.med)
text(a[1:30],a.med[1:30],as.character(1:230)[1:30])






