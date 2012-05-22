d = read.table("farm62.txt", header=T)

sires = unique(as.character(d$sire)); n.sires = length(sires)
dams  = unique(as.character(d$dam))
sows = unique(as.character(d$sowid))
dams.only = setdiff(dams,sows); n.dams.only = length(dams.only) # Sows with no nba data

ids = factor(c(sires,dams.only,sows),c(sires,dams.only,sows), ordered=T)

# Pedigree
first = match(sows,d$sowid)  # Assumes all the pedigree data are correct
pedigree = d[first,c(1,6,7)]
for (i in 1:ncol(pedigree)) {
  levels(pedigree[,i]) = levels(ids)
  pedigree[,i] = as.numeric(pedigree[,i])
}
names(pedigree)[1] = "id"

# Mixed effect Poisson regression (no pedigree)
require(lme4)
summary(freq <- glmer(nba~factor(parity)+(1|sowid), d, family=poisson))
re = ranef(freq)$sowid[,1]

# Evidence of heavy tails
par(mfrow=c(1,2))
hist(re, freq=F, 30)
curve(dnorm(x, mean(re), sd(re)), add=T)
qqnorm(re)
qqline(re)


# Data/inits/model for BUGS/JAGS
x = model.matrix(freq)
dat = list(n=nrow(d), y=d$nba, x=x, p=ncol(x), sowid=d$sowid,
           m=length(ids), n.sire=length(sires), n.dam.only=length(dams.only),
           sire=pedigee$sire, dam=pedigree$dam)
parms = c("b","a","sigma","mu.a")
model.file= "../code/poisHier.txt"

# JAGS
require(rjags)
mod = jags.model(model.file, data=dat, n.chains=3, n.adapt=1e4)
samps = coda.samples(mod, parms, 1e4, thin=10)
gelman.diag(samps)
summary(samps)

a.med = apply(as.matrix(samps[,1:230]),2,median)
plot(a,a.med)
text(a[1:30],a.med[1:30],as.character(1:230)[1:30])






