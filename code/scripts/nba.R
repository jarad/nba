# Simulate data
n = 10
mu = 1
y = rpois(n,mu)

# Poisson regression
summary(mod <- glm(y~1, poisson))
exp(confint(mod))

