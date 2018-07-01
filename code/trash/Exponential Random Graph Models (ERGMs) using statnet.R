# https://statnet.org/trac/raw-attachment/wiki/Sunbelt2016/ergm_tutorial.html#getting-started
# Getting Started
library(statnet)
library(ergm)
library(sna)
library(coda)
sessionInfo()
set.seed(0)

#  Statistical network modeling; the summary and ergm commands, and supporting functions
data(package='ergm') # tells us the datasets in our packages

## Bernoulli model
data(florentine) # loads flomarriage and flobusiness data
flomarriage # Let's look at the flomarriage network properties
par(mfrow=c(1,2)) # Setup a 2 panel plot (for later)
plot(flomarriage, main="Florentine Marriage", cex.main=0.8) # Plot the flomarriage network
summary(flomarriage~edges) # Look at the $g(y)$ statistic for this model
flomodel.01 <- ergm(flomarriage~edges) # Estimate the model 
summary(flomodel.01) # The fitted model object

## Triad formation
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)
class(flomodel.02) # this has the class ergm
names(flomodel.02) # the ERGM object contains lots of components.
flomodel.02$coef # you can extract/inspect individual components

## Nodal covariates: effects on mean degree

wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth
summary(wealth) # summarize the distribution of wealth
plot(flomarriage, vertex.cex=wealth/25, main="Florentine marriage by wealth", cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth'))# observed statistics for the model
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

## Nodal covariates: Homophily
data(faux.mesa.high) 
mesa <- faux.mesa.high
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)
fauxmodel.01 <- ergm(mesa ~edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))
summary(fauxmodel.01)
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")
summary(mesa ~edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))

## Directed ties
data(samplk) 
ls() # directed data: Sampson's Monks
samplk3
plot(samplk3)
summary(samplk3~edges+mutual)
sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)

## Missing data
missnet <- network.initialize(10,directed=F)
missnet[1,2] <- missnet[2,7] <- missnet[3,6] <- 1
missnet[4,6] <- missnet[4,9] <- missnet[5,6] <- NA
summary(missnet)

tempnet <- missnet # plot missnet with missing edge colored red. 
tempnet[4,6] <- tempnet[4,9] <- tempnet[5,6] <- 1
missnetmat <- as.matrix(missnet)
missnetmat[is.na(missnetmat)] <- 2
plot(tempnet,label = network.vertex.names(tempnet),edge.col = missnetmat)
summary(missnet~edges)
summary(ergm(missnet~edges))
missnet_bad <- missnet
missnet_bad[4,6] <- missnet_bad[4,9] <- missnet_bad[5,6] <- 0
summary(missnet_bad)
summary(ergm(missnet_bad~edges))

# Model terms available for ergm estimation and simulation

## Terms provided with ergm
help('ergm-terms')

# Network simulation: the simulate command and network.list objects
flomodel.03.sim <- simulate(flomodel.03,nsim=10)
class(flomodel.03.sim)
summary(flomodel.03.sim)
length(flomodel.03.sim)
flomodel.03.sim[[1]]
plot(flomodel.03.sim[[1]], label= flomodel.03.sim[[1]] %v% "vertex.names")

# Examining the quality of model fit – GOF
flo.03.gof.model <- gof(flomodel.03 ~ model)
flo.03.gof.model
plot(flo.03.gof.model)
flo.03.gof.global <- gof(flomodel.03 ~ degree + esp + distance)
flo.03.gof.global
plot(flo.03.gof.global)
mesamodel.b <- ergm(mesa~edges)
plot(gof(mesamodel.b ~ model, nsim=10))
plot(gof(mesamodel.b ~ degree + esp + distance, nsim=10))

#  Diagnostics: troubleshooting and checking for model degeneracy
## What it looks like when a model converges properly
summary(flobusiness ~ edges+degree(1))
fit <- ergm(flobusiness ~ edges+degree(1))
mcmc.diagnostics(fit)
ergm(flobusiness ~ edges+degree(1), 
     control=control.ergm(MCMC.interval=1))
## What it looks like when a model fails

data('faux.magnolia.high')
magnolia <- faux.magnolia.high
plot(magnolia, vertex.cex=.5)
summary(magnolia ~ edges+triangle)
ergm(magnolia ~ edges+triangle)
fit.mag.01 <- ergm(magnolia ~ edges+triangle, control=control.ergm(MCMLE.maxit=2))
mcmc.diagnostics(fit.mag.01)
fit.mag.02 <- ergm(magnolia ~ edges+gwesp(0.25,fixed=T))
mcmc.diagnostics(fit.mag.02)
fit.mag.02 <- ergm(magnolia ~ edges+gwesp(0.25,fixed=T), verbose=T)
gof.mag.02.model <- gof(fit.mag.02, GOF = ~model)
gof.mag.02.model
plot(gof.mag.02.model)

fit.mag.03 <- ergm(magnolia ~ edges+gwesp(0.25,fixed=T)
                   +nodematch('Grade')+nodematch('Race')+nodematch('Sex'),
                   control = control.ergm(MCMC.interval=20000), eval.loglik = F)
summary(fit.mag.03)
mcmc.diagnostics(fit.mag.03)
plot(gof(fit.mag.03, GOF=~model, control=control.gof.ergm(nsim=200)))
plot(gof(fit.mag.03, GOF = ~ degree + esp + distance))

# 




































