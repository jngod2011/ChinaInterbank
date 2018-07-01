install.packages("ergm.count")
install.packages("latentnet")
library(ergm.count)
library(latentnet)
as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default
data(samplk)
ls()
as.matrix(samplk1)[1:5,1:5]
# A sociomatrix totaling the nominations.
samplk.tot.m<-as.matrix(samplk1)+as.matrix(samplk2)+as.matrix(samplk3)  samplk.tot.m[1:5,1:5]
# Create a network where the number of nominations becomes an attribute of an edge.
samplk.tot <- as.network(samplk.tot.m, directed=TRUE, matrix.type="a",
                         ignore.eval=FALSE, names.eval="nominations" # Important!
)
# Add vertex attributes.
samplk.tot %v% "group" <- samplk1 %v% "group" # Groups identified by Sampson
samplk.tot %v% "group"
samplk.tot %v% "vertex.names" <- samplk1 %v% "vertex.names" # Names
# We can view the attribute as a sociomatrix.
as.matrix(samplk.tot,attrname="nominations")[1:5,1:5]
# Also, note that samplk.tot now has an edge if i nominated j *at least once*.
as.matrix(samplk.tot)[1:5,1:5]



summary(samplk.tot~sum)

summary(samplk.tot~sum, response="nominations")

y <- network.initialize(2,directed=FALSE) # A network with one dyad!

## Discrete Uniform reference
help("ergm-references")
# 0 coefficient: discrete uniform
sim.du3<-simulate(y~sum, coef=0, reference=~DiscUnif(0,3),
                  response="w",statsonly=TRUE,nsim=1000)
# Negative coefficient: truncated geometric skewed to the right
sim.trgeo.m1<-simulate(y~sum, coef=-1, reference=~DiscUnif(0,3),
                       response="w",statsonly=TRUE,nsim=1000)
# Positive coefficient: truncated geometric skewed to the left
sim.trgeo.p1<-simulate(y~sum, coef=+1, reference=~DiscUnif(0,3),
                       response="w",statsonly=TRUE,nsim=1000)
# Plot them:
par(mfrow=c(1,3))
hist(sim.du3,breaks=diff(range(sim.du3))*4)
hist(sim.trgeo.m1,breaks=diff(range(sim.trgeo.m1))*4)
hist(sim.trgeo.p1,breaks=diff(range(sim.trgeo.p1))*4)

## Binomial reference
# 0 coefficient: Binomial(3,1/2)
sim.binom3<-simulate(y~sum, coef=0, reference=~Binomial(15),
                     response="w",statsonly=TRUE,nsim=1000)
# -1 coefficient: Binomial(3, exp(-1)/(1+exp(-1)))
sim.binom3.m1<-simulate(y~sum, coef=-1, reference=~Binomial(15),
                        response="w",statsonly=TRUE,nsim=1000)
# +1 coefficient: Binomial(3, exp(1)/(1+exp(1)))
sim.binom3.p1<-simulate(y~sum, coef=+1, reference=~Binomial(15),
                        response="w",statsonly=TRUE,nsim=1000)
# Plot them:
par(mfrow=c(1,3))
hist(sim.binom3,breaks=diff(range(sim.binom3))*4)
hist(sim.binom3.m1,breaks=diff(range(sim.binom3.m1))*4)
hist(sim.binom3.p1,breaks=diff(range(sim.binom3.p1))*4)

sim.geom<-simulate(y~sum, coef=log(10), reference=~Geometric,
                   response="w",statsonly=TRUE,nsim=1000)
mean(sim.geom)


sim.pois<-simulate(y~sum, coef=log(100), reference=~Poisson,
                   response="w",statsonly=TRUE,nsim=1000)
mean(sim.pois)



par(mfrow=c(1,2))
hist(sim.geom,breaks=diff(range(sim.geom))*4)
hist(sim.pois,breaks=diff(range(sim.pois))*4)


par(mfrow=c(1,1))
sim.geo0<-simulate(y~sum, coef=0, reference=~Geometric,
                   response="w",statsonly=TRUE,nsim=100,
                   control=control.simulate(MCMC.burnin=0,MCMC.interval=1))
mean(sim.geo0)
plot(c(sim.geo0),xlab="MCMC iteration",ylab="Value of the tie")

p <- sum(samplk.tot %e% "nominations")/3/network.dyadcount(samplk.tot)
samplk.sum.init <- log(p/(1-p)) # i.e., logit(p)

m <- sum(zach %e% "contexts")/network.dyadcount(zach)
zach.sum.init <- log(m)

help("ergm-terms")

# example
npar <- length(summary(samplk.tot~sum + nodematch("group",diff=TRUE,form="sum"), response = "nominations"))
npar

samplk.tot.nm <-
  ergm(samplk.tot~sum + nodematch("group",diff=TRUE,form="sum"),
       response="nominations", reference=~Binomial(3),
       control=control.ergm(init=c(samplk.sum.init, rep(0, npar-1))))
mcmc.diagnostics(samplk.tot.nm)
summary(samplk.tot.nm)



as.matrix(samplk.tot, attrname="nominations")
















