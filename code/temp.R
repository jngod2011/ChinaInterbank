ergm.formula <- as.formula(
  temp.y ~ sum(pow = 1)
  #+ edgecov(temp.loan, form = "sum") 
  + nodecov("ass", form = "sum") 
  + absdiff("sty", pow=1, form="sum") 
  + absdiff("dbt", pow=1, form="sum") 
  + cyclicalweights(twopath="min",combine="max",affect="min")
  + mutual(form="min",threshold= 0)
  + nodemix("ecl", base=1, form="sum") 
  + transitiveties(threshold=0)
  + transitiveweights(twopath="min",combine="max",affect="min")
  + nodesqrtcovar(center=TRUE)
  + nodeosqrtcovar
  + nodeocovar
  + nodeicovar
  + nodecovar
  + nodeofactor("ecl", base=1)
  #+ nodeifactor("ecl", base=1)
  #+ absdiffcat("sty", base=NULL, form="sum")
)

temp.y %v% "ecl" <- c("SOB","SOB","SOB","SOB","JECB","JECB","JECB","JECB","JECB",
                      "UCB")
result.vergm <-
  ergm(ergm.formula,
       response="weight",
       reference=~Binomial(max.temp.y),
       control=control.ergm(MCMLE.maxit=5),#init=c(init, rep(0, n.inclusion)),
       verbose=F)


su <- summary(result.vergm)
su$coefs

trans <- c("ON")
triangle <- "all"
directed <- T
BidType <- "vecm.gir"
inclusion.edgecov <- c("ON")
inclusion.nodecov <- NULL
inclusion.nodeocov <- NULL
inclusion.nodeicov <- c("ass")
inclusion.absdiff <-NULL
inclusion.cyclicalweights <- T
inclusion.mutual <- T
inclusion.nodecovar <- T
inclusion.nodeicovar <-T
inclusion.nodeocovar <- T
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- T
x <- dyCoefErgm.yearly(data = vecm.myl.gir[1:2], set = set, 
           inclusion.edgecov = inclusion.edgecov,
           inclusion.nodecov = inclusion.nodecov,
           inclusion.nodeocov = inclusion.nodeocov,
           inclusion.nodeicov = inclusion.nodeicov,
           inclusion.absdiff = inclusion.absdiff,
           inclusion.cyclicalweights = inclusion.cyclicalweights,
           inclusion.mutual = inclusion.mutual,
           inclusion.nodecovar = inclusion.nodecovar,
           inclusion.nodeicovar = inclusion.nodeicovar,
           inclusion.nodeocovar = inclusion.nodeocovar,
           inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar,
           inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar,
           Date = network.y$Date, 
           trans = trans, 
           BidType = BidType,
           MCMLE.maxit = 20,
           triangle = triangle,
           directed = directed,
           tab = T,
           wind = raw.wind[3:10]
)

y <- summary(x$result.vergm.l[[1]])
names(y)
y$coefs

summary(result.vergm)$coefs
