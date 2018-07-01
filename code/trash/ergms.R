detach("package:igraph") 
detach("package:network") 
detach("package:ergm") 
library(sna)
library(statnet)
library(ergm)




VAR <- VAR(sp[,-1], p = 2, type = c("none"), season = NULL, exogen = NULL)
temp <- genFEVD(est = VAR, n.ahead = 1, no.corr = F)  %>%  as.data.frame
temp[temp< 0.07]<-0
diag(temp) <- 0
temp <- temp*100
x <- unlist(temp)
hist(x)
# load directed weighted network as a network object
n.network <- network(temp, 
             vertex.attr = NULL, 
             vertex.attrnames = NULL, 
             directed = TRUE, 
             hyper = FALSE,# multiple head/tail vertices per edge
             multiple = FALSE,# multiplex edges 
             loops = FALSE,# self loop
             matrix.type = "adjacency",
             ignore.eval=FALSE,
             names.eval='weight'
)

what <- ergm(n.network ~ edges + edgecov(test0),
             response="weight", reference=~Binomial(1),verbose=F)  ##?

n.network <- as.network(temp, directed=TRUE, matrix.type="adjacency",
           ignore.eval=FALSE, names.eval="nominations" # Important!
)

# the adjacency matrix contained only zero and ones
n.network[,]
#the weight is contained in the object
get.edge.value(n.network, "nominations")

n.network %v% "debt" <- p.matrix[[i]][,1]
n.network %e% "lb" <- p.matrix[[i]] %>% as.matrix
#set.edge.attribute(n.network, "lb",   p.matrix[[i]])

test <- as.network(p.matrix[[i]],
                   matrix.type = "adjacency",
                   ignore.eval=FALSE,
                   names.eval='weight')



test <- as.network( matrix(sample(0:1,14*14,replace=T),14,14))

model0 <- ergm(n.network ~ edges + nodecov('debt'),
               verbose=TRUE)
model0
test0 <- p.matrix[[i]]
test03 <- p.matrix[[i]]/100
test0.2 <- p.matrix[[i]]
test0.20 <- p.matrix[[i]]/100
test0.7 <- p.matrix[[i]]
test0.2[test0 <= as.numeric(quantile(p.matrix[[i]], 0.2))] <- 0
test0.7[test0 <= as.numeric(quantile(p.matrix[[i]], 0.7))] <- 0

test <- ergm(n.network ~ edges + edgecov(test0),
     verbose=F)
summary(test)
ergm(n.network ~ edges + edgecov(test03),
     verbose=TRUE)
ergm(n.network ~ edges + edgecov(test0.2),
               verbose=TRUE)
ergm(n.network ~ edges + edgecov(test0.20),
     verbose=TRUE)
ergm(n.network ~ edges + edgecov(test0.7),
     verbose=TRUE)

model1 <- ergm(n.network ~ edges + dyadcov('lb'),
               verbose=TRUE)
model1

model2 <- ergm(n.network ~ edges)




summary(model)
ergm(n.network ~ edges + nodecov('debt'),
     verbose=TRUE)
model2
names(model2)
summary(model2)
rm(model2)
# how to explian the result







