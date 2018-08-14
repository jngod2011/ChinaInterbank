library("igraph")
g <- graph_from_literal( X1-X2-X3-X4, X1-X4, X2-X4, X4-X5 )
plot.igraph(g, vertex.color="white", edge.curved=0, edge.width = 2, vertex.size=30)

prices <- read.table("Flourprice.txt")
prices <- ts(prices, start=c(1972,8), end=c(1980,11), frequency=12, names = c("Buff","Minn","Kans"))

matplot(prices, type = "l") 
legend('top', c('Buff','Minn','Kans'),lty=c(1,2,3))


library(vars)
# NOT RUN {
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[4, 1] <- NA
## Estimation method scoring
SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL,
     max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
## Estimation method direct
SVAR(x = var.2c, estmethod = "direct", Amat = amat, Bmat = NULL,
     hessian = TRUE, method="BFGS") 
# }

library(dagR)
demo.dag1;
demo.dag1()
demo.dag2;
demo.dag2();

library(gRbase)
??dag


library(pcalg)
