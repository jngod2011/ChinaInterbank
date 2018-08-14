
m.data.vix <- (data.vix[-nrow(data.vix),] + data.vix[-1,]) * 0.5

growth_rate <- function(x){
  y <- data.frame(matrix(NA, ncol = NCOL(x) , nrow = NROW(x) - 1))
  for(i in 1:NCOL(x)){
    y[,i ] <- diff(x[, i])/x[-nrow(x), i]
  }
  y[is.na(y)] <- 0
  y[y == Inf] <- 1
  return(y)
}

g.m.data.vix <- growth_rate(m.data.vix)

data <- g.m.data.vix
names(data) <- names(data.vix)
suffStat <- list(C = cor(data), n = nrow(data))
pc.fit <- pc(suffStat, indepTest = gaussCItest, p = ncol(data),alpha = 0.01#,
             # maj.rule = TRUE,solve.confl = TRUE
)
library("Rgraphviz")
pc.fit@graph@nodes <- names(data)
names(pc.fit@graph@edgeL) <- names(data)
plot(pc.fit, main = "VIX")

dag <- as(pc.fit, "amat") 
amat <- matrix(rep(0, ncol(dag)*ncol(dag)), ncol(dag), nrow(dag))
amat[dag==1] <- NA
diag(amat) <- NA

library(vars)
sum(is.na(amat))
result.var <- VAR(data, p = 2, type = "const")
result.svar <- SVAR(x = result.var, estmethod = "direct", Amat = amat, Bmat = NULL,
                    hessian = TRUE, method="BFGS")
fevd.svar <- fevd(result.svar, n.ahead = 12)
m.fevd <- fevd.matrix(fevd.svar,n.ahead = 12)

result.fevd <- matrix(rep(NA,14,14),14,14)
result.fevd[-14,-14] <- m.fevd
result.fevd[-14,14] <- rowSums(m.fevd)-diag(m.fevd)
result.fevd[14,-14] <- colSums(m.fevd)-diag(m.fevd)
rownames(result.fevd) <- c(colnames(m.fevd),"OUT")
colnames(result.fevd) <- c(colnames(m.fevd),"IN")
result.fevd <- round(result.fevd,3)*100
result.fevd

g.m.fevd <- fevd_generalised(result.var, n.ahead = 12, normalize=TRUE)

result.fevd <- matrix(rep(NA,14,14),14,14)
result.fevd[-14,-14] <- g.m.fevd
result.fevd[-14,14] <- rowSums(g.m.fevd)-diag(g.m.fevd)
result.fevd[14,-14] <- colSums(g.m.fevd)-diag(g.m.fevd)
rownames(result.fevd) <- c(colnames(m.fevd),"OUT")
colnames(result.fevd) <- c(colnames(m.fevd),"IN")
result.fevd <- round(result.fevd,3)*100
