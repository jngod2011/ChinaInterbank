#################################################################
#time
#################################################################
timestart <- Sys.time()
set.seed(1)
bank14.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPA", "JEPF", "JEHB", "JEMS","JECM", "JECI", "JECITIC", "URNB", "URNJ", "URBJ" )

#################################################################
#setting
#################################################################
ID <- c("stock14")
inclusion.edgecov <- c("loan")
inclusion.nodecov <- c("ass", "dbt","sty")
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
#################################################################
#setting
#################################################################
set <- " "
triangle <- "all"
directed <- T
BidType <- NULL
filename <- paste0(as.character(directed),"_",triangle,"_",BidType, ID)
MCMLE.maxit <- 100
trans <- c("y","loan")
inclusion.cyclicalweights <- T
inclusion.mutual <- T
inclusion.nodecovar <- F
inclusion.nodeicovar <-F
inclusion.nodeocovar <- F
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- T
#################################################################
#library
#################################################################
library(knitr)
library(devtools)
library(dplyr)
library(dygraphs)
library(ergm.count)
library(ergm)
library(frequencyConnectedness)
library(gcdnet)
library(ggplot2)
library(glmnet)
library(htmlwidgets)
#library(igraph)
library(latentnet)
library(lubridate)
library(magrittr)
library(miscTools)
library(msaenet)
#library(networkD3)
library(NetworkRiskMeasures)
library(plyr)
library(rbokeh)
library(readxl)
library(reshape2)
library(tcltk)
#library(ts)
library(tsDyn)
library(TTR)
library(vars)
library(VIM) 
library(xlsx)
library(xtable)
library(xts)
library(GERGM)
library(stargazer)
source("code/function.R")
source("code/functionInterbank.R")
source("code/functionNewRMB.R")
#################################################################
#load data
#################################################################
#stock
n.sp <- bank14.abbr
n.g.sp <- paste0(n.sp,".g")
#shibor
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
#loan
p.matrix <- list()
for (i in 1:8) {#2009:2016
  p.matrix[[i]] <- xlsx::read.xlsx(file="data/bank14/loan14.xlsx", sheetName = paste0("loan",i)) %>% as.data.frame
  rownames(p.matrix[[i]]) <- p.matrix[[i]][,1]
  p.matrix[[i]] <-  p.matrix[[i]][,-1]
}
loan.network <- p.matrix
#wind###########################################################################################
raw.wind <- list()
for (i in 1:10) {#2007:2016
  raw.wind[[i]] <- xlsx::read.xlsx(file="data/bank14/raw.wind14.xlsx", sheetName = paste0("wind",i)) %>% as.data.frame
  rownames(raw.wind[[i]]) <- raw.wind[[i]][,1]
  raw.wind[[i]] <-  raw.wind[[i]][,-1]
}
#load forestData###########################################################################################
data <- read.csv(file = "data/bank14/ForestData14.csv")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
Date <- index(data) %>% as.character

is.higher <- data[,n.is.higher]
g.sp <- data[,n.g.sp]
sp <- data[,n.sp]
#network###########################################################################################
#var.myl.fevd <- list();var.myl.gir <- list()
#vecm.myl.fevd <- list();
vecm.myl.gir <- list()
y.period <- c(#"2006-12-31",
  #"2007-12-31",
  "2008-12-31",
  "2009-12-31",
  "2010-12-31",
  "2011-12-31",
  "2012-12-31",
  "2013-12-31",
  "2014-12-31",
  "2015-12-31",
  "2016-12-31",
  "2017-12-31",
  "2018-12-31"
)
# the sample in 2018 is too small
for (t in 1:(length(y.period)-2)) {
  #temp <- dy.VAR.FEVD(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead =1, mode= "fix", span ="yearly")
  #var.myl.fevd[[t]] <- matrix(temp$fevd.matrix %>% unlist,14,14) #%>% t
  #colnames(var.myl.fevd[[t]]) <- bank14.abbr;rownames(var.myl.fevd[[t]]) <- bank14.abbr
  
  #temp <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead =1, mode= "fix", span ="yearly")
  #var.myl.gir[[t]] <- matrix(temp$gir.matrix %>% unlist,14,14) #%>% t
  #colnames(var.myl.gir[[t]]) <- bank14.abbr;rownames(var.myl.gir[[t]]) <- bank14.abbr
  
  #temp <- dy.VECM.FEVD(data = sp[paste0(y.period[t],"/",y.period[t+1])], group = names(sp), n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.myl.fevd[[t]] <-matrix(temp$fevd.matrix %>% unlist,14,14) #%>% t
  #colnames(vecm.myl.fevd[[t]]) <- bank14.abbr;rownames(vecm.myl.fevd[[t]]) <- bank14.abbr
  
  temp <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], group = names(sp), n.ahead = 1, span = "yearly")
  vecm.myl.gir[[t]] <- matrix(temp$gir.matrix %>% unlist,14,14) #%>% t
  colnames(vecm.myl.gir[[t]]) <- bank14.abbr;rownames(vecm.myl.gir[[t]] ) <- bank14.abbr
}

#################################################################
# basic setting
dyErgm.result <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, 
                                   inclusion.edgecov = inclusion.edgecov,
                                   inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov,
                                   inclusion.absdiff = inclusion.absdiff,
                                   inclusion.cyclicalweights = inclusion.cyclicalweights,
                                   inclusion.mutual = inclusion.mutual,
                                   inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar,
                                   inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar,
                                   Date = network.y$Date, 
                                   trans = trans, 
                                   BidType = BidType,
                                   MCMLE.maxit = MCMLE.maxit,
                                   triangle = triangle,
                                   directed = directed,
                                   tab = T,
                                   wind = raw.wind[3:10]
)

dyErgm.result$scale;
result <- dyErgm.result$tab.ergm$coef
result[dyErgm.result$tab.ergm$pvalue > 0.1 | is.na(dyErgm.result$tab.ergm$pvalue)] <- NA
xlsx::write.xlsx(result, file=paste0(filename,".xlsx"), sheetName="nodecov",  col.names = T, showNA = F)

stargazer(dyErgm.result$result.vergm.l, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), 
          column.labels = c(2009:2016) %>% as.character, 
          covariate.labels = c("cns",sub(pattern = "coef.", replacement = "",  names(result))), 
          notes = "", 
          digits = 2, 
          label = paste0("tab:",filename), 
          out = paste0(filename,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

