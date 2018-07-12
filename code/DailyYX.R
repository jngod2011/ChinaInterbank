#time###########################################################################################
timestart <- Sys.time()
#####################################################################
ID <- "2"
network.y.type <- "VAR"#shor long all VECM VAR ON W1 W2 M1 M3 M6 M9 Y1
inclusion.edgecov <- c("short")#c("rolling")
trans <- c("y", "ON", "W1", "W2", "M1", "M3", "M6", "M9", "Y1", "short", "long", "all")
#setting###########################################################################################
filename <- paste0(network.y.type,"_", ID)
set <- filename
triangle <- "all"
directed <- T
BidType <- "aenet"
MCMLE.maxit <- 40
inclusion.cyclicalweights <- F
inclusion.mutual <- F
inclusion.nodecovar <- F
inclusion.nodeicovar <-F
inclusion.nodeocovar <- F
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- F
inclusion.nodecov <- NULL
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
###########################################################################################
#library
###########################################################################################
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
source("code/function.R")
source("code/functionInterbank.R")
source("code/functionNewRMB.R")
###########################################################################################
#shibor
###########################################################################################
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
data <- read.csv(file = "data/bank10/ForestData.csv")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
Date <- index(data) %>% as.character;Date <- Date[-c(1:250)]
is.higher <- data[,n.is.higher][-c(1:250),]
###########################################################################################
# network.y 
###########################################################################################
if(TRUE){
  load(file="data/Rdata/DailyShiborBidAenet.Rdata")
  if(network.y.type=="short"){
    network.y <- aenet.myl.short
  }
  if(network.y.type=="long"){
    network.y <- aenet.myl.long
  }
  if(network.y.type=="all"){
    network.y <- aenet.myl.all
  }
  if(network.y.type=="ON"){
    network.y <- aenet.myl.ON
  }
  if(network.y.type=="W1"){
    network.y <- aenet.myl.W1
  }
  if(network.y.type=="W2"){
    network.y <- aenet.myl.W2
  }
  if(network.y.type=="M1"){
    network.y <- aenet.myl.M1
  }
  if(network.y.type=="M3"){
    network.y <- aenet.myl.M3
  }
  if(network.y.type=="M6"){
    network.y <- aenet.myl.M6
  }
  if(network.y.type=="M9"){
    network.y <- aenet.myl.M9
  }
  if(network.y.type=="Y1"){
    network.y <- aenet.myl.Y1
  }
  
  if(network.y.type=="VAR"){
    load(file = "data/Rdata/latex_daily.var.gir.Rdata")
    daily.var.gir <- list()
    for (i in 1:length(daily.var.gir0)) {
      GIR <- array(c(daily.var.gir0[[i]],
                     daily.var.gir1[[i]],
                     daily.var.gir2[[i]],
                     daily.var.gir3[[i]],
                     daily.var.gir4[[i]],
                     daily.var.gir5[[i]]),dim = c(10,10,6))
      daily.var.gir[[i]] <- weighted_gir(GIR, divided=1)$weighted.matrix
    }
    network.y <- daily.var.gir
  }
  
  if(network.y.type=="VECM"){
    load(file = "data/Rdata/latex_daily.vecm.gir.Rdata")
    daily.vecm.gir <- list()
    for (i in 1:length(daily.vecm.gir0)) {
      GIR <- array(c(daily.vecm.gir0[[i]],
                     daily.vecm.gir1[[i]],
                     daily.vecm.gir2[[i]],
                     daily.vecm.gir3[[i]],
                     daily.vecm.gir4[[i]],
                     daily.vecm.gir5[[i]]),dim = c(10,10,6))
      daily.vecm.gir[[i]] <- weighted_gir(GIR, divided=1)$weighted.matrix
    }
    network.y <- daily.vecm.gir
  }
}
###########################################################################################
# ergm 
###########################################################################################
dyErgm.result <- dyCoefErgm.yearly(data = network.y, set = set, 
                                   inclusion.edgecov = inclusion.edgecov,
                                   inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov,
                                   inclusion.absdiff = inclusion.absdiff,
                                   inclusion.cyclicalweights = inclusion.cyclicalweights,
                                   inclusion.mutual = inclusion.mutual,
                                   inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar,
                                   inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar,
                                   Date = Date,
                                   trans = trans, 
                                   BidType = BidType,
                                   MCMLE.maxit = MCMLE.maxit,
                                   triangle = triangle,
                                   directed = directed,
                                   tab = F,fig = T,
                                   is.higher = is.higher,
                                   csv = T
                                   
)


