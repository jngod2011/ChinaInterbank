#time###########################################################################################
timestart <- Sys.time()
#####################################################################
ID <- c("ST")
inclusion.edgecov <- c("short", "long","loan")
inclusion.nodecov <- c("ass", "dbt","sty")
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
#setting###########################################################################################
set <- " "
network.y.type <- "VECM.GIR"#"VECM.GIR"#VAR.GIR#VECM.GIR.OR
triangle <- "all"
directed <- T
BidType <- "aenet"
filename <- paste0(as.character(directed),"_",triangle,"_",BidType, ID)
MCMLE.maxit <- 20
trans <- c("y","ON","W1","W2","M1","M3","M6","M9","Y1","short","long")
inclusion.cyclicalweights <- F
inclusion.mutual <- F
inclusion.nodecovar <- F
inclusion.nodeicovar <-F
inclusion.nodeocovar <- F
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- F
#library###########################################################################################
# load libraries
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
#specify 10 banks###########################################################################################
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
#stock price###########################################################################################
## stock price data
n.sp <- bank10.abbr
n.g.sp <- paste0(n.sp,".g")
#bond###########################################################################################
# load bond
n.bond <- c("TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
#shibor###########################################################################################
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
#shibor bid###########################################################################################
# load shibor bid data
n.all.bid.ON <- paste0(bank10.abbr,".ON")
n.all.bid.W1 <- paste0(bank10.abbr,".W1")
n.all.bid.W2 <- paste0(bank10.abbr,".W2")
n.all.bid.M1 <- paste0(bank10.abbr,".M1")
n.all.bid.M3 <- paste0(bank10.abbr,".M3")
n.all.bid.M6 <- paste0(bank10.abbr,".M6")
n.all.bid.M9 <- paste0(bank10.abbr,".M9")
n.all.bid.Y1 <- paste0(bank10.abbr,".Y1")
#loan###########################################################################################
p.matrix <- list()
for (i in 1:8) {#2009:2016
  p.matrix[[i]] <- xlsx::read.xlsx(file="data/bank10/loan.xlsx", sheetName = paste0("loan",i)) %>% as.data.frame
  rownames(p.matrix[[i]]) <- p.matrix[[i]][,1]
  p.matrix[[i]] <-  p.matrix[[i]][,-1]
}
loan.network <- p.matrix
#wind###########################################################################################
raw.wind <- list()
for (i in 1:10) {#2007:2016
  raw.wind[[i]] <- xlsx::read.xlsx(file="data/bank10/raw.wind.xlsx", sheetName = paste0("wind",i)) %>% as.data.frame
  rownames(raw.wind[[i]]) <- raw.wind[[i]][,1]
  raw.wind[[i]] <-  raw.wind[[i]][,-1]
}
#load forestData###########################################################################################
data <- read.csv(file = "data/bank10/ForestData.csv")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
Date <- index(data) %>% as.character

is.higher <- data[,n.is.higher]
g.sp <- data[,n.g.sp]
sp <- data[,n.sp]
bond <- data[,n.bond]
all.bid.ON <- data[,n.all.bid.ON]
all.bid.W1 <- data[,n.all.bid.W1]
all.bid.W2 <- data[,n.all.bid.W2]
all.bid.M1 <- data[,n.all.bid.M1]
all.bid.M3 <- data[,n.all.bid.M3]
all.bid.M6 <- data[,n.all.bid.M6]
all.bid.M9 <- data[,n.all.bid.M9]
all.bid.Y1 <- data[,n.all.bid.Y1]
#network###########################################################################################
aenet.myl.ON <- list();aenet.myl.W1 <- list()
aenet.myl.W2 <- list();aenet.myl.M1 <- list()
aenet.myl.M3 <- list();aenet.myl.M6 <- list()
aenet.myl.M9 <- list();aenet.myl.Y1 <- list()
aenet.myl.short <- list();aenet.myl.long <- list()
var.myl.fevd <- list();var.myl.gir <- list()
vecm.myl.fevd <- list();vecm.myl.gir <- list()
network.y <- list()
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
  temp <- aenet.fix(data = all.bid.ON, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef
  aenet.myl.ON[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.ON[[t]]) <- bank10.abbr;rownames(aenet.myl.ON[[t]]) <- bank10.abbr
  aenet.myl.ON <- lapply(aenet.myl.ON, replaceNA0)
  
  temp <- aenet.fix(data = all.bid.W1, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.W1[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.W1[[t]]) <- bank10.abbr;rownames(aenet.myl.W1[[t]]) <- bank10.abbr
  aenet.myl.W1 <- lapply(aenet.myl.W1, replaceNA0)
  
  temp <- aenet.fix(data = all.bid.W2, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.W2[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.W2[[t]]) <- bank10.abbr;rownames(aenet.myl.W2[[t]]) <- bank10.abbr
  aenet.myl.W2 <- lapply(aenet.myl.W2, replaceNA0)
  
  temp <- aenet.fix(data = all.bid.M1, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M1[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M1[[t]]) <- bank10.abbr;rownames(aenet.myl.M1[[t]]) <- bank10.abbr
  aenet.myl.M1 <- lapply(aenet.myl.M1, replaceNA0)
  
  
  temp <- aenet.fix(data = all.bid.M3, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M3[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M3[[t]]) <- bank10.abbr;rownames(aenet.myl.M3[[t]]) <- bank10.abbr
  aenet.myl.M3 <- lapply(aenet.myl.M3, replaceNA0)
  
  
  temp <- aenet.fix(data = all.bid.M6, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M6[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M6[[t]]) <- bank10.abbr;rownames(aenet.myl.M6[[t]]) <- bank10.abbr
  aenet.myl.M6 <- lapply(aenet.myl.M6, replaceNA0)
  
  
  temp <- aenet.fix(data = all.bid.M9, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M9[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M9[[t]]) <- bank10.abbr;rownames(aenet.myl.M9[[t]]) <- bank10.abbr
  aenet.myl.M9 <- lapply(aenet.myl.M9, replaceNA0)
  
  temp <- aenet.fix(data = all.bid.Y1, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.Y1[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.Y1[[t]]) <- bank10.abbr;rownames(aenet.myl.Y1[[t]]) <- bank10.abbr
  aenet.myl.Y1 <- lapply(aenet.myl.Y1, replaceNA0)
  
  aenet.myl.short[[t]] <- (aenet.myl.ON[[t]] + aenet.myl.W1[[t]] + aenet.myl.W2[[t]] + aenet.myl.M1[[t]])/4
  aenet.myl.long[[t]] <- (aenet.myl.M3[[t]] + aenet.myl.M6[[t]] + aenet.myl.M9[[t]] + aenet.myl.Y1[[t]])/4
  
  #temp <- dy.VAR.FEVD(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead =1, mode= "fix", span ="yearly")
  #var.myl.fevd[[t]] <- matrix(temp$fevd.matrix %>% unlist,10,10) #%>% t
  #colnames(var.myl.fevd[[t]]) <- bank10.abbr;rownames(var.myl.fevd[[t]]) <- bank10.abbr
  
  #temp <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead =1, mode= "fix", span ="yearly")
  #var.myl.gir[[t]] <- matrix(temp$gir.matrix %>% unlist,10,10) #%>% t
  #colnames(var.myl.gir[[t]]) <- bank10.abbr;rownames(var.myl.gir[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = sp[paste0(y.period[t],"/",y.period[t+1])], group = names(sp), n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.myl.fevd[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) #%>% t
  #colnames(vecm.myl.fevd[[t]]) <- bank10.abbr;rownames(vecm.myl.fevd[[t]]) <- bank10.abbr
  
  if(network.y.type == "VECM.GIR.OR"){
    temp <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], group = names(sp), n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
    network.y[[t]] <- matrix(temp$gir.matrix %>% unlist,10,10) #%>% t
    colnames(network.y[[t]]) <- bank10.abbr;rownames(network.y[[t]] ) <- bank10.abbr
  }
  if(network.y.type == "VECM.GIR"){
    vecm.gir0 <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 0, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir0 <- matrix(data = unlist(vecm.gir0),nrow =10,ncol = 10)
    vecm.gir1 <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 1, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir1 <- matrix(data = unlist(vecm.gir1),nrow =10,ncol = 10)
    vecm.gir2 <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 2, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir2 <- matrix(data = unlist(vecm.gir2),nrow =10,ncol = 10)
    vecm.gir3 <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 3, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir3 <- matrix(data = unlist(vecm.gir3),nrow =10,ncol = 10)
    vecm.gir4 <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 4, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir4 <- matrix(data = unlist(vecm.gir4),nrow =10,ncol = 10)
    vecm.gir5 <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 5, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir5 <- matrix(data = unlist(vecm.gir5),nrow =10,ncol = 10)
    GIR <- array(c(vecm.gir0,
                   vecm.gir1,
                   vecm.gir2,
                   vecm.gir3,
                   vecm.gir4,
                   vecm.gir5), dim = c(10,10,6))
    temp <- weighted_gir(GIR, divided=1)$weighted.matrix
    temp <- matrix(temp %>% unlist,10,10) #%>% t
    network.y[[t]] <- temp
    colnames(network.y[[t]]) <- bank10.abbr;rownames(network.y[[t]] ) <- bank10.abbr
  print(t)
  print(network.y.type)
  print("############################")
    }
  if(network.y.type == "VAR.GIR"){
    var.gir0 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 0, span = "yearly", keep.var = T, rank = 4)[[1]];var.gir0 <- matrix(data = unlist(var.gir0),nrow =10,ncol = 10)
    var.gir1 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 1, span = "yearly", keep.var = T, rank = 4)[[1]];var.gir1 <- matrix(data = unlist(var.gir1),nrow =10,ncol = 10)
    var.gir2 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 2, span = "yearly", keep.var = T, rank = 4)[[1]];var.gir2 <- matrix(data = unlist(var.gir2),nrow =10,ncol = 10)
    var.gir3 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 3, span = "yearly", keep.var = T, rank = 4)[[1]];var.gir3 <- matrix(data = unlist(var.gir3),nrow =10,ncol = 10)
    var.gir4 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 4, span = "yearly", keep.var = T, rank = 4)[[1]];var.gir4 <- matrix(data = unlist(var.gir4),nrow =10,ncol = 10)
    var.gir5 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 5, span = "yearly", keep.var = T, rank = 4)[[1]];var.gir5 <- matrix(data = unlist(var.gir5),nrow =10,ncol = 10)
    GIR <- array(c(var.gir0,
                   var.gir1,
                   var.gir2,
                   var.gir3,
                   var.gir4,
                   var.gir5), dim = c(10,10,6))
    temp <- weighted_gir(GIR, divided=1)$weighted.matrix
    temp <- matrix(temp %>% unlist,10,10) #%>% t
    network.y[[t]] <- temp
    colnames(network.y[[t]]) <- bank10.abbr;rownames(network.y[[t]] ) <- bank10.abbr
  }
}
#network###########################################################################################
# vecm gir and fevd for shibor bid
#vecm.gir.myl.ON <- list(); vecm.gir.myl.W1 <- list()
#vecm.gir.myl.W2 <- list(); vecm.gir.myl.M1 <- list()
#vecm.gir.myl.M3 <- list(); vecm.gir.myl.M6 <- list()
#vecm.gir.myl.M9 <- list(); vecm.gir.myl.Y1 <- list()

#vecm.fevd.myl.ON <- list(); vecm.fevd.myl.W1 <- list()
#vecm.fevd.myl.W2 <- list(); vecm.fevd.myl.M1 <- list()
#vecm.fevd.myl.M3 <- list(); vecm.fevd.myl.M6 <- list()
#vecm.fevd.myl.M9 <- list(); vecm.fevd.myl.Y1 <- list()

#for (t in 1:(length(y.period)-2)) {
  #temp <- dy.VECM.FEVD(data = all.bid.ON[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.ON[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.ON[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.ON[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.W1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.W1[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.W1[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.W1[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.W2[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.W2[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.W2[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.W2[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.M1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.M1[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.M1[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.M1[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.M3[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.M3[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.M3[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.M3[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.M6[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.M6[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.M6[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.M6[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.M9[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.M9[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.M9[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.M9[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.FEVD(data = all.bid.Y1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.fevd.myl.Y1[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
  #colnames(vecm.fevd.myl.Y1[[t]]) <- bank10.abbr;rownames(vecm.fevd.myl.Y1[[t]]) <- bank10.abbr
  
  ################################################################################################################################################################
  #temp <- dy.VECM.GIR(data = all.bid.ON[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.ON[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.ON[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.ON[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.W1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.W1[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.W1[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.W1[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.W2[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.W2[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.W2[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.W2[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.M1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.M1[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.M1[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.M1[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.M3[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.M3[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.M3[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.M3[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.M6[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.M6[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.M6[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.M6[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.M9[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.M9[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.M9[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.M9[[t]]) <- bank10.abbr
  
  #temp <- dy.VECM.GIR(data = all.bid.Y1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.gir.myl.Y1[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
  #colnames(vecm.gir.myl.Y1[[t]]) <- bank10.abbr;rownames(vecm.gir.myl.Y1[[t]]) <- bank10.abbr
#}

#var.gir.myl.ON <- list(); var.gir.myl.W1 <- list()
#var.gir.myl.W2 <- list(); var.gir.myl.M1 <- list()
#var.gir.myl.M3 <- list(); var.gir.myl.M6 <- list()
#var.gir.myl.M9 <- list(); var.gir.myl.Y1 <- list()

#var.fevd.myl.ON <- list(); var.fevd.myl.W1 <- list()
#var.fevd.myl.W2 <- list(); var.fevd.myl.M1 <- list()
#var.fevd.myl.M3 <- list(); var.fevd.myl.M6 <- list()
#var.fevd.myl.M9 <- list(); var.fevd.myl.Y1 <- list()

#for (t in 1:(length(y.period)-2)) {
#  temp <- dy.VAR.FEVD(data = all.bid.ON[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.ON[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.ON[[t]]) <- bank10.abbr;rownames(var.fevd.myl.ON[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.W1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.W1[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.W1[[t]]) <- bank10.abbr;rownames(var.fevd.myl.W1[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.W2[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.W2[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.W2[[t]]) <- bank10.abbr;rownames(var.fevd.myl.W2[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.M1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.M1[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.M1[[t]]) <- bank10.abbr;rownames(var.fevd.myl.M1[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.M3[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.M3[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.M3[[t]]) <- bank10.abbr;rownames(var.fevd.myl.M3[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.M6[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.M6[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.M6[[t]]) <- bank10.abbr;rownames(var.fevd.myl.M6[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.M9[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.M9[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.M9[[t]]) <- bank10.abbr;rownames(var.fevd.myl.M9[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.FEVD(data = all.bid.Y1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.fevd.myl.Y1[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) 
#  colnames(var.fevd.myl.Y1[[t]]) <- bank10.abbr;rownames(var.fevd.myl.Y1[[t]]) <- bank10.abbr
#  
################################################################################################################################################################
#  temp <- dy.VAR.GIR(data = all.bid.ON[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.ON[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.ON[[t]]) <- bank10.abbr;rownames(var.gir.myl.ON[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.W1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.W1[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.W1[[t]]) <- bank10.abbr;rownames(var.gir.myl.W1[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.W2[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.W2[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.W2[[t]]) <- bank10.abbr;rownames(var.gir.myl.W2[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.M1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.M1[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.M1[[t]]) <- bank10.abbr;rownames(var.gir.myl.M1[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.M3[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.M3[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.M3[[t]]) <- bank10.abbr;rownames(var.gir.myl.M3[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.M6[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.M6[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.M6[[t]]) <- bank10.abbr;rownames(var.gir.myl.M6[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.M9[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.M9[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.M9[[t]]) <- bank10.abbr;rownames(var.gir.myl.M9[[t]]) <- bank10.abbr
#  
#  temp <- dy.VAR.GIR(data = all.bid.Y1[paste0(y.period[t],"/",y.period[t+1])][,1:10], n.ahead = 1, mode = "fix", span = "yearly")
#  var.gir.myl.Y1[[t]] <-matrix(temp$gir.matrix %>% unlist,10,10) 
#  colnames(var.gir.myl.Y1[[t]]) <- bank10.abbr;rownames(var.gir.myl.Y1[[t]]) <- bank10.abbr
#}



#################################################################
# basic setting
dyErgm.result <- dyCoefErgm.yearly(data = network.y[1:2], set = set, 
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

