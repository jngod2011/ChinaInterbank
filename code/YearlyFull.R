#time###########################################################################################
timestart <- Sys.time()
#####################################################################
ID <- c("full")
#setting###########################################################################################
set <- " "
triangle <- "all"
directed <- T
BidType <- "aenet"
filename <- paste0(as.character(directed),"_",triangle,"_",BidType, ID)
MCMLE.maxit <- 100
trans <- c("y","ON","W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.cyclicalweights <- T
inclusion.mutual <- T
inclusion.nodecovar <- F
inclusion.nodeicovar <-F
inclusion.nodeocovar <- F
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- T
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
bank10.abre <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
#stock price###########################################################################################
#stock price###########################################################################################
## stock price data
n.sp <- bank10.abre
n.g.sp <- paste0(n.sp,".g")
#bond###########################################################################################
# load bond
n.bond <- c("TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
#shibor###########################################################################################
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
#shibor bid###########################################################################################
# load shibor bid data
n.all.bid.ON <- paste0(bank10.abre,".ON")
n.all.bid.W1 <- paste0(bank10.abre,".W1")
n.all.bid.W2 <- paste0(bank10.abre,".W2")
n.all.bid.M1 <- paste0(bank10.abre,".M1")
n.all.bid.M3 <- paste0(bank10.abre,".M3")
n.all.bid.M6 <- paste0(bank10.abre,".M6")
n.all.bid.M9 <- paste0(bank10.abre,".M9")
n.all.bid.Y1 <- paste0(bank10.abre,".Y1")
#loan###########################################################################################
p.matrix <- list()
for (i in 1:8) {#2009:2016
  p.matrix[[i]] <- xlsx::read.xlsx(file="data/loan.xlsx", sheetName = paste0("loan",i)) %>% as.data.frame
  rownames(p.matrix[[i]]) <- p.matrix[[i]][,1]
  p.matrix[[i]] <-  p.matrix[[i]][,-1]
}
loan.network <- p.matrix
#wind###########################################################################################
raw.wind <- list()
for (i in 1:10) {#2007:2016
  raw.wind[[i]] <- xlsx::read.xlsx(file="data/raw.wind.xlsx", sheetName = paste0("wind",i)) %>% as.data.frame
  rownames(raw.wind[[i]]) <- raw.wind[[i]][,1]
  raw.wind[[i]] <-  raw.wind[[i]][,-1]
}
raw.wind <- F.fill.up.NAs(raw.wind)

mean.wind <- raw.wind[[3]]
for (i in 4:10) {
  mean.wind <- mean.wind + raw.wind[[i]]
}
mean.wind <- list(mean.wind/8)
#load forestData###########################################################################################
data <- read.csv(file = "data/ForestData.csv")
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
var.myl.fevd <- list();var.myl.gir <- list()
vecm.myl.fevd <- list();vecm.myl.gir <- list()
y.period <- c(#"2006-12-31",
  #"2007-12-31",
  "2008-12-31",
  "2018-12-31"
)
# the sample in 2018 is too small
for (t in 1:1) {
  temp <- aenet.fix(data = all.bid.ON, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef
  aenet.myl.ON[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.ON[[t]]) <- bank10.abre;rownames(aenet.myl.ON[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.W1, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.W1[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.W1[[t]]) <- bank10.abre;rownames(aenet.myl.W1[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.W2, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.W2[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.W2[[t]]) <- bank10.abre;rownames(aenet.myl.W2[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.M1, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M1[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M1[[t]]) <- bank10.abre;rownames(aenet.myl.M1[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.M3, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M3[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M3[[t]]) <- bank10.abre;rownames(aenet.myl.M3[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.M6, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M6[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M6[[t]]) <- bank10.abre;rownames(aenet.myl.M6[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.M9, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.M9[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.M9[[t]]) <- bank10.abre;rownames(aenet.myl.M9[[t]]) <- bank10.abre
  
  temp <- aenet.fix(data = all.bid.Y1, fix = "yearly", start.point = y.period[t], end.point = y.period[t+1])$Coef 
  aenet.myl.Y1[[t]] <- matrix(temp %>% unlist,25,25)[1:10,1:10] #%>% t
  colnames(aenet.myl.Y1[[t]]) <- bank10.abre;rownames(aenet.myl.Y1[[t]]) <- bank10.abre
  
  #temp <- dy.VAR.FEVD(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead =1, mode= "fix", span ="yearly")
  #var.myl.fevd[[t]] <- matrix(temp$fevd.matrix %>% unlist,10,10) #%>% t
  #colnames(var.myl.fevd[[t]]) <- bank10.abre;rownames(var.myl.fevd[[t]]) <- bank10.abre
  
  #temp <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead =1, mode= "fix", span ="yearly")
  #var.myl.gir[[t]] <- matrix(temp$gir.matrix %>% unlist,10,10) #%>% t
  #colnames(var.myl.gir[[t]]) <- bank10.abre;rownames(var.myl.gir[[t]]) <- bank10.abre
  
  #temp <- dy.VECM.FEVD(data = sp[paste0(y.period[t],"/",y.period[t+1])], group = names(sp), n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  #vecm.myl.fevd[[t]] <-matrix(temp$fevd.matrix %>% unlist,10,10) #%>% t
  #colnames(vecm.myl.fevd[[t]]) <- bank10.abre;rownames(vecm.myl.fevd[[t]]) <- bank10.abre
  
  temp <- dy.VECM.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], group = names(sp), n.ahead = 1, mode = "fix", span = "yearly", keep.vecm = TRUE,rank = 9)
  vecm.myl.gir[[t]] <- matrix(temp$gir.matrix %>% unlist,10,10) #%>% t
  colnames(vecm.myl.gir[[t]]) <- bank10.abre;rownames(vecm.myl.gir[[t]] ) <- bank10.abre
}
######################################################################
# 1
inclusion.edgecov <- c("loan")
inclusion.nodecov <- NULL
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
dyErgm.result1 <- dyCoefErgm.yearly(data = vecm.myl.gir, set = set, inclusion.edgecov = inclusion.edgecov, inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov, inclusion.absdiff = inclusion.absdiff, inclusion.cyclicalweights = inclusion.cyclicalweights, inclusion.mutual = inclusion.mutual, inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar, inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar, Date = network.y$Date, trans = trans, BidType = BidType, MCMLE.maxit = MCMLE.maxit, triangle = triangle, directed = directed, tab = T, wind = mean.wind)
# 2
inclusion.edgecov <- c("ON","W1","W2","M1","M3","M6","M9","Y1")
inclusion.nodecov <- NULL
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
dyErgm.result2 <- dyCoefErgm.yearly(data = vecm.myl.gir, set = set, inclusion.edgecov = inclusion.edgecov, inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov, inclusion.absdiff = inclusion.absdiff, inclusion.cyclicalweights = inclusion.cyclicalweights, inclusion.mutual = inclusion.mutual, inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar, inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar, Date = network.y$Date, trans = trans, BidType = BidType, MCMLE.maxit = MCMLE.maxit, triangle = triangle, directed = directed, tab = T, wind = mean.wind)
# 3
inclusion.edgecov <- c("ON","W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.nodecov <- NULL
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
dyErgm.result3 <- dyCoefErgm.yearly(data = vecm.myl.gir, set = set, inclusion.edgecov = inclusion.edgecov, inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov, inclusion.absdiff = inclusion.absdiff, inclusion.cyclicalweights = inclusion.cyclicalweights, inclusion.mutual = inclusion.mutual, inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar, inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar, Date = network.y$Date, trans = trans, BidType = BidType, MCMLE.maxit = MCMLE.maxit, triangle = triangle, directed = directed, tab = T, wind = mean.wind)
# 4
inclusion.edgecov <- c("ON","W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.nodecov <- NULL
inclusion.nodeocov <- c("ass","dbt","sty")
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
dyErgm.result4 <- dyCoefErgm.yearly(data = vecm.myl.gir, set = set, inclusion.edgecov = inclusion.edgecov, inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov, inclusion.absdiff = inclusion.absdiff, inclusion.cyclicalweights = inclusion.cyclicalweights, inclusion.mutual = inclusion.mutual, inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar, inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar, Date = network.y$Date, trans = trans, BidType = BidType, MCMLE.maxit = MCMLE.maxit, triangle = triangle, directed = directed, tab = T, wind = mean.wind)
# 5
inclusion.edgecov <- c("ON","W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.nodecov <- NULL
inclusion.nodeocov <- c("ass","dbt","sty")
inclusion.nodeicov <- NULL
inclusion.absdiff <- c("brr","lnd")
dyErgm.result5 <- dyCoefErgm.yearly(data = vecm.myl.gir, set = set, inclusion.edgecov = inclusion.edgecov, inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov, inclusion.absdiff = inclusion.absdiff, inclusion.cyclicalweights = inclusion.cyclicalweights, inclusion.mutual = inclusion.mutual, inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar, inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar, Date = network.y$Date, trans = trans, BidType = BidType, MCMLE.maxit = MCMLE.maxit, triangle = triangle, directed = directed, tab = T, wind = mean.wind)
#################################################################

result <- list(dyErgm.result1$result.vergm.l,
               dyErgm.result2$result.vergm.l,
               dyErgm.result3$result.vergm.l,
               dyErgm.result4$result.vergm.l,
               dyErgm.result5$result.vergm.l
           )
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), 
          column.labels = c(1:5) %>% as.character, 
          covariate.labels = c("cns",sub(pattern = "coef.", replacement = "",  names(result))), 
          notes = "", 
          digits = 2, 
          label = paste0("tab:",filename), 
          out = paste0(filename,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

