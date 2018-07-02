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
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
n.sp <- bank10.abbr
n.bond <- c("TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
n.all.bid.ON <- paste0(bank10.abbr,".ON")
n.all.bid.W1 <- paste0(bank10.abbr,".W1")
n.all.bid.W2 <- paste0(bank10.abbr,".W2")
n.all.bid.M1 <- paste0(bank10.abbr,".M1")
n.all.bid.M3 <- paste0(bank10.abbr,".M3")
n.all.bid.M6 <- paste0(bank10.abbr,".M6")
n.all.bid.M9 <- paste0(bank10.abbr,".M9")
n.all.bid.Y1 <- paste0(bank10.abbr,".Y1")

data <- read.csv(file = "data/bank10/ForestData.csv")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
Date <- index(data) %>% as.character

is.higher <- data[,n.is.higher]
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

#aenet.myl.ON <- aenet.fix(data = merge(all.bid.ON,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.W1 <- aenet.fix(data = merge(all.bid.W1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.W2 <- aenet.fix(data = merge(all.bid.W2,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.M1 <- aenet.fix(data = merge(all.bid.M1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.M3 <- aenet.fix(data = merge(all.bid.M3,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.M6 <- aenet.fix(data = merge(all.bid.M6,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.M9 <- aenet.fix(data = merge(all.bid.M9,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#aenet.myl.Y1 <- aenet.fix(data = merge(all.bid.Y1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
#save(aenet.myl.ON, aenet.myl.W1, aenet.myl.W2, aenet.myl.M1, aenet.myl.M3, aenet.myl.M6, aenet.myl.Y1,file="data/Rdata/DailyShiborBidAenet.Rdata")  
#aenet.dygraph(aenet.myl.ON)

load(file="data/Rdata/DailyShiborBidAenet.Rdata")  
aenet.dygraph(aenet.myl.ON)
aenet.dygraph(aenet.myl.W1)
aenet.dygraph(aenet.myl.W2)
aenet.dygraph(aenet.myl.M1)
aenet.dygraph(aenet.myl.M3)
aenet.dygraph(aenet.myl.M6)
aenet.dygraph(aenet.myl.M9)
aenet.dygraph(aenet.myl.Y1)
