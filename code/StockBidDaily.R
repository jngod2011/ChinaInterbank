#time###########################################################################################
timestart <- Sys.time()
set.seed(1)
#setting###########################################################################################
set <- "VECM_GIR_UA"
set.o <- set
inclusion <- c("ON","W1","W2","M1","M3","M6","M9","Y1")
trans <- inclusion
BidType <- "vecm.gir"
MCMLE.maxit <- 50
triangle <- "all"#NULL
directed <- F
library(knitr)
set <- paste0(set,
              combine_words(inclusion, sep = "", and = ""))
#library###########################################################################################
# load libraries

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
#specify 10 banks###########################################################################################
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
#stock price###########################################################################################
# load stock price data
## group information
group.stockprice <- read_excel(path = "data/stockprice.xlsx",
                               sheet = "group",
                               skip = 0,
                               col_names = T,
                               col_types = rep("text", 5)) %>% as.data.frame
group.stockprice$Eclass <- factor(group.stockprice$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
group.stockprice <- group.stockprice[order(group.stockprice$Eclass,decreasing = F),] 
rownames(group.stockprice) <- group.stockprice$Abbr
group.stockprice$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(10)[-10], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6])
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
#data <- na.omit(data)

#network.x###########################################################################################

if(!is.na(match("ON",inclusion))){
aenet.myl.ON <- aenet.fix(data = merge(all.bid.ON,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("W1",inclusion))){
aenet.myl.W1 <- aenet.fix(data = merge(all.bid.W1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("W2",inclusion))){
aenet.myl.W2 <- aenet.fix(data = merge(all.bid.W2,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("M1",inclusion))){
aenet.myl.M1 <- aenet.fix(data = merge(all.bid.M1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("M3",inclusion))){
aenet.myl.M3 <- aenet.fix(data = merge(all.bid.M3,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("M6",inclusion))){
aenet.myl.M6 <- aenet.fix(data = merge(all.bid.M6,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("M9",inclusion))){
aenet.myl.M9 <- aenet.fix(data = merge(all.bid.M9,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}
if(!is.na(match("Y1",inclusion))){
  aenet.myl.Y1 <- aenet.fix(data = merge(all.bid.Y1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
}

vecm.gir.myl.ON <- list(); vecm.gir.myl.W1 <- list()
vecm.gir.myl.W2 <- list(); vecm.gir.myl.M1 <- list()
vecm.gir.myl.M3 <- list(); vecm.gir.myl.M6 <- list()
vecm.gir.myl.M9 <- list(); vecm.gir.myl.Y1 <- list()

#vecm.fevd.myl.ON <- list(); vecm.fevd.myl.W1 <- list()
#vecm.fevd.myl.W2 <- list(); vecm.fevd.myl.M1 <- list()
#vecm.fevd.myl.M3 <- list(); vecm.fevd.myl.M6 <- list()
#vecm.fevd.myl.M9 <- list(); vecm.fevd.myl.Y1 <- list()

#if(!is.na(match("ON",inclusion))){
#  vecm.fevd.myl.ON <- dy.VECM.FEVD(data = all.bid.ON[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("W1",inclusion))){
#  vecm.fevd.myl.W1 <- dy.VECM.FEVD(data = all.bid.W1[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("W2",inclusion))){
#  vecm.fevd.myl.W2 <- dy.VECM.FEVD(data = all.bid.W2[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("M1",inclusion))){
#  vecm.fevd.myl.M1 <- dy.VECM.FEVD(data = all.bid.M1[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("M3",inclusion))){
#  vecm.fevd.myl.M3 <- dy.VECM.FEVD(data = all.bid.M3[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("M6",inclusion))){
#  vecm.fevd.myl.M6 <- dy.VECM.FEVD(data = all.bid.M6[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("M9",inclusion))){
#  vecm.fevd.myl.M9 <- dy.VECM.FEVD(data = all.bid.M9[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#  }
#if(!is.na(match("Y1",inclusion))){
#  vecm.fevd.myl.Y1 <- dy.VECM.FEVD(data = all.bid.Y1[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)[[1]]
#}
################################################################################################################################################################
data <- merge(bond)
vecm.tsDyn <- VECM(data, lag=K, estim="ML")#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
rank.test <- vecm.tsDyn
# Maximum-likelihood test of the cointegrating rank.
rank.eigen <- rank.test(vecm.tsDyn, cval = 0.05, type = "eigen")$r
rank.trace <- rank.test(vecm.tsDyn, cval = 0.05, type = "trace")$r
v.rank <- c(rank.eigen, rank.trace)
names(v.rank) <- c("eigen", "trace")
floor((rank.eigen + rank.trace)/2)
rank <- ifelse(missing(rank),floor((rank.eigen + rank.trace)/2), rank)
rank 

if(!is.na(match("ON",inclusion))){#9
   vecm.gir.myl.ON <- dy.VECM.GIR(data = all.bid.ON[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
  }
if(!is.na(match("W1",inclusion))){#9
  vecm.gir.myl.W1 <- dy.VECM.GIR(data = all.bid.W1[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
  }
if(!is.na(match("W2",inclusion))){#9
  vecm.gir.myl.W2 <- dy.VECM.GIR(data = all.bid.W2[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
  }
if(!is.na(match("M1",inclusion))){#9
  vecm.gir.myl.M1 <- dy.VECM.GIR(data = all.bid.M1[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
  }
if(!is.na(match("M3",inclusion))){#9
  vecm.gir.myl.M3 <- dy.VECM.GIR(data = all.bid.M3[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
  }
if(!is.na(match("M6",inclusion))){#9
  vecm.gir.myl.M6 <- dy.VECM.GIR(data = all.bid.M6[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
  }
if(!is.na(match("M9",inclusion))){#9
  vecm.gir.myl.M9 <- dy.VECM.GIR(data = all.bid.M9[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
 }
   if(!is.na(match("Y1",inclusion))){#9
  vecm.gir.myl.Y1 <- dy.VECM.GIR(data = all.bid.Y1[,1:10], n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE, rank = 9)[[1]]
}
#network.y###########################################################################################
#if(set.o == "VAR_FEVD_"){network.y <- dy.VAR.FEVD(data = g.sp, n.ahead =1, mode= "fix", span =250)}
#if(set.o == "VAR_GIR_"){network.y <- dy.VAR.GIR(data = g.sp, n.ahead =1, mode= "fix", span =250)}
#if(set.o == "VECM_FEVD_"){network.y <- dy.VECM.FEVD(data = sp, group = names(sp), n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)}
#if(set.o == "VECM_GIR_"){#}
network.y <- dy.VECM.GIR(data = sp, group = names(sp), n.ahead = 1, mode = "fix", span = 250, keep.vecm = TRUE)
#ERGM###########################################################################################
dyCoefErgm(data = network.y[[1]], set = set, inclusion = inclusion,
           Date = network.y$Date, 
           trans = trans, 
           BidType = BidType,
           MCMLE.maxit = MCMLE.maxit,
           triangle = triangle,
           directed = directed
           )

