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
bank10.abre <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
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
rownames(group.stockprice) <- group.stockprice$Abre
group.stockprice$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(10)[-10], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6])
## stock price data
raw.stockprice <- read_excel(path = "data/stockprice.xlsx",
                             sheet = 1,
                             skip = 4,
                             col_names = F,
                             col_types = c("date", rep("numeric", 25))) %>% as.data.frame
sp <- raw.stockprice[,c(1, 18, 19, 20, 22, 23, 2, 6, 7, 8, 9, 15, 17, 21, 25, 3, 11, 12, 13, 16, 24, 4, 5, 10, 14, 26)]
names(sp) <- c("Date", group.stockprice$Abre)
sp <- sp[sp$Date >="2007-12-31", ]
sp <- sp[, !is.na(sp[1,])]

## stock price for 10 banks
sp <- sp[,c("Date", bank10.abre)]
g.sp <- as.data.frame(lapply(sp[,-1], log_GrowthRate))
sp <- xts(sp[,-1], as.Date(sp$Date, format='%Y-%m-%d'))["2008-01-01/"]
g.sp <- xts(g.sp, as.Date(index(sp)[-1], format='%Y-%m-%d'))
names(g.sp) <- paste0(names(g.sp),".g")
#bond###########################################################################################
# load bond
bond <- read_excel(path = "data/bond/Chinabond.xlsx",
                   sheet = "yield", 
                   skip = 22, 
                   col_names = F, 
                   col_types = c("date", rep("numeric", 15))) %>% as.data.frame
names(bond) <- c("Date","TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
bond$Date <- substr(bond$Date, 1, 10)
bond <- xts(bond[,-1], as.Date(bond$Date, format='%Y-%m-%d'))
#shibor###########################################################################################
# load shibor data
fnlist.shibor <- dir("data/shibor")
raw.shibor <- list()
length.fnlist <- length(fnlist.shibor)
shibor <- c()
for(i in 1:length.fnlist){
  raw.shibor[[i]] <- read_excel(path = paste0("data/shibor/",fnlist.shibor[i]),
                                sheet = "Sheet",
                                skip = 0,
                                col_names = T,
                                col_types = c("date", rep("numeric", 8))) %>% as.data.frame
  shibor <- rbind(shibor,raw.shibor[[i]])
}
names(shibor)[1] <- "Date"
shibor$Date<- substr(shibor$Date, 1, 10)
## shibor: is shorter rae higher than the longer one?
is.higher <- shibor
is.higher[,c(2:dim(shibor)[2])] <- 0
names(is.higher)[ncol(is.higher)] <- "sum"
for (i in 1:nrow(shibor)) {
  for (j in 2:(ncol(shibor)-1)) {
    for (f in 1:(9-j)) {
      if(shibor[i,j]>shibor[i,j+f]){is.higher[i,j] <- is.higher[i,j]+1
      }
    }
  }
}
is.higher$sum <- rowSums(is.higher[,-c(1,ncol(is.higher))])
is.higher$crisis.sma20 <- TTR::SMA(is.higher$sum,20)
is.higher$crisis <- cut(is.higher$sum,breaks = 5, include.lowest=F, labels = c(1:5))
is.higher <- xts(is.higher, as.Date(is.higher$Date, format='%Y-%m-%d'))
#shibor bid###########################################################################################
# load shibor bid data
## load group information for bid data

group.bid <- read_excel(path = "data/GroupBid.xlsx",
                        sheet = "group",
                        skip = 0,
                        col_names = T,
                        col_types = rep("text", 5)) %>% as.data.frame
group.bid$Eclass <- factor(group.bid$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks","Foreign Banks"))
group.bid <- group.bid[order(group.bid$Eclass,decreasing = F),] 
rownames(group.bid) <- group.bid$Abre
group.bid$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(11)[-11], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6],"#2196F3")
group.bid$Eclass <- factor(group.bid$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks","Foreign Banks"))
group.bid <- group.bid[order(group.bid$Eclass, decreasing = F),]
#match(c("农业银行", "交通银行", "工商银行", "建设银行","中国银行",  "浦发银行", "华夏银行", "招商银行", "兴业银行", "上海银行", "光大银行", "中信银行", "广发银行", "北京银行","汇丰中国"),group.bid$Cname)
group.bid <- group.bid[c(1, 2, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 20, 27),]# dim(group.bid)

## load shibor bid data
fnlist.shibor.bid <- dir("data/bid")
raw.shibor.bid <- list()
shibor.bid <- c()
all.bid.banks <- c()
all.shibor <- c()
locate <- c()
for(i in 3:length(fnlist.shibor.bid)){#2008
  if(as.numeric(substr(fnlist.shibor.bid[i],4,7))<=2015){
    raw.shibor.bid[[i]] <- read_excel(path = paste0("data/bid/",fnlist.shibor.bid[i]),
                                      sheet = "Sheet",
                                      skip = 0,
                                      col_names = T,
                                      col_types = c("date", "text", rep("numeric", 16)))[-c(1,2),-c(seq(4,18,2))] %>% as.data.frame
    names(raw.shibor.bid[[i]])[c(1,2)] <- c("Date","bider")
    shibor.bid <- rbind(shibor.bid,raw.shibor.bid[[i]])
  }else{
    raw.shibor.bid[[i]] <- read_excel(path = paste0("data/bid/",fnlist.shibor.bid[i]),
                                      sheet = "Sheet",
                                      skip = 0,
                                      col_names = T,
                                      col_types = c("date", "text", rep("numeric", 8))) %>% as.data.frame
    names(raw.shibor.bid[[i]])[c(1,2)] <- c("Date","bider")
    shibor.bid <- rbind(shibor.bid,raw.shibor.bid[[i]])
  }
  names(shibor.bid)[c(1,2)] <- c("Date","bider")
  #print(substr(fnlist.shibor.bid[i],4,7))
  #print(dim(raw.shibor.bid[[i]]))
  #print(unique(raw.shibor.bid[[i]]$bider))
  #print(length(unique(raw.shibor.bid[[i]]$bider)))
  #temp <- match(unique(raw.shibor.bid[[i]]$bider), all.bid.banks)
  #locate <- c(temp,locate)
  #print("===========================================")
  all.bid.banks <- c(all.bid.banks,unique(raw.shibor.bid[[i]]$bider)) %>% unique
}
shibor.bid$Date<- substr(shibor.bid$Date, 1, 10)

all.bid <- c()
for(i in 2:length(fnlist.shibor.bid)){
  all.bid <- rbind(all.bid,raw.shibor.bid[[i]])
}

names(all.bid) <- c("Date", "Bank", "ON", "W1", "W2", "M1", "M3", "M6", "M9", "Y1")
all.bid$Date <- substr(all.bid$Date,1,10)
all.bid <- melt(all.bid, id = c("Date", "Bank"), variable.name = "Term", value.name = "Bid", na.rm=T)
all.bid <- dcast(all.bid, Date + Term ~ Bank , fun.aggregate = mean) %>% as.data.frame
match(group.bid$Cname, names(all.bid))
all.bid <- all.bid[, c(1,2,9,6,14,16,5,21,11,18,8,3,7,4,15,10,20)]
names(all.bid)[-c(1,2)] <- group.bid$Abre
all.bid <- all.bid[, c("Date", "Term", bank10.abre)]
sum(is.na(all.bid))

all.bid.ON <- all.bid[all.bid$Term == "ON",-c(2)] #all.bid.ON <- all.bid.ON[,!names(all.bid.ON) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.ON <- xts(all.bid.ON[,-1], as.Date(all.bid.ON$Date, format='%Y-%m-%d'))
names(all.bid.ON) <- paste0(names(all.bid.ON),".ON")

all.bid.W1 <- all.bid[all.bid$Term == "W1",-c(2)] #all.bid.W1 <- all.bid.W1[,!names(all.bid.W1) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.W1 <- xts(all.bid.W1[,-1], as.Date(all.bid.W1$Date, format='%Y-%m-%d'))
names(all.bid.W1) <- paste0(names(all.bid.W1),".W1")

all.bid.W2 <- all.bid[all.bid$Term == "W2",-c(2)] #all.bid.W2 <- all.bid.W2[,!names(all.bid.W2) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.W2 <- xts(all.bid.W2[,-1], as.Date(all.bid.W2$Date, format='%Y-%m-%d'))
names(all.bid.W2) <- paste0(names(all.bid.W2),".W2")

all.bid.M1 <- all.bid[all.bid$Term == "M1",-c(2)] #all.bid.M1 <- all.bid.M1[,!names(all.bid.M1) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.M1 <- xts(all.bid.M1[,-1], as.Date(all.bid.M1$Date, format='%Y-%m-%d'))
names(all.bid.M1) <- paste0(names(all.bid.M1),".M1")

all.bid.M3 <- all.bid[all.bid$Term == "M3",-c(2)] #all.bid.M3 <- all.bid.M3[,!names(all.bid.M3) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.M3 <- xts(all.bid.M3[,-1], as.Date(all.bid.M3$Date, format='%Y-%m-%d'))
names(all.bid.M3) <- paste0(names(all.bid.M3),".M3")

all.bid.M6 <- all.bid[all.bid$Term == "M6",-c(2)] #all.bid.M6 <- all.bid.M6[,!names(all.bid.M6) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.M6 <- xts(all.bid.M6[,-1], as.Date(all.bid.M6$Date, format='%Y-%m-%d'))
names(all.bid.M6) <- paste0(names(all.bid.M6),".M6")

all.bid.M9 <- all.bid[all.bid$Term == "M9",-c(2)] #all.bid.M9 <- all.bid.M9[,!names(all.bid.M9) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.M9 <- xts(all.bid.M9[,-1], as.Date(all.bid.M9$Date, format='%Y-%m-%d'))
names(all.bid.M9) <- paste0(names(all.bid.M9),".M9")

all.bid.Y1 <- all.bid[all.bid$Term == "Y1",-c(2)] #all.bid.Y1 <- all.bid.Y1[,!names(all.bid.Y1) %in% c("南京银行","国开行","德银中国","民生银行","邮储银行","渣打银行")]
all.bid.Y1 <- xts(all.bid.Y1[,-1], as.Date(all.bid.Y1$Date, format='%Y-%m-%d'))
names(all.bid.Y1) <- paste0(names(all.bid.Y1),".Y1")


data <- merge(is.higher,g.sp,sp, bond, all.bid.ON, all.bid.W1, all.bid.W2, all.bid.M1, all.bid.M3, all.bid.M6, all.bid.M9, all.bid.Y1)
#data <- na.omit(data)
data <- data["2008-01-01/2017-12-31"]
Date <- index(data) %>% as.character
#md.pattern(data)
#data <- apply(data, 2, f.na.approx)
#data <- xts(data, as.Date(Date, format='%Y-%m-%d'))
#data <- data[1:260,]
sp <- data[,names(sp)]
g.sp <- data[,names(g.sp)]

timesmiddle1 <- Sys.time()
#bond <- data[,names(bond)]
all.bid.ON <- data[,names(all.bid.ON)] %>% as.data.frame
all.bid.ON <- missForest(all.bid.ON, variablewise = T)$ximp
all.bid.ON <- xts(all.bid.ON, as.Date(Date, format='%Y-%m-%d'))

all.bid.W1 <- data[,names(all.bid.W1)] %>% as.data.frame
all.bid.W1 <- missForest(all.bid.W1, variablewise = T)$ximp
all.bid.W1 <- xts(all.bid.W1, as.Date(Date, format='%Y-%m-%d'))


all.bid.W2 <- data[,names(all.bid.W2)] %>% as.data.frame
all.bid.W2 <- missForest(all.bid.W2, variablewise = T)$ximp
all.bid.W2 <- xts(all.bid.W2, as.Date(Date, format='%Y-%m-%d'))

all.bid.M1 <- data[,names(all.bid.M1)] %>% as.data.frame
all.bid.M1 <- missForest(all.bid.M1, variablewise = T)$ximp
all.bid.M1 <- xts(all.bid.M1, as.Date(Date, format='%Y-%m-%d'))

all.bid.M3 <- data[,names(all.bid.M3)] %>% as.data.frame
all.bid.M3 <- missForest(all.bid.M3, variablewise = T)$ximp
all.bid.M3 <- xts(all.bid.M3, as.Date(Date, format='%Y-%m-%d'))

all.bid.M6 <- data[,names(all.bid.M6)] %>% as.data.frame
all.bid.M6 <- missForest(all.bid.M6, variablewise = T)$ximp
all.bid.M6 <- xts(all.bid.M6, as.Date(Date, format='%Y-%m-%d'))

all.bid.M9 <- data[,names(all.bid.M9)] %>% as.data.frame
all.bid.M9 <- missForest(all.bid.M9, variablewise = T)$ximp
all.bid.M9 <- xts(all.bid.M9, as.Date(Date, format='%Y-%m-%d'))

all.bid.Y1 <- data[,names(all.bid.Y1)] %>% as.data.frame
all.bid.Y1 <- missForest(all.bid.Y1, variablewise = T)$ximp
all.bid.Y1 <- xts(all.bid.Y1, as.Date(Date, format='%Y-%m-%d'))

bond <- data[,names(bond)] %>% as.data.frame
bond <- missForest(bond, variablewise = T)$ximp
bond <- xts(bond, as.Date(Date, format='%Y-%m-%d'))
#timesmiddle2 <- Sys.time()
#timesmiddle2 - timesmiddle1
#timesmiddle2 - timestart 

test.data <- data %>% as.data.frame
test.data <- missForest(test.data, variablewise = T)$ximp
test.data <- xts(test.data, as.Date(Date, format='%Y-%m-%d'))
dygraph(test.data[,names(all.bid.ON)])
dygraph(data[,names(all.bid.ON)])
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

