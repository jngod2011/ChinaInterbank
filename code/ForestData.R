#################################################################
#time
#################################################################
timestart <- Sys.time()
set.seed(1)
#################################################################
#library
#################################################################
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
library(beepr)#beep()
library(mice)
library(VIM)
library(lattice)
library(missForest)
source("code/function.R")
source("code/functionInterbank.R")
source("code/functionNewRMB.R")
#################################################################
#specify 10 banks
#################################################################
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
#################################################################
#stock price
#################################################################
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
raw.stockprice <- read_excel(path = "data/stockprice.xlsx",
                             sheet = 1,
                             skip = 4,
                             col_names = F,
                             col_types = c("date", rep("numeric", 25))) %>% as.data.frame
sp <- raw.stockprice[,c(1, 18, 19, 20, 22, 23, 2, 6, 7, 8, 9, 15, 17, 21, 25, 3, 11, 12, 13, 16, 24, 4, 5, 10, 14, 26)]
names(sp) <- c("Date", group.stockprice$Abbr)
sp <- sp[sp$Date >="2007-12-31", ]
sp <- sp[, !is.na(sp[1,])]

## stock price for 10 banks
sp <- sp[,c("Date", bank10.abbr)]
g.sp <- as.data.frame(lapply(sp[,-1], log_GrowthRate))
sp <- xts(sp[,-1], as.Date(sp$Date, format='%Y-%m-%d'))["2008-01-01/"]
g.sp <- xts(g.sp, as.Date(index(sp)[-1], format='%Y-%m-%d'))
names(g.sp) <- paste0(names(g.sp),".g")
#################################################################
#bond
#################################################################
# load bond
bond <- read_excel(path = "data/bond/Chinabond.xlsx",
                   sheet = "yield", 
                   skip = 22, 
                   col_names = F, 
                   col_types = c("date", rep("numeric", 15))) %>% as.data.frame
names(bond) <- c("Date","TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
bond$Date <- substr(bond$Date, 1, 10)
bond <- xts(bond[,-1], as.Date(bond$Date, format='%Y-%m-%d'))
#################################################################
#shibor
#################################################################
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
is.higher <- is.higher[-which(weekdays(index(is.higher))=="Saturday"),]
is.higher <- is.higher[-which(weekdays(index(is.higher))=="Sunday"),]
is.higher <- subset(is.higher,select = -Date)
#################################################################
#shibor bid
#################################################################
# load shibor bid data
## load group information for bid data

group.bid <- read_excel(path = "data/GroupBid.xlsx",
                        sheet = "group",
                        skip = 0,
                        col_names = T,
                        col_types = rep("text", 5)) %>% as.data.frame
group.bid$Eclass <- factor(group.bid$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks","Foreign Banks"))
group.bid <- group.bid[order(group.bid$Eclass,decreasing = F),] 
rownames(group.bid) <- group.bid$Abbr
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
names(all.bid)[-c(1,2)] <- group.bid$Abbr
all.bid <- all.bid[, c("Date", "Term", bank10.abbr)]
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

data <- merge(g.sp,sp, bond, is.higher,all.bid.ON, all.bid.W1, all.bid.W2, all.bid.M1, all.bid.M3, all.bid.M6, all.bid.M9, all.bid.Y1)#data <- na.omit(data) 
data <- data["2008-01-01/2017-12-31"]
data <- data[-which(weekdays(index(data))=="Saturday"),]
data <- data[-which(weekdays(index(data))=="Sunday"),]

g.sp <- data[,names(g.sp)]
data <- data[-which(is.na(g.sp[,1])==TRUE),]
Date <- index(data) %>% as.character

mdp <- md.pattern(data)
aggr(data,prop=FALSE, numbers=TRUE)
matrixplot(data)#time line of NA
#dim(data)#2373

mf.data <- data %>% as.data.frame
mf.data <- missForest(mf.data, variablewise = T, ntree = 2000)#mice(test.data, m=1, maxit = 50, method = 'pmm', seed = 500,remove_collinear = FALSE)#complete(mf.data,1)
names(mf.data)
c.data <- mf.data$ximp
matrixplot(c.data)

oob <- mf.data$OOBerror
oob


#**#write.csv(c.data, file = "ForestData.csv")

#test.data <- read.csv(file = "ForestData1.csv")
#test.data <- xts(test.data, as.Date(index(data), format='%Y-%m-%d'))
#dygraph(test.data[,names(all.bid.Y1)]) %>% dyOptions(colors = c("gray","gray","gray","gray","red","gray","gray","gray","gray","gray"))
#dygraph(data[,names(all.bid.Y1)]) %>% dyOptions(colors = c("gray","gray","gray","gray","red","gray","gray","gray","gray","gray"))
#beep()

#################################################################
# load interbank loan data
#################################################################
raw.combas <- read_excel(path = "data/bank_combas.xlsx",
                         sheet = 1,
                         skip = 0,
                         col_names = T,
                         col_types = c("numeric", "text", "numeric", "date", "text", rep("numeric", 67))) %>% as.data.frame
names(raw.combas) <- c("银行代码", "银行中文简称", "股票代码", "会计期间", "报表类型编码", "现金及存放中央银行款项", "贵金属", "存放同业款项", "拆出资金净额", "交易性金融资产", "衍生金融资产", "买入返售金融资产净额", "应收款项类投资", "其他应收款净额", "应收利息净额", "应收股利净额", "发放贷款及垫款净额", "定期存款", "可供出售金融资产净额", "持有至到期投资净额", "长期股权投资净额", "投资性房地产净额", "固定资产净额", "在建工程净额", "固定资产清理", "无形资产净额", "商誉净额", "长期待摊费用", "递延所得税资产", "其他资产", "资产总计", "向中央银行借款", "拆入资金", "吸收存款及同业存放", "其中：同业及其他金融机构存放款项", "其中：吸收存款", "短期借款", "交易性金融负债", "衍生金融负债", "卖出回购金融资产款", "应付职工薪酬", "应交税费", "应付利息", "应付股利", "递延收益-流动负债", "其他应付款", "长期借款", "应付债券", "预计负债", "递延收益-非流动负债", "递延所得税负债", "其他负债", "负债合计", "实收资本(或股本)", "其他权益工具", "其中：优先股", "其中：永续债", "其中：其他", "资本公积", "减：库存股", "专项储备", "其他综合收益", "盈余公积", "未分配利润", "外币报表折算差额", "未确认的投资损失", "一般风险准备", "交易风险准备", "归属于母公司所有者权益合计", "少数股东权益", "所有者权益合计", "负债与所有者权益总计")
raw.combas$银行中文简称[raw.combas$银行中文简称 == "中国工商银行" ] <- "工商银行"
raw.combas$银行中文简称[raw.combas$银行中文简称 == "中国建设银行" ] <- "建设银行"
raw.combas$银行中文简称[raw.combas$银行中文简称 == "中国民生银行" ] <- "民生银行"
raw.combas$银行中文简称[raw.combas$银行中文简称 == "中国农业银行" ] <- "农业银行"
combas.date <- unique(raw.combas$会计期间)
combas.date <- combas.date[order(combas.date)]
p.matrix <- list()
matrix <- list()
data.interbank <- list()
data.interbank10 <- list()
for (i in 1:length(combas.date)) {
  data <- filter(raw.combas,  报表类型编码 == "A" & 会计期间 == combas.date[[i]])
  data[is.na(data)] <- 0
  data$银行类型 <- rep("Urban Commercial Banks", nrow(data))
  
  locate <- which(grepl("农",data$银行中文简称))
  locate <- locate %>% unlist
  data$银行类型[locate] <- "Rural Commercial Banks"
  
  locate <- sapply(c("国家开发银行", "进出口银行", "农发行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  data$银行类型[locate] <- "Policy Banks"
  
  locate <- sapply(c("工商银行", "农业银行", "建设银行", "交通银行", "中国银行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  data$银行类型[locate] <- "State-Owned Banks"
  
  locate <- sapply(c("招商银行", "浦发银行", "中信银行", "光大银行", "华夏银行", "中国民生银行", "广发银行", "兴业银行", "平安银行", "恒丰银行", "浙商银行", "渤海银行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which) 
  locate <- locate %>% unlist
  data$银行类型[locate] <-  "Joint-Equity Commercial Banks"
  
  data$银行类型 <- factor(data$银行类型, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks", "Policy Banks"))
  data <- data[order(data$银行类型,decreasing = F),] 
  
  #maximum entropy
  locate <- sapply(c("交通银行", "工商银行", "建设银行", "中国银行", "浦发银行", "华夏银行", "招商银行", "兴业银行", "中信银行", "北京银行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$拆出资金净额, colsums = data$拆入资金, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  rownames(EstimatedMatrix.me) <- data$银行中文简称
  colnames(EstimatedMatrix.me) <- data$银行中文简称
  
  matrix[[i]] <- EstimatedMatrix.me
  
  p.matrix[[i]] <- EstimatedMatrix.me[locate,locate] #part of the matrix
  rownames(p.matrix[[i]]) <- bank10.abbr#10:length(p.matrix)) = 2009:2016
  colnames(p.matrix[[i]]) <- bank10.abbr#**#
  data.interbank[[i]] <- data
  
  data.interbank10[[i]] <- data[locate,]
}

#################################################################
#import wind data
#################################################################
group.wind <- c("交通银行股份有限公司", 
                "中国工商银行股份有限公司", 
                "中国建设银行股份有限公司", 
                "中国银行股份有限公司", 
                "上海浦东发展银行股份有限公司", 
                "华夏银行股份有限公司", 
                "招商银行股份有限公司", 
                "兴业银行股份有限公司", 
                "中信银行股份有限公司", 
                "北京银行股份有限公司")

fnlist.wind <- dir("data/wind")
raw.wind <- list()
length.fnlist <- length(fnlist.wind)
raw.wind <- c()
for(i in 1:length.fnlist){#2007:2016
  temp <- read_excel(path = paste0("data/wind/",fnlist.wind[i]),
                     sheet = "万得",
                     skip = 0,
                     col_names = T,
                     col_types = c("text", "text", rep("numeric", 55), "date")) %>% as.data.frame
  temp <- temp[temp$数据来源=="合并报表",]
  
  locate <- sapply(group.wind, grepl, temp$公司名称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  temp <- temp[locate,]
  #temp$公司名称 <- group[1,]
  temp <- temp[,c("资产总计(万元)","负债合计(万元)","一般风险准备(万元)","手续费及佣金收入(万元)","营业收入(万元)","现金及存放中央银行款项(万元)","向中央银行借款(万元)","拆出资金(万元)","拆入资金(万元)")]
  names(temp) <- c("ass", "dbt", "rsr", "income.fee", "income",  "ltc", "bfc", "lnd", "brr")
  #cat(dim(raw.wind[[i]])[1]," ")
  rownames(temp) <- bank10.abbr
  raw.wind[[i]] <- temp
  
  #**#xlsx::write.xlsx(raw.wind[[i]], file="data/raw.wind.xlsx", sheetName=paste0("wind",i), append=TRUE, row.names=T, col.names = T, showNA = F) 
}
sum(is.na(unlist(raw.wind[[i]])))

#read.xlsx(file="data/raw.wind.xlsx", sheetName = paste0("wind",i))
#################################################################
# save data
#################################################################
save(c.data,raw.wind,p.matrix,file="data/ForestData.Rdata")
#load(file="data/ForestData.Rdata")
