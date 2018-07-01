Sys.setlocale("LC_TIME", "C")
library(readxl)
library(dplyr)
library(reshape2)
library(lubridate)

# load raw data
raw.data <- read_excel(path = "data/bank_combas.xlsx",
                       sheet = 1,
                       skip = 0,
                       col_names = T,
                       col_types = c("numeric", "text", "numeric", "date", "text", rep("numeric", 67))) %>% as.data.frame
names(raw.data) <- c("银行代码", "银行中文简称", "股票代码", "会计期间", "报表类型编码", "现金及存放中央银行款项", "贵金属", "存放同业款项", "拆出资金净额", "交易性金融资产", "衍生金融资产", "买入返售金融资产净额", "应收款项类投资", "其他应收款净额", "应收利息净额", "应收股利净额", "发放贷款及垫款净额", "定期存款", "可供出售金融资产净额", "持有至到期投资净额", "长期股权投资净额", "投资性房地产净额", "固定资产净额", "在建工程净额", "固定资产清理", "无形资产净额", "商誉净额", "长期待摊费用", "递延所得税资产", "其他资产", "资产总计", "向中央银行借款", "拆入资金", "吸收存款及同业存放", "其中：同业及其他金融机构存放款项", "其中：吸收存款", "短期借款", "交易性金融负债", "衍生金融负债", "卖出回购金融资产款", "应付职工薪酬", "应交税费", "应付利息", "应付股利", "递延收益-流动负债", "其他应付款", "长期借款", "应付债券", "预计负债", "递延收益-非流动负债", "递延所得税负债", "其他负债", "负债合计", "实收资本(或股本)", "其他权益工具", "其中：优先股", "其中：永续债", "其中：其他", "资本公积", "减：库存股", "专项储备", "其他综合收益", "盈余公积", "未分配利润", "外币报表折算差额", "未确认的投资损失", "一般风险准备", "交易风险准备", "归属于母公司所有者权益合计", "少数股东权益", "所有者权益合计", "负债与所有者权益总计")
raw.data$银行中文简称[raw.data$银行中文简称 == "中国工商银行" ] <- "工商银行"
raw.data$银行中文简称[raw.data$银行中文简称 == "中国建设银行" ] <- "建设银行"
raw.data$银行中文简称[raw.data$银行中文简称 == "中国民生银行" ] <- "民生银行"
raw.data$银行中文简称[raw.data$银行中文简称 == "中国农业银行" ] <- "农业银行"
combas.date <- unique(raw.data$会计期间)
combas.date <- combas.date[order(combas.date)]
p.matrix <- list()
matrix <- list()
data.interbank <- list()
data.interbank14 <- list()
for (i in 1:length(combas.date)) {
  data <- filter(raw.data,  报表类型编码 == "A" & 会计期间 == combas.date[[i]])
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
  locate <- sapply(c("交通银行", "工商银行", "建设银行", "中国银行", "平安银行", "浦发银行", "华夏银行", "民生银行", "招商银行", "兴业银行", "中信银行", "宁波银行", "南京银行", "北京银行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$拆出资金净额, colsums = data$拆入资金, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  rownames(EstimatedMatrix.me) <- data$银行中文简称
  colnames(EstimatedMatrix.me) <- data$银行中文简称
  
  matrix[[i]] <- EstimatedMatrix.me
  p.matrix[[i]] <- EstimatedMatrix.me[locate,locate]
  data.interbank[[i]] <- data
  
  data.interbank14[[i]] <- data[locate,]
}

max.matrix <- unlist(p.matrix) %>% max
sd.matrix <- list()
for (i in 1:length(p.matrix)) {
  sd.matrix[[i]] <- p.matrix[[i]]/max.matrix
}

temp <- sd.matrix %>% unlist
temp <- temp[temp>0.02]
hist(temp,breaks=100,col="red4",xlab="fevd", main="histogram")
quantile(temp,c(0,0.2,0.5,0.8,1))

#igraph
for (i in seq(1,length(sd.matrix),1)) {
  temp <- sd.matrix[[i]] %>% t#temp[lower.tri(temp)] <- 0
  if(max(temp) > 0.02){
    temp[temp < 0.02] <- 0
  }
  igraph.matrix <- igraph::graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
  networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
  networkD3.matrix[[2]]$group <- group[5, colnames(sd.matrix[[i]])]
  networkD3.matrix[[2]]$name <- group[1, colnames(sd.matrix[[i]])]#group[1,]
  
  networkD3.matrix[[2]]$size <- colSums(temp)
  networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
  networkD3.matrix[[2]] <- networkD3.matrix[[2]][order(networkD3.matrix[[2]]$group,decreasing = F),] 
  networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*20
  networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu*20
  matrix.date <- data.frame(name="",size=0, group=as.character(substr(combas.date[i],1,4)))
  networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
  
  ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white"]);'
  rbokeh.interbank <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                              Source = "source", Target = "target", Value = "value", 
                              arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                              NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                              Group = "group", legend = T, fontSize = 15, opacity = 0.8, opacityNoHover = 1,
                              height = 700 , width = 700, bounded = F, zoom = T, charge = -500)
  widget2png(rbokeh.interbank, file = paste0("latex/report/igraph/interbank", substr(combas.date[i],1,4), ".png"), timeout = 1200)
  #time.end <- Sys.time()
  #running.time <- (time.end - time.start)
  #print(running.time)
  
  cat("\r", round(i/length(fevd.matrix),2))
  flush.console()
}

#network drawing
data <- data.interbank[[length(data.interbank)]] 
EstimatedMatrix.me <- matrix_estimation(rowsums = data$拆出资金净额, colsums = data$拆入资金, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
#rownames(EstimatedMatrix.me) <- data$银行中文简称
#colnames(EstimatedMatrix.me) <- data$银行中文简称
#max.matrix <- max(EstimatedMatrix.me)
temp <- EstimatedMatrix.me/max.matrix 
temp <- temp %>% t
if(max(temp)>0.02){
  temp[temp < 0.02] <- 0
}
library(igraph)

igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
networkD3.matrix[[2]]$group <- data$银行类型

networkD3.matrix[[2]]$size <- colSums(temp)
networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*10
networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$value*10
#matrix.date <- data.frame(name="",size=0, group=as.character(substr(combas.date[i],1,4)))
networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)

ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","#1A237E","white"]);'
#rbokeh.interbank <- 
rbokeh <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                                 Source = "source", Target = "target", Value = "value", 
                                 arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                                 NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                                 Group = "group", legend = T, #fontSize = 15,
                                 opacity = 0.8, opacityNoHover = 1,
                                 height = 700 , width = 700, bounded = F, zoom = T, charge = -1000)
widget2png(rbokeh , file = paste0("latex/report/igraph/Allinterbank", substr(combas.date[i],1,4), ".png"), timeout = 1200)


#number of bnaks in inerbank market.
number.banks <- c()
for (i in 1:length(data.interbank)) {
  temp <- nrow(data.interbank[[i]])
  number.banks <- c(number.banks, temp)
}

number.banks <- xts(number.banks, as.Date( paste0(seq(2000,2016,1),"-06-30"), format='%Y-%m-%d'))
index.higher <- xts(is.higher$crisis.sma20, as.Date(shibor$Date, format='%Y-%m-%d'))
dy.data <- merge(number.banks, index.higher)
names(dy.data) <- c("number.banks","crisis")
for (i in 2:length(dy.data$number.banks)) {
  if(is.na(dy.data$number.banks[i])){
    dy.data$number.banks[i] <- dy.data$number.banks[i-1]
  }
}

                   
dy.main <- "Number of Participants in China's Interbank Market from 2006 to 2016"
color <- c("red","black")
dygraph.interbank(dy.data, dy.main, color, begin.dateWindow = index(index.higher)[1], end.dateWindow = index(number.banks)[17]) %>% 
  dySeries("crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5 ,axis = "y2") %>%
  dyOptions(stepPlot = TRUE) %>%
  dyAxis("y", label = "Number of Participants in China's Interbank Market", independentTicks = TRUE) %>%
  dyAxis("y2", label = "Crisis", independentTicks = TRUE, drawGrid = F)
  
  
