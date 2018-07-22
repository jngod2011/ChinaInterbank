#################################################################
# combas loan and deposit network
#################################################################
library(NetworkRiskMeasures)
load(file = "data/Rdata/latex_group.stockprice.Rdata")
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
raw.data <- read_excel(path = "data/bank_combas.xlsx",
                       sheet = "raw",
                       skip = 0,
                       col_names = T,
                       col_types = c("numeric", "text", "numeric", "date", "text", rep("numeric", 67))) %>% as.data.frame
names(raw.data) <- c("银行代码", "银行中文简称", "股票代码", "会计期间", "报表类型编码", "现金及存放中央银行款项", "贵金属", "存放同业款项", "拆出资金净额", "交易性金融资产", "衍生金融资产", "买入返售金融资产净额", "应收款项类投资", "其他应收款净额", "应收利息净额", "应收股利净额", "发放贷款及垫款净额", "定期存款", "可供出售金融资产净额", "持有至到期投资净额", "长期股权投资净额", "投资性房地产净额", "固定资产净额", "在建工程净额", "固定资产清理", "无形资产净额", "商誉净额", "长期待摊费用", "递延所得税资产", "其他资产", "资产总计", "向中央银行借款", "拆入资金", "吸收存款及同业存放", "其中：同业及其他金融机构存放款项", "其中：吸收存款", "短期借款", "交易性金融负债", "衍生金融负债", "卖出回购金融资产款", "应付职工薪酬", "应交税费", "应付利息", "应付股利", "递延收益-流动负债", "其他应付款", "长期借款", "应付债券", "预计负债", "递延收益-非流动负债", "递延所得税负债", "其他负债", "负债合计", "实收资本(或股本)", "其他权益工具", "其中：优先股", "其中：永续债", "其中：其他", "资本公积", "减：库存股", "专项储备", "其他综合收益", "盈余公积", "未分配利润", "外币报表折算差额", "未确认的投资损失", "一般风险准备", "交易风险准备", "归属于母公司所有者权益合计", "少数股东权益", "所有者权益合计", "负债与所有者权益总计")
bank.id <- raw.data$银行代码 %>% unique
for (i in 1:length(bank.id)) {
  temp <- raw.data[raw.data$银行代码 == bank.id[i], "银行中文简称"] %>% unique %>% length
  if(temp != 1){
    print(bank.id[i])
  }
}

combas.date <- unique(raw.data$会计期间)
combas.date <- combas.date[order(combas.date)]
p.matrix <- list()
matrix <- list()
d.matrix <- list()
p.d.matrix <- list()
data.interbank <- list()
p.matrix <- list()
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
  
  locate <- sapply(c("中国工商银行", "中国农业银行", "中国建设银行", "交通银行", "中国银行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  data$银行类型[locate] <- "State-Owned Banks"
  
  locate <- sapply(c("招商银行", "浦发银行", "中信银行", "光大银行", "华夏银行", "中国民生银行", "广发银行", "兴业银行", "平安银行", "恒丰银行", "浙商银行", "渤海银行"), grepl, data$银行中文简称)
  locate <- apply(locate, 2, which) 
  locate <- locate %>% unlist
  data$银行类型[locate] <-  "Joint-Equity Commercial Banks"
  
  data$银行类型 <- factor(data$银行类型, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks", "Policy Banks"))
  data <- data[order(data$银行类型,decreasing = F),] 
  #View(data[locate,]$存放同业款项)
  #maximum entropy
  locate <- match(group.stockprice[bank10.abbr,"Ccombas"],data$银行中文简称)
  #
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$拆出资金净额, colsums = data$拆入资金, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  rownames(EstimatedMatrix.me) <- data$银行中文简称
  colnames(EstimatedMatrix.me) <- data$银行中文简称
  matrix[[i]] <- EstimatedMatrix.me
  p.matrix[[i]] <- EstimatedMatrix.me[locate,locate]
  colnames(p.matrix[[i]]) <- group.stockprice[bank10.abbr,"Cname"]
  rownames(p.matrix[[i]]) <- group.stockprice[bank10.abbr,"Cname"]
  #
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$存放同业款项, colsums = data$`其中：同业及其他金融机构存放款项`, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  rownames(EstimatedMatrix.me) <- data$银行中文简称
  colnames(EstimatedMatrix.me) <- data$银行中文简称
  d.matrix[[i]] <- EstimatedMatrix.me
  p.d.matrix[[i]] <- EstimatedMatrix.me[locate,locate]
  colnames(p.d.matrix[[i]]) <- group.stockprice[bank10.abbr,"Cname"]
  rownames(p.d.matrix[[i]]) <- group.stockprice[bank10.abbr,"Cname"]
  
  data.interbank[[i]] <- data
}

names(matrix) <- paste0("year",substr(combas.date,1,4))
names(p.matrix) <- paste0("year",substr(combas.date,1,4))
names(d.matrix) <- paste0("year",substr(combas.date,1,4))
names(p.d.matrix) <- paste0("year",substr(combas.date,1,4))
save(matrix, p.matrix, d.matrix, p.d.matrix,file = "data/Rdata/latex_combas.Rdata")
#################################################################
# combas:proportion loan and deposit interbank network
#################################################################
load(file = "data/Rdata/latex_combas.Rdata")
bank10.Cname <- group.stockprice[bank10.abbr,"Cname"]
#original intrbank network
if(TRUE){
  r.loan.network <- lapply(p.matrix[10:17], FUN = function(x){
    y <- x/(10^12)
    return(y)})
  
  r.deposit.network <- lapply(p.d.matrix[10:17], FUN = function(x){
    y <- x/(10^12)
    return(y)})#log(deposit.network %>% unlist,10)
  
  r.compound.network <- list()
  for (i in 1:length(r.loan.network)) {
    r.compound.network[[i]] <- r.loan.network[[i]] + r.deposit.network[[i]]
  }
}#scale#

# divided the network by sum of lending of 10 banks
if(TRUE){
  pl.loan.network <- lapply(p.matrix[10:17], FUN = function(x){
    y <- x/rowSums(x)
    return(y)})
  
  pl.deposit.network <- lapply(p.d.matrix[10:17], FUN = function(x){
    y <- x/rowSums(x)
    return(y)})#log(deposit.network %>% unlist,10)
  
  pl.compound.network <- list()
  for (i in 1:length(p.matrix)) {
    pl.compound.network[[i]] <- p.matrix[[i]] + p.d.matrix[[i]]
  }
  pl.compound.network <- lapply(pl.compound.network[10:17], FUN = function(x){
    y <- x/rowSums(x)
    return(y)})
}

# divided the network by sum of borrwoing of 10 banks
if(TRUE){
  pb.loan.network <- lapply(p.matrix[10:17], FUN = function(x){
    y <- x/colSums(x)
    return(y)})
  
  pb.deposit.network <- lapply(p.d.matrix[10:17], FUN = function(x){
    y <- x/colSums(x)
    return(y)})#log(deposit.network %>% unlist,10)
  
  pb.compound.network <- list()
  for (i in 1:length(p.matrix)) {
    pb.compound.network[[i]] <- p.matrix[[i]] + p.d.matrix[[i]]
  }
  pb.compound.network <- lapply(pb.compound.network[10:17], FUN = function(x){
    y <- x/colSums(x)
    return(y)})
}

# divided the network by sum of borrowing of all banks
if(TRUE){
  ab.loan.network <- lapply(matrix[10:17], FUN = function(x){
    y <- x/colSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
    return(y)})
  
  for (i in 1:length(matrix[10:17])) {
    x <- matrix[10:17][[i]]
    y <- x/colSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
  }
  
  ab.deposit.network <- lapply(d.matrix[10:17], FUN = function(x){
    y <- x/colSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
    return(y)})#log(deposit.network %>% unlist,10)
  
  ab.compound.network <- list()
  for (i in 1:length(p.matrix)) {
    ab.compound.network[[i]] <- matrix[[i]] + d.matrix[[i]]
  }
  ab.compound.network <- lapply(ab.compound.network[10:17], FUN = function(x){
    y <- x/colSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
    return(y)})
}

# divided the network by sum of lending of all banks
if(TRUE){
  al.loan.network <- lapply(matrix[10:17], FUN = function(x){
    y <- x/rowSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
    return(y)})
  
  al.deposit.network <- lapply(d.matrix[10:17], FUN = function(x){
    y <- x/rowSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
    return(y)})#log(deposit.network %>% unlist,10)
  
  al.compound.network <- list()
  for (i in 1:length(p.matrix)) {
    al.compound.network[[i]] <- matrix[[i]] + d.matrix[[i]]
  }
  al.compound.network <- lapply(al.compound.network[10:17], FUN = function(x){
    y <- x/rowSums(x)
    locate <- sapply(bank10.Cname, grep, colnames(x)) %>% as.numeric
    y <- y[locate, locate]
    return(y)})
}

save(r.loan.network, r.deposit.network, r.compound.network,
     pl.loan.network, pl.deposit.network, pl.compound.network,
     pb.loan.network, pb.deposit.network, pb.compound.network,
     al.loan.network, al.deposit.network, al.compound.network,
     ab.loan.network, ab.deposit.network, ab.compound.network,
     file = "data/Rdata/latex_combas_rpa.Rdata")
#################################################################
# combas draw loan network
#################################################################
load(file = "data/Rdata/latex_combas.Rdata")
max.matrix <- unlist(matrix) %>% max
#
for (i in 1:length(data.interbank)) {
  #i <- length(data.interbank)
  data <- data.interbank[[length(data.interbank)]] 
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$拆出资金净额, colsums = data$拆入资金, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  temp <- EstimatedMatrix.me/max.matrix 
  if(max(temp)>0.02){temp[temp < 0.02] <- 0}
  igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
  networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
  networkD3.matrix[[2]]$group <- data$银行类型
  
  temp.size <- rowSums(temp)
  temp.size[temp.size < 0.4] <- 0.4
  networkD3.matrix[[2]]$size <- temp.size
  networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
  networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*10
  networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$value*20
  matrix.date <- data.frame(name="",size=0, group=as.character(substr(combas.date[i],1,4)))
  networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
  
  ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","#1A237E","white"]);'
  rbokeh <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                         Source = "source", Target = "target", Value = "value", 
                         arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                         NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                         Group = "group", legend = T, fontSize = 0,
                         opacity = 0.8, opacityNoHover = 1,
                         height = 700 , width = 700, bounded = F, zoom = T, charge = -1000)
  widget2png(rbokeh , file = paste0("latex/report/figure/Allinterbank", substr(combas.date[i],1,4), ".png"), timeout = 1200)
}
#################################################################
# combas desity and power law
#################################################################
load(file = "data/Rdata/latex_combas.Rdata")#2000:2016
max.matrix <- unlist(matrix) %>% max
matrix <- lapply(matrix,
                 FUN = function(x){
                   y <- x/max.matrix*20
                   return(y)})
# density
density.pl <- lapply(matrix, sum) %>% unlist
density.length <- (lapply(matrix, length) %>% unlist)^(1/2)
density.pl <- density.pl/density.length
density.pl <- xts(density.pl, as.Date(paste0(seq(2000,2016,1),"-06-30"), format='%Y-%m-%d'))

# largest 10
for (i in 9:17) {
  x <- matrix[[i]] %>% as.numeric
  plot(x[order(-x)])
}

# alpha of power law
xmin <- matrix %>% unlist
length(xmin)*0.1
xmin <- xmin[order(-xmin)][1:26000]
plot(xmin)
xmin <- xmin[order(-xmin)][52095]
alpha.pl <- lapply(matrix[9:17], FUN = function(x){
  y <- power.law.fit(x, xmin = NULL, start = 2)$alpha
  return(y)
})
alpha.pl <- xts(alpha.pl %>% unlist, as.Date(paste0(seq(2000,2016,1)[9:17],"-06-30"), format='%Y-%m-%d'))

dy.data <- merge(alpha.pl["2008-01-03/2016-12-30"],density.pl["2008-01-03/2016-12-30"], is.higher$crisis.sma20)
#dy.data <- merge(alpha.pl["2008-01-03/2016-12-30"],alpha.pl1["2008-01-03/2016-12-30"], is.higher$crisis.sma20)
#names(dy.data) <- c("0","1","Crisis")
names(dy.data) <- c("alpha of power law","density","Crisis")
dy.date <- index(dy.data) %>% as.character
dy.data <- apply(X = dy.data, MARGIN = 2,FUN = replace.na.near)
dy.data <- xts(dy.data, as.Date(dy.date, format='%Y-%m-%d'))

dy.main <- "structure"
color <- c("blue","red","gray")#colorRampPalette(c("red", "blue"))(3)
dygraph.interbank(dy.data = dy.data, dy.main, color, begin.dateWindow = index(dy.data)[1]) %>%
  dyAxis("y", label = "Coefficients", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2") %>%
  dyLimit(1, color = "gray") %>%
  dyLimit(2, color = "gray") %>%
  dyAxis("y", valueRange = c(0, 2.5)) %>% 
  dyShading(from = 1, to = 2, color = "#FFFAF0", axis = "y")
#################################################################
# trimed interbank 10
#################################################################
temp <- p.matrix[[length(p.matrix)]]/max.matrix 
colnames(temp) <- bank10.abbr;rownames(temp) <- bank10.abbr
if(max(temp)>0.02){temp[temp < 0.02] <- 0}
igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
networkD3.matrix[[2]]$group <- group.stockprice[bank10.abbr,"Eclass"]

temp.size <- rowSums(temp)
temp.size[temp.size < 0.4] <- 0.4
networkD3.matrix[[2]]$size <- temp.size
networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*10
networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$value*10
matrix.date <- data.frame(name="",size=0, group=as.character(substr(combas.date[i],1,4)))
networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)

ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white"]);'
rbokeh <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                       Source = "source", Target = "target", Value = "value", 
                       arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                       NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                       Group = "group", legend = T, fontSize = 10,
                       opacity = 0.8, opacityNoHover = 1,
                       height = 700 , width = 700, bounded = F, zoom = T, charge = -1000)
widget2png(rbokeh , file = paste0("latex/report/figure/Trimedinterbank", substr(combas.date[i],1,4), ".png"), timeout = 1200)
#################################################################
# Number of Banks Participate in Interbank Market
#################################################################
raw.data <- read_excel(path = "data/bank_combas.xlsx",
                       sheet = 1,
                       skip = 0,
                       col_names = T,
                       col_types = c("numeric", "text", "numeric", "date", "text", rep("numeric", 67))) %>% as.data.frame
names(raw.data) <- c("银行代码", "银行中文简称", "股票代码", "会计期间", "报表类型编码", "现金及存放中央银行款项", "贵金属", "存放同业款项", "拆出资金净额", "交易性金融资产", "衍生金融资产", "买入返售金融资产净额", "应收款项类投资", "其他应收款净额", "应收利息净额", "应收股利净额", "发放贷款及垫款净额", "定期存款", "可供出售金融资产净额", "持有至到期投资净额", "长期股权投资净额", "投资性房地产净额", "固定资产净额", "在建工程净额", "固定资产清理", "无形资产净额", "商誉净额", "长期待摊费用", "递延所得税资产", "其他资产", "资产总计", "向中央银行借款", "拆入资金", "吸收存款及同业存放", "其中：同业及其他金融机构存放款项", "其中：吸收存款", "短期借款", "交易性金融负债", "衍生金融负债", "卖出回购金融资产款", "应付职工薪酬", "应交税费", "应付利息", "应付股利", "递延收益-流动负债", "其他应付款", "长期借款", "应付债券", "预计负债", "递延收益-非流动负债", "递延所得税负债", "其他负债", "负债合计", "实收资本(或股本)", "其他权益工具", "其中：优先股", "其中：永续债", "其中：其他", "资本公积", "减：库存股", "专项储备", "其他综合收益", "盈余公积", "未分配利润", "外币报表折算差额", "未确认的投资损失", "一般风险准备", "交易风险准备", "归属于母公司所有者权益合计", "少数股东权益", "所有者权益合计", "负债与所有者权益总计")
combas.date <- unique(raw.data$会计期间)
combas.date <- combas.date[order(combas.date)]
number.banks <- c()
for (i in 1:length(combas.date)) {
  temp <- filter(raw.data,  报表类型编码 == "A" & 会计期间 == combas.date[[i]]) %>% nrow
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
dygraph.interbank(dy.data, dy.main, color, begin.dateWindow =  as.Date("2006-01-01", format='%Y-%m-%d')) %>% 
  dySeries("crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5 ,axis = "y2") %>%
  dyOptions(stepPlot = TRUE) %>%
  dyAxis("y", label = "Number of Participants in China's Interbank Market", independentTicks = TRUE) %>%
  dyAxis("y2", label = "Crisis", independentTicks = TRUE, drawGrid = F)

#################################################################
# save data used for drawing for combas
#################################################################
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
raw.data <- filter(raw.data,  报表类型编码 == "A")
save(raw.data, file = "data/Rdata/latex_draw_combas.Rdata")
#################################################################
# data draw of interbank market loan
#################################################################
load(file = "data/Rdata/latex_draw_combas.Rdata")
raw.data <- raw.data[,c("银行中文简称", "会计期间","拆出资金净额","拆入资金")]
raw.data <- melt(raw.data, id = c("会计期间", "银行中文简称"), variable.name = "Term", value.name = "Value", na.rm=T)
raw.data <- dcast(raw.data, 会计期间 + Term ~ 银行中文简称, fun.aggregate = mean) %>% as.data.frame
raw.data[is.na(raw.data)] <- 0
raw.data$sum.all <- rowSums(raw.data[,-c(1,2)])
raw.data$sum.10 <- rowSums(raw.data[,c("交通银行", "工商银行", "建设银行", "中国银行", "平安银行", "浦发银行", "华夏银行", "民生银行", "招商银行", "兴业银行", "中信银行", "宁波银行", "南京银行", "北京银行")])
#lending
interbank.l <- raw.data[raw.data$Term =="拆出资金净额",c("会计期间","交通银行", "工商银行", "建设银行", "中国银行", "浦发银行", "华夏银行", "招商银行","兴业银行", "中信银行", "北京银行","sum.all","sum.10")] 
interbank.l$会计期间 <- paste0(c(2000:2016),c("-06-30"))
interbank.l <- xts(interbank.l[,-1], as.Date(interbank.l$会计期间, format='%Y-%m-%d'))
names(interbank.l)[1:10] <- bank10.abbr
dy.data <- merge(interbank.l[,bank10.abbr]/10^12, is.higher$crisis.sma20)#scale#
temp <- apply(X = dy.data, MARGIN = 2, FUN = replace.na.near)
dy.data <- xts(temp, as.Date(index(dy.data), format='%Y-%m-%d'))
colnames(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Interbank Lending"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = as.Date("2008-01-01", format='%Y-%m-%d')) %>%
  dyAxis("y", label = "Lending", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")

#borrowing
interbank.b <- raw.data[raw.data$Term =="拆入资金",c("会计期间","交通银行", "工商银行", "建设银行", "中国银行", "浦发银行", "华夏银行", "招商银行","兴业银行", "中信银行", "北京银行","sum.all","sum.10")] 
interbank.b$会计期间 <- paste0(c(2000:2016),c("-06-30"))
interbank.b <- xts(interbank.b[,-1], as.Date(interbank.b$会计期间, format='%Y-%m-%d'))
names(interbank.b)[1:10] <- bank10.abbr
dy.data <- merge(interbank.b[,bank10.abbr]/10^12, is.higher$crisis.sma20)#scale#
temp <- apply(X = dy.data, MARGIN = 2, FUN = replace.na.near)
dy.data <- xts(temp, as.Date(index(dy.data), format='%Y-%m-%d'))
colnames(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Interbank Borrowing"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = as.Date("2008-01-01", format='%Y-%m-%d')) %>%
  dyAxis("y", label = "Borrowing", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#proportion
interbank.p <- merge(interbank.l$sum.10/interbank.l$sum.all,interbank.b$sum.10/interbank.b$sum.all)
names(interbank.p) <- c("Lending","Borrowing")
dy.data <- merge(interbank.p, is.higher$crisis.sma20)
temp <- apply(X = dy.data, MARGIN = 2, FUN = replace.na.near)
dy.data <- xts(temp, as.Date(index(dy.data), format='%Y-%m-%d'))
colnames(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Loan Proportion"
color <- c("red","blue","gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = as.Date("2008-01-01", format='%Y-%m-%d')) %>%
  dyAxis("y", label = "Proportion", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2") %>%
  dyAxis("y", valueRange = c(0.3, 1.1))
#################################################################
# data draw of interbank market deposit
#################################################################
load(file = "data/Rdata/latex_draw_combas.Rdata")
raw.data <- filter(raw.data,  报表类型编码 == "A")
raw.data <- raw.data[,c("银行中文简称", "会计期间","存放同业款项","其中：同业及其他金融机构存放款项")]
raw.data <- melt(raw.data, id = c("会计期间", "银行中文简称"), variable.name = "Term", value.name = "Value", na.rm=T)
raw.data <- dcast(raw.data, 会计期间 + Term ~ 银行中文简称, fun.aggregate = mean) %>% as.data.frame
raw.data[is.na(raw.data)] <- 0
raw.data$sum.all <- rowSums(raw.data[,-c(1,2)])
raw.data$sum.10 <- rowSums(raw.data[,c("交通银行", "工商银行", "建设银行", "中国银行", "平安银行", "浦发银行", "华夏银行", "民生银行", "招商银行", "兴业银行", "中信银行", "宁波银行", "南京银行", "北京银行")])
#lending
interbank.l <- raw.data[raw.data$Term =="存放同业款项",c("会计期间","交通银行", "工商银行", "建设银行", "中国银行", "浦发银行", "华夏银行", "招商银行","兴业银行", "中信银行", "北京银行","sum.all","sum.10")] 
interbank.l$会计期间 <- paste0(c(2000:2016),c("-06-30"))
interbank.l <- xts(interbank.l[,-1], as.Date(interbank.l$会计期间, format='%Y-%m-%d'))
names(interbank.l)[1:10] <- bank10.abbr
dy.data <- merge(interbank.l[,bank10.abbr]/10^12, is.higher$crisis.sma20)#scale#
temp <- apply(X = dy.data, MARGIN = 2, FUN = replace.na.near)
dy.data <- xts(temp, as.Date(index(dy.data), format='%Y-%m-%d'))
colnames(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Interbank Deposit Lending"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = as.Date("2008-01-01", format='%Y-%m-%d')) %>%
  dyAxis("y", label = "Deposit Lending", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")

#borrowing
interbank.b <- raw.data[raw.data$Term =="其中：同业及其他金融机构存放款项",c("会计期间","交通银行", "工商银行", "建设银行", "中国银行", "浦发银行", "华夏银行", "招商银行","兴业银行", "中信银行", "北京银行","sum.all","sum.10")] 
interbank.b$会计期间 <- paste0(c(2000:2016),c("-06-30"))
interbank.b <- xts(interbank.b[,-1], as.Date(interbank.b$会计期间, format='%Y-%m-%d'))
names(interbank.b)[1:10] <- bank10.abbr
dy.data <- merge(interbank.b[,bank10.abbr]/10^12, is.higher$crisis.sma20)#scale#
temp <- apply(X = dy.data, MARGIN = 2, FUN = replace.na.near)
dy.data <- xts(temp, as.Date(index(dy.data), format='%Y-%m-%d'))
colnames(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Interbank Deposit Borrowing"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = as.Date("2008-01-01", format='%Y-%m-%d')) %>%
  dyAxis("y", label = "Deposit Borrowing", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#proportion
interbank.p <- merge(interbank.l$sum.10/interbank.l$sum.all,interbank.b$sum.10/interbank.b$sum.all)
names(interbank.p) <- c("Deposit Lending","Deposit Borrowing")
dy.data <- merge(interbank.p, is.higher$crisis.sma20)
temp <- apply(X = dy.data, MARGIN = 2, FUN = replace.na.near)
dy.data <- xts(temp, as.Date(index(dy.data), format='%Y-%m-%d'))
colnames(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Deposit Proportion"
color <- c("red","blue","gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = as.Date("2008-01-01", format='%Y-%m-%d')) %>%
  dyAxis("y", label = "Proportion", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2") %>%
  dyAxis("y", valueRange = c(0.3, 1.01))
#################################################################
# proportion of deposit and loan in interbank market
#################################################################
load(file = "data/Rdata/latex_draw_combas.Rdata")
raw.data <- raw.data[,c("银行中文简称", "会计期间","拆出资金净额","拆入资金","存放同业款项","其中：同业及其他金融机构存放款项")]
raw.data <- melt(raw.data, id = c("会计期间", "银行中文简称"), variable.name = "Term", value.name = "Value", na.rm=T)
raw.data[is.na(raw.data)] <- 0
p.names <- c("交通银行", "工商银行", "建设银行", "中国银行", "浦发银行", "华夏银行", "招商银行","兴业银行", "中信银行", "北京银行")

compound <- dcast(raw.data, 会计期间 ~ 银行中文简称, fun.aggregate = sum) %>% as.data.frame
p.compound <- compound[,p.names] %>% rowSums
a.compound <- compound[,-1] %>% rowSums

loan <- raw.data[raw.data$Term=="拆出资金净额"|raw.data$Term=="拆入资金",]
loan <- dcast(loan, 会计期间 ~ 银行中文简称, fun.aggregate = sum) %>% as.data.frame
p.loan <- loan[,p.names] %>% rowSums
a.loan <- loan[,-1] %>% rowSums

deposit <- raw.data[raw.data$Term=="存放同业款项"|raw.data$Term=="其中：同业及其他金融机构存放款项",]
deposit <- dcast(deposit, 会计期间 ~ 银行中文简称, fun.aggregate = sum) %>% as.data.frame
p.deposit <- deposit[,p.names] %>% rowSums
a.deposit <- deposit[,-1] %>% rowSums

draw <- data.frame("Interbank Deposit" = round(p.deposit/a.compound*100), "Interbank Loan" = round(p.loan/a.compound*100), Year = 2000:2016)
draw <- melt(draw, id = c("Year"), variable.name = "Type", value.name = "Proportion", na.rm=T)

ggplot(data = draw, aes(x = Year, y = Proportion, fill = Type, frame = Year, cumulative = TRUE)) +
  geom_col() +
  labs(x = "Year", y = "Proportion", title = "", subtitle = "") +
  geom_text(aes(label = Proportion), color = "Black", size = 5, position = position_stack(vjust = 0.5)) + 
  #theme(axis.text.x = element_text(colour="Black",size=15), axis.text.y = element_text(colour="Black",size=15)) +
  theme_minimal()+
  scale_fill_manual(values=c("#FFBFC0","#BFC0FA"))
#################################################################
# data decription of loan and deposit data used to estimate networks
#################################################################
load(file = "data/Rdata/latex_draw_combas.Rdata")
raw.data <- raw.data[,c("银行中文简称", "会计期间","拆出资金净额","拆入资金","存放同业款项","其中：同业及其他金融机构存放款项")]
raw.data <- melt(raw.data, id = c("会计期间", "银行中文简称"), variable.name = "Term", value.name = "Value", na.rm=T)
raw.data <- raw.data[raw.data$会计期间 > "2008-12-31",]
raw.data[is.na(raw.data)] <- 0
raw.data <- dcast(raw.data, 会计期间 + 银行中文简称 ~ Term, fun.aggregate = mean) %>% as.data.frame
raw.data[is.na(raw.data)] <- 0
raw.data[raw.data < 0] <- 0
#sum(raw.data < 0)
data <- raw.data[,-c(1,2)]/(10^10)#scale#
names(data) <- c("lnd","brr","dlnd","dbrr")
summary.data <- sapply(data, each(min, max, median, mean, sd, skewness, kurtosis))# * c(100,100,10000,10000,100,1,1)
summary.data <- summary.data %>% round(2) %>% t
names(summary.data) <- c("min", "max", "median", "mean", "sd", "skewness", "kurtosis")
write.xlsx(summary.data, file = "latex/report/excel/summary_interbank.xlsx", row.names = TRUE)

summary.data <- xtable(summary.data, caption = "Data Summary of Interbank Loan and Interbank Deposit",
                       label = "tab:summary_interbank"
)
align(summary.data) <- "llllllll"
print(summary.data, 
      file="latex/report/table/summary_interbank.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
