#################################################################
# This is code for figure and table for latex
# should be run with ForestData 
#################################################################
#################################################################
# preparation
#################################################################
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
n.sp <- bank10.abbr
n.g.sp <- paste0(n.sp,".g")
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
#################################################################
# load sp group
#################################################################
group.stockprice <- read_excel(path = "data/stockprice.xlsx",
                               sheet = "group",
                               skip = 0,
                               col_names = T,
                               col_types = rep("text", 5)) %>% as.data.frame
group.stockprice$Eclass <- factor(group.stockprice$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
group.stockprice <- group.stockprice[order(group.stockprice$Eclass,decreasing = F),] 
rownames(group.stockprice) <- group.stockprice$Abbr
group.stockprice$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(10)[-10], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6])
#################################################################
# dygraph for SHIBOR
#################################################################
## shibor rate
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
#y <- table(x) View(y)

data <- cbind(shibor[,-1], as.data.frame(is.higher$crisis.sma20))
names(data)[ncol(data)] <- "Crisis"
dy.data <- xts(data, as.Date(shibor$Date, format='%Y-%m-%d'))
dy.main <- "Shanghai Interbank Offered Rate from 2006 to 2018"
color <- c(colorRampPalette(c("red", "white"))(5)[-5],colorRampPalette(c("blue", "white"))(5)[-5], "black")
dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Shanghai Interbank Offered Rate", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#################################################################
# summary shibor
#################################################################
su.shibor <- shibor[,-1]
names(su.shibor) <- c("O/N","W1","W2","M1","M3","M6","M9","Y1")
summary.shibor <- sapply(su.shibor %>% na.omit, each(min, max, median, mean, sd, skewness, kurtosis))# * c(100,100,10000,10000,100,1,1)
summary.shibor <- summary.shibor %>% round(2) %>% t
names(summary.shibor) <- c("min", "max", "median", "mean", "sd", "skewness", "kurtosis")
write.xlsx(summary.shibor, file = "latex/report/excel/summary_shibor.xlsx", row.names = TRUE)

summary.shibor <- xtable(summary.shibor, caption = "Data Summary of Shibor",
                       label = "tab:summary_shibor"
)
align(summary.shibor) <- "llllllll"
print(summary.shibor, 
      file="latex/report/table/summary_shibor.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
#################################################################
# load data for :combas draw loan and deposit network
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
  #View(data[locate,]$存放同业款项)
  #maximum entropy
  locate <- sapply(group.stockprice[bank10.abbr,"Cname"], grepl, data$银行中文简称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  #
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$拆出资金净额, colsums = data$拆入资金, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  rownames(EstimatedMatrix.me) <- data$银行中文简称
  colnames(EstimatedMatrix.me) <- data$银行中文简称
  matrix[[i]] <- EstimatedMatrix.me
  p.matrix[[i]] <- EstimatedMatrix.me[locate,locate]
  #
  EstimatedMatrix.me <- matrix_estimation(rowsums = data$存放同业款项, colsums = data$`其中：同业及其他金融机构存放款项`, method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  rownames(EstimatedMatrix.me) <- data$银行中文简称
  colnames(EstimatedMatrix.me) <- data$银行中文简称
  d.matrix[[i]] <- EstimatedMatrix.me
  p.d.matrix[[i]] <- EstimatedMatrix.me[locate,locate]
  
  data.interbank[[i]] <- data
}
save(matrix, p.matrix, d.matrix, p.d.matrix,file = "data/Rdata/latex_combas.Rdata")
#################################################################
# proportion loan and deposit interbank network
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
#################################################################
# bank list
#################################################################
appendix.group <- group.stockprice[bank10.abbr,c("Abbr","Ename", "Eclass")]
names(appendix.group) <- c("Abbr","Full Name","Category")
appendix.group <- xtable(appendix.group, caption = "Bank List of the Sample",
                         label = "tab:BankList") 

align(appendix.group) <- paste0(rep("l",ncol(appendix.group)+1), collapse = "")
print(appendix.group, 
      file=paste0("latex/report/table/BankList.tex"), 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      include.rownames = FALSE,
      include.colnames = TRUE)

#################################################################
# wind scale
#################################################################
group.stockprice[bank10.abbr,"Cname"]
group.wind <- c("交通银行股份有限公司", 
                "中国工商银行股份有限公司", 
                "中国建设银行股份有限公司", 
                "中国银行股份有限公司", 
                #"平安银行股份有限公司", 
                "上海浦东发展银行股份有限公司", 
                "华夏银行股份有限公司", 
                #"中国民生银行股份有限公司", 
                "招商银行股份有限公司", 
                "兴业银行股份有限公司", 
                "中信银行股份有限公司", 
                #"宁波银行股份有限公司", 
                #"南京银行股份有限公司", 
                "北京银行股份有限公司")

wind.name <- read_excel(path = "data/windname.xlsx",sheet = "name",
                        skip = 0,
                        col_names = F,
                        col_types = c("text", "text" , "text")) %>% as.data.frame

fnlist.wind <- dir("data/wind")
raw.wind <- list()
raw.wind.full <- list()
length.fnlist <- length(fnlist.wind)
for(i in 1:length.fnlist){
  temp <- read_excel(path = paste0("data/wind/",fnlist.wind[i]),
                     sheet = "万得",
                     skip = 0,
                     col_names = T,
                     col_types = c("text", "text", rep("numeric", 55), "date")) %>% as.data.frame
  temp <- temp[temp$数据来源=="合并报表",]
  #print(paste("lending",length(temp$`拆出资金(万元)` %>% na.omit), "in",i))
  #print(paste("borrowing",length(temp$`拆入资金(万元)` %>% na.omit), "in",i))

  temp$`向中央银行借款(万元)`[is.na(temp$`向中央银行借款(万元)`)] <- 0
  temp$`向中央银行借款净增加额(万元)`[is.na(temp$`向中央银行借款净增加额(万元)`)] <- 0
  temp$`向其他金融机构拆入资金净增加额(万元)`[is.na(temp$`向其他金融机构拆入资金净增加额(万元)`)] <- 0
  temp$`客户存款和同业存放款项净增加额(万元)`[is.na(temp$`客户存款和同业存放款项净增加额(万元)`)] <- 0
  temp$`客户贷款及垫款净增加额(万元)`[is.na(temp$`客户贷款及垫款净增加额(万元)`)] <- 0
  temp <- subset(temp, select =  -`应付利息(万元)`)
  temp <- subset(temp, select =  -`应收利息(万元)`)
  #temp <- subset(temp, select =  -`基本每股收益(万/股)`)
  #temp <- subset(temp, select =  -`稀释每股收益(元/股)`)
  temp$`基本每股收益(万/股)` <- temp$`基本每股收益(万/股)` * (10^5)/10000*(12^10)#scale#
  temp$`稀释每股收益(元/股)` <- temp$`稀释每股收益(元/股)` * (10^5)/10000*(12^10)#scale#
  #temp$`应付利息(万元)` <- temp$`应付利息(万元)` /(10^6)/10000*(12^10)#scale#
  #temp$`应收利息(万元)` <- temp$`应收利息(万元)` /(10^6)/10000*(12^10)#scale#
  temp.title <- temp[,c("公司名称","数据来源","报告期")]
  temp <- subset(temp, select =  -`报告期`)
  temp <- subset(temp, select =  -`公司名称`)
  temp <- subset(temp, select =  -`数据来源`)
  
  #print(temp %>% is.na %>% sum)
  onames <- names(temp)
  names(temp) <- wind.name[,2][match(onames,wind.name[,1])]
  temp <- temp*10000/(12^10)#scale#
  #log(temp[1,] %>% abs,10) %>% ceiling
  #log(temp$asst %>% abs,10) %>% ceiling
  raw.wind.full[[i]] <- temp
  locate <- sapply(group.wind, grepl, temp.title$公司名称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  temp <- temp[locate,]
  rownames(temp) <- bank10.abbr
  raw.wind[[i]] <- temp
}
raw.wind <- F.fill.up.NAs(raw.wind)
raw.wind.full <- F.fill.up.NAs(raw.wind.full)

raw.wind %>% unlist %>%is.na %>% sum
raw.wind.full %>%unlist%>% is.na %>% sum

for (i in 1:length(raw.wind)) {
  raw.wind[[i]]$clnd <- raw.wind[[i]]$lnd + raw.wind[[i]]$dlnd
  raw.wind[[i]]$cbrr <- raw.wind[[i]]$brr + raw.wind[[i]]$dbrr
}

for (i in 1:length(raw.wind.full)) {
  raw.wind.full[[i]]$clnd <- raw.wind.full[[i]]$lnd + raw.wind.full[[i]]$dlnd
  raw.wind.full[[i]]$cbrr <- raw.wind.full[[i]]$brr + raw.wind.full[[i]]$dbrr
}

wind.full <- raw.wind.full[[1]]
for (i in 2:length(raw.wind)) {
  temp <- raw.wind.full[[i]]
  wind.full <- rbind(wind,temp)
}
wind.full[is.na(wind.full)] <- 0

wind <- raw.wind[[1]]
for (i in 2:length(raw.wind)) {
  temp <- raw.wind[[i]]
  wind <- rbind(wind,temp)
}

save(raw.wind, raw.wind.full, wind, wind.full, file = "data/Rdata/latex_raw.wind.Rdata")
load(file = "data/Rdata/latex_raw.wind.Rdata")
#################################################################
# plot cor wind
#################################################################
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
load(file = "data/Rdata/latex_raw.wind.Rdata")
library(RColorBrewer)
library(corrplot)
res1 <- cor.mtest(wind, conf.level = .95)
corrplot(cor(wind), p.mat = res1$p, insig = "blank",
         method = "color",
         #col = brewer.pal(n = 8, name = "PuOr"),
         type = "lower",
         tl.col = "black", #tl.srt = 90, 
         tl.cex = 1,
         diag = FALSE
)
#
selected.wind <- c("asst","lblt","lnd","cbrr","brr","dlnd","dbrr","nprf", "grp")
wind <- wind[,selected.wind]
res1 <- cor.mtest(wind, conf.level = .95)
corrplot(cor(wind), p.mat = res1$p, insig = "blank",
         method = "color",
         #type = "lower",
         tl.col = "black", tl.srt = 90,
         diag = FALSE
)
#################################################################
# summary wind
#################################################################
load(file = "data/Rdata/latex_raw.wind.Rdata")

selected.wind <- c("asst","lblt","lnd","brr","dlnd","dbrr","clnd","cbrr","nprf", "grp")
wind <- wind[,selected.wind]

summary.wind <- sapply(wind %>% na.omit, each(min, max, median, mean, sd, skewness, kurtosis))# * c(100,100,10000,10000,100,1,1)
summary.wind <- summary.wind %>% round(2) %>% t
names(summary.wind) <- c("min", "max", "median", "mean", "sd", "skewness", "kurtosis")
write.xlsx(summary.wind, file = "latex/report/excel/summary_wind.xlsx", row.names = TRUE)

summary.wind <- xtable(summary.wind, caption = "Data Summary of Banks' Operating Indicators",
                       label = "tab:summary_wind"
)
align(summary.wind) <- "llllllll"
print(summary.wind, 
      file="latex/report/table/summary_wind.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
#################################################################
# Correlation table with significance indicators for wind data
#################################################################
load(file = "data/Rdata/latex_raw.wind.Rdata")
selected.wind <- c("asst","lblt","lnd","brr","dlnd","dbrr","clnd","cbrr","nprf", "grp")
wind <- wind[,selected.wind]
temp.corr <- corstar(wind)[-seq(2,length(selected.wind)*2,2),-1]
rownames(temp.corr) <- colnames(temp.corr)
temp.corr[upper.tri(temp.corr)] <- ""
diag(temp.corr) <- ""

correlation.table <- xtable(temp.corr, caption = "Correlation Matrix of Key Indicators",
                       label = "tab:correlation_table"
)
align(correlation.table) <-  combine_words(rep("l",dim(correlation.table)[2]+1),sep="",and = "")

print(correlation.table, 
      file="latex/report/table/correlation_table.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
#################################################################
# g.sp data description
#################################################################
names(g.sp) <- bank10.abbr
summary.g.sp <- sapply(g.sp * 100, each(min, max, median, mean, sd, skewness, kurtosis))# * c(100,100,10000,10000,100,1,1)#scale#
summary.g.sp <- summary.g.sp %>% round(2) %>% t
names(summary.g.sp) <- c("min", "max", "median", "mean", "sd", "skewness", "kurtosis")
write.xlsx(summary.g.sp, file = "latex/report/excel/summary_g_sp.xlsx", row.names = TRUE)

summary.g.sp <- xtable(summary.g.sp, caption = "Data Summary of Stock Price in Log Difference",
                       label = "tab:summary_g_sp"
)
align(summary.g.sp) <- "llllllll"
print(summary.g.sp, 
      file="latex/report/table/summary_g_sp.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
#################################################################
# dygraph for sp
#################################################################
dy.data <- cbind(sp[,bank10.abbr], is.higher$crisis.sma20) %>% na.omit
names(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Stock Price"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color,begin.dateWindow = index(sp)[1]) %>%
  dyAxis("y", label = "Stock Price", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#################################################################
# network for sp
#################################################################
vecm.gir0 <- dy.VECM.GIR(data = sp["2007-01-01/2016-12-31"], n.ahead = 0, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir0 <- matrix(data = unlist(vecm.gir0),nrow =10,ncol = 10)
vecm.gir1 <- dy.VECM.GIR(data = sp["2007-01-01/2016-12-31"], n.ahead = 1, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir1 <- matrix(data = unlist(vecm.gir1),nrow =10,ncol = 10)
vecm.gir2 <- dy.VECM.GIR(data = sp["2007-01-01/2016-12-31"], n.ahead = 2, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir2 <- matrix(data = unlist(vecm.gir2),nrow =10,ncol = 10)
vecm.gir3 <- dy.VECM.GIR(data = sp["2007-01-01/2016-12-31"], n.ahead = 3, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir3 <- matrix(data = unlist(vecm.gir3),nrow =10,ncol = 10)
vecm.gir4 <- dy.VECM.GIR(data = sp["2007-01-01/2016-12-31"], n.ahead = 4, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir4 <- matrix(data = unlist(vecm.gir4),nrow =10,ncol = 10)
vecm.gir5 <- dy.VECM.GIR(data = sp["2007-01-01/2016-12-31"], n.ahead = 5, span = "yearly", keep.vecm = T, rank = 4)[[1]];vecm.gir5 <- matrix(data = unlist(vecm.gir5),nrow =10,ncol = 10)

GIR <- array(c(vecm.gir0,
               vecm.gir1,
               vecm.gir2,
               vecm.gir3,
               vecm.gir4,
               vecm.gir5), dim = c(10,10,6))
temp <- weighted_gir(GIR, divided=1)$weighted.matrix
temp <- matrix(temp %>% unlist,10,10) #%>% t
colnames(temp) <- bank10.abbr;rownames(temp) <- bank10.abbr
temp <- temp %>% t
temp <- temp/max(temp)
if(max(temp)>0.02){temp[temp < 0.02] <- 0}
igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
networkD3.matrix[[2]]$group <- group.stockprice[bank10.abbr,c("Eclass")] %>% as.character

temp.size <- rowSums(temp)-colSums(temp)
temp.size[temp.size < 0] <- min(temp.size[temp.size > 0])/2
networkD3.matrix[[2]]$size <- temp.size
#networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks"))
networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*5
networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu*4
matrix.date <- data.frame(name="",size=0, group=as.character(substr(combas.date[i],1,4)))
networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)

ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white"]);'
#rbokeh.interbank <- 
rbokeh <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                       Source = "source", Target = "target", Value = "value", 
                       arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                       NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                       Group = "group", legend = T, fontSize = 15, opacity = 0.8, opacityNoHover = 1,
                       height = 700 , width = 700, bounded = F, zoom = T, charge = -100)
widget2png(rbokeh , file = paste0("latex/report/figure/snVECMw", ".png"), timeout = 1200)
#################################################################
# ADF test for sp
#################################################################
table.ADFtest <- rbind(
  ADFtest.tau3(sp, selectlags = "AIC")[[1]],
  ADFtest.tau3(d.sp, selectlags = "AIC")[[1]],
  ADFtest.tau3(sp, selectlags = "BIC")[[1]],
  ADFtest.tau3(d.sp, selectlags = "BIC")[[1]]
) %>% t


table.ADFtest <- xtable(table.ADFtest, caption = "Unit Root Tests",
                        label = "tab:table_ADFtest")

addtorow <- list(pos = list(0, 0,0),
                 command = c("& \\multicolumn{4}{c}{AIC} & \\multicolumn{4}{c}{BIC}\\\\\n",
                             "  & \\multicolumn{2}{c}{I(0)} & \\multicolumn{2}{c}{I(1)} & \\multicolumn{2}{c}{I(0)} & \\multicolumn{2}{c}{I(1)}\\\\\n",
                             "  & T & lags & T & lags & T & lags & T & lags \\\\\n"
                 ) )
align(table.ADFtest) <- "lllllllll"
print(table.ADFtest, 
      file="latex/report/table/table_ADFtest.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      hline.after = c(-1,0, nrow(table.ADFtest)),
      add.to.row = addtorow,
      include.colnames = FALSE
)


table.ADFtest.xlsx <- rbind(
  ADFtest.tau3(sp, selectlags = "AIC")[[2]],
  ADFtest.tau3(d.sp, selectlags = "AIC")[[2]],
  ADFtest.tau3(sp, selectlags = "BIC")[[2]],
  ADFtest.tau3(d.sp, selectlags = "BIC")[[2]]
) %>% t
write.xlsx(table.ADFtest.xlsx, file = "latex/report/excel/table.ADFtest.xlsx", row.names = TRUE)
#################################################################
# cointegration test for sp
#################################################################
data <- sp
group.cointest <- bank10.abbr
tab.label <- "cointest"
model <- process.genFEVD.VECMorVAR(data = sp, group=group.cointest, start.point = "2007-09-25", end.point = "2018-1-19", n.ahead=1)
data <- model$data
vecm.tsDyn <- model$rank.test
#vecm.tsDyn <- VECM(data, lag = K, estim="ML",include = c("const"))
COIN.rank.test <- rank.test(vecm.tsDyn)
COIN.res <- COIN.rank.test$res_df #COIN.eigen$r #COIN.trace$r #COIN.eigen$pval #COIN.trace$pval
table.n <- nrow(COIN.res)
K <- model$K
COIN.AIC <- rank.select(data, include = c("none"))$AICs[1:table.n, K] 
COIN.BIC <- rank.select(data, include = c("none"))$BICs[1:table.n, K] 

trace <- c(3.76, 15.41, 29.68, 47.21, 68.52, 94.15, 124.24, 156, 192.89, 233.13, 277.71, NA, NA)#14variable
lambda <- c(3.76, 14.07, 20.97, 27.07, 33.46, 39.37, 45.28, 51.42, 57.12, 62.81, 68.83, NA, NA)
# cointegratin test table in latex version      
table.COINtest <- data.frame(matrix(NA,nrow = table.n,ncol = 8))
names(table.COINtest) <- c("$H0<=r$", "trace", "CL for trace","eigen","CL for eigen","$H0=r$", "AIC", "BIC")
table.COINtest$"$H0<=r$" <- COIN.res$r
table.COINtest$"$H0=r$" <- COIN.res$r
table.COINtest$"AIC" <- COIN.AIC
table.COINtest$"BIC" <- COIN.BIC
table.COINtest$"CL for trace" <- trace[14:(14-table.n+1)]
table.COINtest$"CL for eigen" <- lambda[14:(14-table.n+1)]

for (i in 1:table.n) {
  if(COIN.res$trace_pval[i] < 0.1){table.COINtest$trace[i] <- paste(round(COIN.res$trace[i],2),"$^{*}$")}
  if(COIN.res$trace_pval[i] < 0.05){table.COINtest$trace[i] <- paste(round(COIN.res$trace[i],2),"$^{**}$")}
  if(COIN.res$trace_pval[i] < 0.01){table.COINtest$trace[i] <- paste(round(COIN.res$trace[i],2),"$^{***}$")}
  if(COIN.res$trace_pval[i] > 0.1){table.COINtest$trace[i] <- round(COIN.res$trace[i],2)}
  
  
  if(COIN.res$eigen_pval[i] < 0.1){table.COINtest$eigen[i] <- paste(round(COIN.res$eigen[i],2),"$^{*}$")}
  if(COIN.res$eigen_pval[i] < 0.05){table.COINtest$eigen[i] <- paste(round(COIN.res$eigen[i],2),"$^{**}$")}
  if(COIN.res$eigen_pval[i] < 0.01){table.COINtest$eigen[i] <- paste(round(COIN.res$eigen[i],2),"$^{***}$")}
  if(COIN.res$eigen_pval[i] > 0.1){table.COINtest$eigen[i] <- round(COIN.res$eigen[i],2)}
}#floor((rank.test(vecm.tsDyn, cval = 0.05, type = "eigen")$r+rank.test(vecm.tsDyn, cval = 0.05, type = "trace")$r)/2)

table.COINtest <- xtable(table.COINtest, caption = paste("Co-integration Test", tab.label,sep = " "),
                         label = paste0("tab:COINtest", tab.label)) 

align(table.COINtest) <- paste0(rep("l",9), collapse = "")
print(table.COINtest, 
      file=paste0("latex/report/table/COINtest", tab.label, ".tex"), 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      include.rownames = FALSE,
      include.colnames = TRUE)
# cointegratin test table in excel version      
table.COINtest.xlsx <- data.frame(matrix(NA,nrow = table.n,ncol = 8))
names(table.COINtest.xlsx) <- c("H0<=r", "trace", "CL for trace","eigen","CL for eigen","H0=r", "AIC", "BIC")
table.COINtest.xlsx$"H0<=r" <- COIN.res$r
table.COINtest.xlsx$"H0=r" <- COIN.res$r
table.COINtest.xlsx$"AIC" <- COIN.AIC
table.COINtest.xlsx$"BIC" <- COIN.BIC
table.COINtest.xlsx$"CL for trace" <- trace[14:(14-table.n+1)]
table.COINtest.xlsx$"CL for eigen" <- lambda[14:(14-table.n+1)]

for (i in 1:table.n) {
  if(COIN.res$trace_pval[i] < 0.1){table.COINtest.xlsx$trace[i] <- paste(round(COIN.res$trace[i],2),"*")}
  if(COIN.res$trace_pval[i] < 0.05){table.COINtest.xlsx$trace[i] <- paste(round(COIN.res$trace[i],2),"**")}
  if(COIN.res$trace_pval[i] < 0.01){table.COINtest.xlsx$trace[i] <- paste(round(COIN.res$trace[i],2),"***")}
  if(COIN.res$trace_pval[i] > 0.1){table.COINtest.xlsx$trace[i] <- round(COIN.res$trace[i],2)}
  
  
  if(COIN.res$eigen_pval[i] < 0.1){table.COINtest.xlsx$eigen[i] <- paste(round(COIN.res$eigen[i],2),"*")}
  if(COIN.res$eigen_pval[i] < 0.05){table.COINtest.xlsx$eigen[i] <- paste(round(COIN.res$eigen[i],2),"**")}
  if(COIN.res$eigen_pval[i] < 0.01){table.COINtest.xlsx$eigen[i] <- paste(round(COIN.res$eigen[i],2),"***")}
  if(COIN.res$eigen_pval[i] > 0.1){table.COINtest.xlsx$eigen[i] <- round(COIN.res$eigen[i],2)}
}
table.COINtest.xlsx[is.na(table.COINtest.xlsx)] <- ""
write.xlsx(table.COINtest.xlsx, file = paste0("latex/report/excel/COINtest", tab.label,".xlsx"), row.names = TRUE)
#################################################################
# spillover var gir
#################################################################
#daily.var.gir0 <- dy.VAR.GIR(data = g.sp, n.ahead = 0, mode = "fix", span = 250)[[1]]
#daily.var.gir1 <- dy.VAR.GIR(data = g.sp, n.ahead = 1, mode = "fix", span = 250)[[1]]
#daily.var.gir2 <- dy.VAR.GIR(data = g.sp, n.ahead = 2, mode = "fix", span = 250)[[1]]
#daily.var.gir3 <- dy.VAR.GIR(data = g.sp, n.ahead = 3, mode = "fix", span = 250)[[1]]
#daily.var.gir4 <- dy.VAR.GIR(data = g.sp, n.ahead = 4, mode = "fix", span = 250)[[1]]
#daily.var.gir5 <- dy.VAR.GIR(data = g.sp, n.ahead = 5, mode = "fix", span = 250)[[1]]
#save(daily.var.gir0, daily.var.gir1, daily.var.gir2, daily.var.gir3, daily.var.gir4, daily.var.gir5, g.sp, file = "data/Rdata/latex_daily.var.gir.Rdata")
load(file = "data/Rdata/latex_daily.var.gir.Rdata")
index.net <- c()
index.to <- c()
index.from <- c()
for (i in 1:length(daily.var.gir0)) {
  GIR <- array(c(daily.var.gir0[[i]],
                 daily.var.gir1[[i]],
                 daily.var.gir2[[i]],
                 daily.var.gir3[[i]],
                 daily.var.gir4[[i]],
                 daily.var.gir5[[i]]),dim = c(10,10,6))
  temp <- weighted_gir(GIR, divided=1)
  index.net <- rbind(index.net,temp$net_average)
  index.to <- rbind(index.to,temp$to_average)
  index.from <- rbind(index.from,temp$from_average)
}

dy.data <- xts(index.net, as.Date(index(sp)[-c(1:250)], format='%Y-%m-%d'))
dy.data <- merge(dy.data, is.higher$crisis.sma20) %>% na.omit
colnames(dy.data) <- c(bank10.abbr,"Crisis")
dy.main <- "Spillover Index VAR GIR"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Spillover Index", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#################################################################
# spillover vecm gir
#################################################################
try.error <- try(vecm.tsDyn <- VECM(sp, lag=2, estim="ML"),silent = TRUE)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
rank.test <- vecm.tsDyn
rank.eigen <- rank.test(vecm.tsDyn, cval = 0.01, type = "eigen")$r
rank.trace <- rank.test(vecm.tsDyn, cval = 0.01, type = "trace")$r
rank <- floor((rank.eigen + rank.trace)/2)#4

#daily.vecm.gir0 <- dy.VECM.GIR(data = sp, n.ahead = 0, mode = "fix", span = 250, keep.vecm = T,rank = 4)[[1]]
#daily.vecm.gir1 <- dy.VECM.GIR(data = sp, n.ahead = 1, mode = "fix", span = 250, keep.vecm = T,rank = 4)[[1]]
#daily.vecm.gir2 <- dy.VECM.GIR(data = sp, n.ahead = 2, mode = "fix", span = 250, keep.vecm = T,rank = 4)[[1]]
#daily.vecm.gir3 <- dy.VECM.GIR(data = sp, n.ahead = 3, mode = "fix", span = 250, keep.vecm = T,rank = 4)[[1]]
#daily.vecm.gir4 <- dy.VECM.GIR(data = sp, n.ahead = 4, mode = "fix", span = 250, keep.vecm = T,rank = 4)[[1]]
#daily.vecm.gir5 <- dy.VECM.GIR(data = sp, n.ahead = 5, mode = "fix", span = 250, keep.vecm = T,rank = 4)[[1]]
#save(daily.vecm.gir0, daily.vecm.gir1, daily.vecm.gir2, daily.vecm.gir3, daily.vecm.gir4, daily.vecm.gir5, sp, file = "data/Rdata/latex_daily.vecm.gir.Rdata")
load(file = "data/Rdata/latex_daily.vecm.gir.Rdata")

index.net <- c()
index.to <- c()
index.from <- c()
for (i in 1:length(daily.vecm.gir0)) {
  GIR <- array(c(daily.vecm.gir0[[i]],
                 daily.vecm.gir1[[i]],
                 daily.vecm.gir2[[i]],
                 daily.vecm.gir3[[i]],
                 daily.vecm.gir4[[i]],
                 daily.vecm.gir5[[i]]),dim = c(10,10,6))
  temp <- weighted_gir(GIR, divided=1)
  index.net <- rbind(index.net,temp$net_average)
  index.to <- rbind(index.to,temp$to_average)
  index.from <- rbind(index.from,temp$from_average)
}

dy.data <- xts(index.net, as.Date(index(sp), format='%Y-%m-%d')[-c(1:250)])
dy.data <- merge(dy.data, is.higher$crisis.sma20) %>% na.omit
colnames(dy.data) <- c(bank10.abbr,"Crisis")
dy.main <- "Spillover Index VECM GIR"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Spillover Index", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#################################################################
# spillover spill over index the result is the same with var gir
#################################################################
daily.spill <- index.spill(data = g.sp, mode = "fix", span = 250, divided = 1)
dy.data <- xts(daily.spill[[1]][-c(1:250),], as.Date(index(sp), format='%Y-%m-%d')[-c(1:251)])

dy.data <- merge(dy.data, is.higher$crisis.sma20) %>% na.omit
colnames(dy.data) <- c(bank10.abbr,"Crisis")
dy.main <- "Spillover Index of 10 listed Banks from 2007 to 2018"
color <- c(group.stockprice[bank10.abbr,"color"],"gray")
dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Spillover Index", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
#################################################################
# shibor BID
#################################################################
aenet.myl.ON <- aenet.fix(data = merge(all.bid.ON,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.W1 <- aenet.fix(data = merge(all.bid.W1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.W2 <- aenet.fix(data = merge(all.bid.W2,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.M1 <- aenet.fix(data = merge(all.bid.M1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.M3 <- aenet.fix(data = merge(all.bid.M3,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.M6 <- aenet.fix(data = merge(all.bid.M6,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.M9 <- aenet.fix(data = merge(all.bid.M9,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef
aenet.myl.Y1 <- aenet.fix(data = merge(all.bid.Y1,bond), fix = 250, start.point = "NULL",end.point = "NULL")$Coef

aenet.myl.long <- list()
aenet.myl.short <- list()
aenet.myl.all <- list()
for (i in 1:length(aenet.myl.ON)) {
  ON <- aenet.myl.ON[[i]];ON[is.na(ON)] <- 0
  W1 <- aenet.myl.W1[[i]];W1[is.na(W1)] <- 0
  W2 <- aenet.myl.W2[[i]];W2[is.na(W2)] <- 0
  M1 <- aenet.myl.M1[[i]];M1[is.na(M1)] <- 0
  M3 <- aenet.myl.M3[[i]];M3[is.na(M3)] <- 0
  M6 <- aenet.myl.M6[[i]];M6[is.na(M6)] <- 0
  M9 <- aenet.myl.M9[[i]];M9[is.na(M9)] <- 0
  Y1 <- aenet.myl.Y1[[i]];Y1[is.na(Y1)] <- 0
  aenet.myl.short[[i]] <- (ON+W1+W2+M1)/4
  aenet.myl.long[[i]] <- (M3+M6+M9+Y1)/4
  aenet.myl.all[[i]] <- (ON+W1+W2+M1+M3+M6+M9+Y1)/8
  ##print(i)
}
for (i in 1:n) {
  temp <- aenet.myl.all[[i]][1:10,1:10]
  if(sum(temp==0)==100){
    print(i)
  }
}

n <- length(aenet.myl.ON)
locate.na.ON <- c()
locate.na.W1 <- c()
locate.na.W2 <- c()
locate.na.M1 <- c()
locate.na.M3 <- c()
locate.na.M6 <- c()
locate.na.M9 <- c()
locate.na.Y1 <- c()
for (i in 1:n) {
  temp <- aenet.myl.ON[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.ON <- c(locate.na.ON,i)}
  temp <- aenet.myl.W1[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.W1 <- c(locate.na.W1,i)}
  temp <- aenet.myl.W2[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.W2 <- c(locate.na.W2,i)}
  temp <- aenet.myl.M1[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.M1 <- c(locate.na.M1,i)}
  temp <- aenet.myl.M3[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.M3 <- c(locate.na.M3,i)}
  temp <- aenet.myl.M6[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.M6 <- c(locate.na.M6,i)}
  temp <- aenet.myl.M9[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.M9 <- c(locate.na.M9,i)}
  temp <- aenet.myl.Y1[[i]][1:10,1:10]
  if(sum(is.na(temp))==100){print(i);locate.na.Y1 <- c(locate.na.Y1,i)}
}

for (j in 1:length(locate.na.ON)) {aenet.myl.ON[[locate.na.ON[j]]] <-aenet.myl.ON[[locate.na.ON[j]-1]]}
for (j in 1:length(locate.na.W1)) {aenet.myl.W1[[locate.na.W1[j]]] <-aenet.myl.W1[[locate.na.W1[j]-1]]}
for (j in 1:length(locate.na.W2)) {aenet.myl.W2[[locate.na.W2[j]]] <-aenet.myl.W2[[locate.na.W2[j]-1]]}
for (j in 1:length(locate.na.M1)) {aenet.myl.M1[[locate.na.M1[j]]] <-aenet.myl.M1[[locate.na.M1[j]-1]]}
for (j in 1:length(locate.na.M3)) {aenet.myl.M3[[locate.na.M3[j]]] <-aenet.myl.M3[[locate.na.M3[j]-1]]}
for (j in 1:length(locate.na.M6)) {aenet.myl.M6[[locate.na.M6[j]]] <-aenet.myl.M6[[locate.na.M6[j]-1]]}
for (j in 1:length(locate.na.M9)) {aenet.myl.M9[[locate.na.M9[j]]] <-aenet.myl.M9[[locate.na.M9[j]-1]]}
for (j in 1:length(locate.na.Y1)) {aenet.myl.Y1[[locate.na.Y1[j]]] <-aenet.myl.Y1[[locate.na.Y1[j]-1]]}



save(aenet.myl.ON,
     aenet.myl.W1,
     aenet.myl.W2,
     aenet.myl.M1,
     aenet.myl.M3,
     aenet.myl.M6,
     aenet.myl.M9,
     aenet.myl.Y1,
     aenet.myl.short,
     aenet.myl.long,
     aenet.myl.all,
     file="data/Rdata/DailyShiborBidAenet.Rdata")  

#load("data/Rdata/DailyShiborBidAenet.Rdata")
aenet.dygraph(aenet.myl.ON,"ON")
aenet.dygraph(aenet.myl.W1,"W1")
aenet.dygraph(aenet.myl.W2,"W2")
aenet.dygraph(aenet.myl.M1,"M1")
aenet.dygraph(aenet.myl.M3,"M3")
aenet.dygraph(aenet.myl.M6,"M6")
aenet.dygraph(aenet.myl.M9,"M9")
aenet.dygraph(aenet.myl.Y1,"Y1")
aenet.dygraph(aenet.myl.short,"short")
aenet.dygraph(aenet.myl.long,"long")
aenet.dygraph(aenet.myl.all,"all")


for (i in 1:length(aenet.myl.ON)) {
  print(100-sum(is.na(aenet.myl.ON[[i]][1:10,1:10])))
}




for (i in 1:length(aenet.myl.ON)) {
  aenet.myl.short[[i]][aenet.myl.short[[i]]==0] <-NA
  print(100-sum(is.na(aenet.myl.short[[i]][1:10,1:10])))
}

for (i in 1:length(aenet.myl.ON)) {
  aenet.myl.long[[i]][aenet.myl.long[[i]]==0] <-NA
  print(100-sum(is.na(aenet.myl.long[[i]][1:10,1:10])))
}

x <- aenet.myl.W1[[1125]]
y <- aenet.myl.W1[[1124]]
x[is.na(x)] <- 0
y[is.na(y)] <- 0
x-y

aenet.myl.W1[[1125]] - aenet.myl.W1[[1125]]


#################################################################
# save yearly network y
#################################################################
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
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
n.sp <- bank10.abbr
n.g.sp <- paste0(n.sp,".g")

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

aenet.myl.ON <- list();aenet.myl.W1 <- list()
aenet.myl.W2 <- list();aenet.myl.M1 <- list()
aenet.myl.M3 <- list();aenet.myl.M6 <- list()
aenet.myl.M9 <- list();aenet.myl.Y1 <- list()
aenet.myl.short <- list();aenet.myl.long <- list();aenet.myl.all <- list()
var.myl.gir <- list()
vecm.myl.gir <- list()
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
  aenet.myl.all[[t]] <- (aenet.myl.short[[t]]+aenet.myl.long[[t]])/2
  
  
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
    vecm.myl.gir[[t]] <- temp
    colnames(vecm.myl.gir[[t]]) <- bank10.abbr;rownames(vecm.myl.gir[[t]]) <- bank10.abbr

    var.gir0 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 0, span = "yearly")[[1]];var.gir0 <- matrix(data = unlist(var.gir0),nrow =10,ncol = 10)
    var.gir1 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 1, span = "yearly")[[1]];var.gir1 <- matrix(data = unlist(var.gir1),nrow =10,ncol = 10)
    var.gir2 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 2, span = "yearly")[[1]];var.gir2 <- matrix(data = unlist(var.gir2),nrow =10,ncol = 10)
    var.gir3 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 3, span = "yearly")[[1]];var.gir3 <- matrix(data = unlist(var.gir3),nrow =10,ncol = 10)
    var.gir4 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 4, span = "yearly")[[1]];var.gir4 <- matrix(data = unlist(var.gir4),nrow =10,ncol = 10)
    var.gir5 <- dy.VAR.GIR(data = sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 5, span = "yearly")[[1]];var.gir5 <- matrix(data = unlist(var.gir5),nrow =10,ncol = 10)
    GIR <- array(c(var.gir0,
                   var.gir1,
                   var.gir2,
                   var.gir3,
                   var.gir4,
                   var.gir5), dim = c(10,10,6))
    temp <- weighted_gir(GIR, divided=1)$weighted.matrix
    temp <- matrix(temp %>% unlist,10,10) #%>% t
    var.myl.gir[[t]] <- temp
    colnames(var.myl.gir[[t]]) <- bank10.abbr;rownames(var.myl.gir[[t]] ) <- bank10.abbr

}

save(y.period,
     vecm.myl.gir,
     var.myl.gir,
     aenet.myl.short, 
     aenet.myl.long,
     aenet.myl.all,
     aenet.myl.ON,
     aenet.myl.W1,
     aenet.myl.W2,
     aenet.myl.M1,
     aenet.myl.M3,
     aenet.myl.M6,
     aenet.myl.M9,
     aenet.myl.Y1,
     file = "data/Rdata/latex_yearly_networky.Rdata"
)

