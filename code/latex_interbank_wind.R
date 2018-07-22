#################################################################
# wind: estimate loan and deposit network
#################################################################
library(NetworkRiskMeasures)
load(file = "data/Rdata/latex_group.stockprice.Rdata")
group.wind <- group.stockprice[,"Cwind"]
wind.name <- read_excel(path = "data/windname.xlsx",sheet = "name",
                        skip = 0,
                        col_names = F,
                        col_types = c("text", "text" , "text")) %>% as.data.frame

fnlist.wind <- dir("data/wind")
l.matrix <- list()
d.matrix <- list()
length.fnlist <- length(fnlist.wind)
for(i in 1:length.fnlist){
  temp <- read_excel(path = paste0("data/wind/",fnlist.wind[i]),
                     sheet = "万得",
                     skip = 0,
                     col_names = T,
                     col_types = c("text", "text", rep("numeric", 55), "date")) %>% as.data.frame
  temp <- temp[temp$数据来源=="合并报表",]
  temp <- temp[apply(temp, 1, function(x) !all(is.na(x))),]
  temp <- temp[,c("公司名称", "拆出资金(万元)", "拆入资金(万元)", "存放同业和其它金融机构款项(万元)", "同业和其它金融机构存放款项(万元)")]
  temp[is.na(temp)] <- 0
  temp[,-1] <- temp[,-1]*10000/(10^12)#scale#
  
  l.matrix[[i]] <- matrix_estimation(rowsums = temp$"拆出资金(万元)", colsums = temp$"拆入资金(万元)", method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  colnames(l.matrix[[i]]) <- temp$"公司名称";rownames(l.matrix[[i]]) <- temp$公司名称
  l.matrix[[i]][is.na(l.matrix[[i]])] <- 0
  
  d.matrix[[i]] <- matrix_estimation(rowsums = temp$"存放同业和其它金融机构款项(万元)", colsums = temp$"同业和其它金融机构存放款项(万元)", method = "me", max.it = 1000, abs.tol = 0.001, verbose = TRUE)
  colnames(d.matrix[[i]]) <- temp$"公司名称";rownames(d.matrix[[i]]) <- temp$公司名称
  d.matrix[[i]][is.na(d.matrix[[i]])] <- 0
}
save(l.matrix,d.matrix, file = "data/Rdata/latex_interbank_wind.Rdata")
#################################################################
# wind: proportion loan and deposit interbank network for 10 banks
#################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
group.wind <- group.stockprice[bank10.abbr,"Cwind"]

p.l.matrix <- lapply(l.matrix, FUN = function(x){
  locate <- match(group.wind, colnames(x))
  x <- x[locate,locate]
  x[is.na(x)] <- 0
  colnames(x) <- bank10.abbr;rownames(x) <- bank10.abbr;
  return(x)})

p.d.matrix <- lapply(d.matrix, FUN = function(x){
  locate <- match(group.wind, colnames(x))
  x <- x[locate,locate]
  x[is.na(x)] <- 0
  colnames(x) <- bank10.abbr;rownames(x) <- bank10.abbr;
  return(x)})

#original intrbank network
if(TRUE){
  r.loan.network <- lapply(p.l.matrix, FUN = function(x){
    y <- x#/(10^12)
    return(y)})
  
  r.deposit.network <- lapply(p.d.matrix, FUN = function(x){
    y <- x#/(10^12)
    return(y)})#log(deposit.network %>% unlist,10)
  
  r.compound.network <- list()
  for (i in 1:length(r.loan.network)) {
    r.compound.network[[i]] <- r.loan.network[[i]] + r.deposit.network[[i]]
  }
}#scale#

# divided the network by sum of lending of 10 banks
if(TRUE){
  pl.loan.network <- lapply(p.l.matrix, FUN = function(x){
    y <- x/rowSums(x)
    return(y)})
  
  pl.deposit.network <- lapply(p.d.matrix, FUN = function(x){
    y <- x/rowSums(x)
    return(y)})#log(deposit.network %>% unlist,10)
  
  pl.compound.network <- list()
  for (i in 1:length(p.l.matrix)) {
    pl.compound.network[[i]] <- p.l.matrix[[i]] + p.d.matrix[[i]]
  }
  pl.compound.network <- lapply(pl.compound.network, FUN = function(x){
    y <- x/rowSums(x)
    return(y)})
}

# divided the network by sum of borrwoing of 10 banks
if(TRUE){
  pb.loan.network <- lapply(p.l.matrix, FUN = function(x){
    y <- x/colSums(x)
    return(y)})
  
  pb.deposit.network <- lapply(p.d.matrix, FUN = function(x){
    y <- x/colSums(x)
    return(y)})#log(deposit.network %>% unlist,10)
  
  pb.compound.network <- list()
  for (i in 1:length(p.l.matrix)) {
    pb.compound.network[[i]] <- p.l.matrix[[i]] + p.d.matrix[[i]]
  }
  pb.compound.network <- lapply(pb.compound.network, FUN = function(x){
    y <- x/colSums(x)
    return(y)})
}

# divided the network by sum of borrowing of all banks
if(TRUE){
  ab.loan.network <- lapply(l.matrix, FUN = function(x){
    y <- x/colSums(x)
    locate <- match(group.wind, colnames(x))
    y <- y[locate, locate]
    return(y)})
  
  ab.deposit.network <- lapply(d.matrix, FUN = function(x){
    y <- x/colSums(x)
    locate <- match(group.wind, colnames(x))
    y <- y[locate, locate]
    return(y)})#log(deposit.network %>% unlist,10)
  
  ab.compound.network <- list()
  for (i in 1:length(p.l.matrix)) {
    ab.compound.network[[i]] <- l.matrix[[i]] + d.matrix[[i]]
  }
  ab.compound.network <- lapply(ab.compound.network, FUN = function(x){
    y <- x/colSums(x)
    locate <- match(group.wind, colnames(x))
    y <- y[locate, locate]
    return(y)})
}

# divided the network by sum of lending of all banks
if(TRUE){
  al.loan.network <- lapply(l.matrix, FUN = function(x){
    y <- x/rowSums(x)
    locate <- match(group.wind, colnames(x))
    y <- y[locate, locate]
    return(y)})
  
  al.deposit.network <- lapply(d.matrix, FUN = function(x){
    y <- x/rowSums(x)
    locate <- match(group.wind, colnames(x))
    y <- y[locate, locate]
    return(y)})#log(deposit.network %>% unlist,10)
  
  al.compound.network <- list()
  for (i in 1:length(p.l.matrix)) {
    al.compound.network[[i]] <-l.matrix[[i]] + d.matrix[[i]]
  }
  al.compound.network <- lapply(al.compound.network, FUN = function(x){
    y <- x/rowSums(x)
    locate <- match(group.wind, colnames(x))
    y <- y[locate, locate]
    return(y)})
}

save(r.loan.network, r.deposit.network, r.compound.network,
     pl.loan.network, pl.deposit.network, pl.compound.network,
     pb.loan.network, pb.deposit.network, pb.compound.network,
     al.loan.network, al.deposit.network, al.compound.network,
     ab.loan.network, ab.deposit.network, ab.compound.network,
     file = "data/Rdata/latex_wind_rpa.Rdata")
#################################################################
# wind: draw compound network
#################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")
load(file = "data/Rdata/latex_group.stockprice.Rdata")

c.matrix <- list()
for (i in 1:length(l.matrix)) {
  c.matrix[[i]] <- l.matrix[[i]] + d.matrix[[i]]
}
max.matrix <- unlist(c.matrix) %>% max
#
wind.date <- 2007:2016
for (i in 1:length(wind.date)) {
  #i <- length(data.interbank)
  data <- c.matrix[[i]] 
  data <- data/max.matrix 
  
  wind.group <- rep("Urban Commercial Banks", nrow(data))
  
  locate <- which(grepl("农",colnames(data)))
  wind.group[locate] <- "Rural Commercial Banks"
  
  locate <- sapply(c(group.stockprice[group.stockprice$Eclass == "State-Owned Banks", "Cwind"],
                    "中国邮政储蓄银行股份有限公司"), 
                   grepl, colnames(data))
  locate <- apply(locate, 2, which) %>% unlist
  wind.group[locate] <- "State-Owned Banks"
  
  locate <- sapply(group.stockprice[group.stockprice$Eclass == "Joint-Equity Commercial Banks", "Cwind"], 
                   grepl, colnames(data))
  locate <- apply(locate, 2, which) %>% unlist
  wind.group[locate] <-  "Joint-Equity Commercial Banks"
  
  
  wind.group <- factor(wind.group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks", "Policy Banks"))
  order.bank <- colnames(data)
  order.bank <- order.bank[order(wind.group, decreasing = F)] 
  wind.group <- wind.group[order(wind.group, decreasing = F)] 
  
  temp <- data[order.bank,order.bank]
  if(max(temp)>0.02){temp[temp < 0.02] <- 0}
  igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
  networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
  networkD3.matrix[[2]]$group <- wind.group
  temp.size <- rowSums(temp)
  temp.size[temp.size < 0.4] <- 0.4
  networkD3.matrix[[2]]$size <- temp.size
  networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
  networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size * 5
  networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$value * 10
  matrix.date <- data.frame(name="",size=0, group=wind.date[i] %>% as.character)
  networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
  
  ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","#1A237E","white"]);'
  rbokeh <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                         Source = "source", Target = "target", Value = "value", 
                         arrows = T, linkDistance = JS("function(d){return 1 / d.value * 50}"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                         NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                         Group = "group", legend = T, fontSize = 0,
                         opacity = 0.8, opacityNoHover = 1,
                         height = 700 , width = 700, bounded = F, zoom = T, charge = -100)
  widget2png(rbokeh , file = paste0("latex/report/figure/Allinterbankt", wind.date[i], ".png"), timeout = 1200)
}
#################################################################
# wind: desity and power law
#################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")#2007:2016
load(file = "data/Rdata/latex_group.stockprice.Rdata")
load(file = "data/Rdata/latex_ForestData.Rdata")
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
is.higher <- data[,n.is.higher]

c.matrix <- list()
for (i in 1:length(l.matrix)) {
  c.matrix[[i]] <- l.matrix[[i]] + d.matrix[[i]]
}
max.c.matrix <- c.matrix %>% unlist %>% max
c.matrix <- lapply(c.matrix, FUN = function(x){ 
  y <- x/max.c.matrix/15
return(y)})

# density
density.pl <- lapply(c.matrix, sum) %>% unlist
density.length <- (lapply(c.matrix, length) %>% unlist)^(1/2)
density.pl <- density.pl/density.length*100
density.pl <- xts(density.pl, as.Date(paste0(seq(2007,2016,1),"-06-30"), format='%Y-%m-%d'))

# largest 10
#for (i in 1:length(c.matrix)) {x <-c.matrix[[i]] %>% as.numeric;plot(x[order(-x)])}

# alpha of power law
#xmin <- c.matrix %>% unlist %>% is.na %>% sum
#length(xmin)*0.1
#xmin <- xmin[order(-xmin)][1:26000]
#plot(xmin)
#xmin <- xmin[order(-xmin)][52095]
alpha.pl <- lapply(c.matrix, FUN = function(x){
  y <- power.law.fit(x, xmin = NULL, start = 2)$alpha
  return(y)
})
alpha.pl <- xts(alpha.pl %>% unlist, as.Date(paste0(seq(2007,2016,1),"-06-30"), format='%Y-%m-%d'))

dy.data <- merge(alpha.pl["2007-01-03/2016-12-30"],density.pl["2007-01-03/2016-12-30"], is.higher$crisis.sma20)
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
# wind: draw trimed interbank 10
#################################################################
load(file = "data/Rdata/latex_wind_rpa.Rdata")
load(file = "data/Rdata/latex_interbank_wind.Rdata")
wind.date <- c(2007:2016)
for (i in 1:length(wind.date)) {
temp <- r.compound.network[[i]] 

colnames(temp) <- bank10.abbr;rownames(temp) <- bank10.abbr
if(max(temp)>0.02){temp[temp < 0.02] <- 0}
igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
networkD3.matrix[[2]]$group <- group.stockprice[bank10.abbr,"Eclass"]

temp.size <- rowSums(temp)
temp.size[temp.size < 0.4] <- 0.4
networkD3.matrix[[2]]$size <- temp.size
networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*20
networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$value*20
matrix.date <- data.frame(name="",size=0, group=as.character(wind.date[[i]]))
networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)

ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white"]);'
rbokeh <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                       Source = "source", Target = "target", Value = "value", 
                       arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                       NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                       Group = "group", legend = T, fontSize = 10,
                       opacity = 0.8, opacityNoHover = 1,
                       height = 700 , width = 700, bounded = F, zoom = T, charge = -100)
widget2png(rbokeh , file = paste0("latex/report/figure/Trimmedinterbank", wind.date[i], ".png"), timeout = 1200)
}
#################################################################
# wind: Number of Banks Participate in Interbank Market
#################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")
number.banks <- c()
for (i in 1:length(l.matrix)) {
  temp <- dim(l.matrix[[i]])[1]
  number.banks <- c(number.banks, temp)
}
number.banks <- xts(number.banks, as.Date( paste0(seq(2007,2016,1),"-06-30"), format='%Y-%m-%d'))
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
# wind: proportion of deposit and loan in interbank market
#################################################################
load(file = "data/Rdata/latex_raw.wind.Rdata")
load(file = "data/Rdata/latex_group.stockprice.Rdata")

raw.data <- wind.full[,c("公司名称", "报告期","lnd","brr","dlnd","dbrr")]
raw.data <- melt(raw.data, id = c("报告期", "公司名称"), variable.name = "Term", value.name = "Value", na.rm=T)
raw.data[is.na(raw.data)] <- 0
bank10.abbr <- c("SOBC", "SOICBC", "SOCCB", "SOBOC", "JEPF", "JEHB", "JECM", "JECI", "JECITIC", "URBJ")
p.names <- group.stockprice[bank10.abbr,"Cwind"]

compound <- dcast(raw.data, 报告期 ~ 公司名称, fun.aggregate = sum) %>% as.data.frame
p.compound <- compound[,p.names] %>% rowSums
a.compound <- compound[,-1] %>% rowSums

loan <- raw.data[raw.data$Term=="lnd"|raw.data$Term=="brr",]
loan <- dcast(loan, 报告期 ~ 公司名称, fun.aggregate = sum) %>% as.data.frame
p.loan <- loan[,p.names] %>% rowSums
a.loan <- loan[,-1] %>% rowSums

deposit <- raw.data[raw.data$Term=="dlnd"|raw.data$Term=="dbrr",]
deposit <- dcast(deposit, 报告期 ~ 公司名称, fun.aggregate = sum) %>% as.data.frame
p.deposit <- deposit[,p.names] %>% rowSums
a.deposit <- deposit[,-1] %>% rowSums

draw <- data.frame("Interbank Deposit" = round(p.deposit/a.compound*100), "Interbank Loan" = round(p.loan/a.compound*100), Year = 2007:2016)
draw <- melt(draw, id = c("Year"), variable.name = "Type", value.name = "Proportion", na.rm=T)

ggplot(data = draw, aes(x = Year, y = Proportion, fill = Type, frame = Year, cumulative = TRUE)) +
  geom_col() +
  labs(x = "Year", y = "Proportion", title = "", subtitle = "") +
  geom_text(aes(label = Proportion), color = "Black", size = 5, position = position_stack(vjust = 0.5)) + 
  #theme(axis.text.x = element_text(colour="Black",size=15), axis.text.y = element_text(colour="Black",size=15)) +
  theme_minimal()+
  scale_fill_manual(values=c("#FFBFC0","#BFC0FA"))
#################################################################
# wind: data decription of loan and deposit data used to estimate networks
#################################################################
load(file = "data/Rdata/latex_raw.wind.Rdata")
load(file = "data/Rdata/latex_group.stockprice.Rdata")

raw.data <- wind.full[,c("公司名称", "报告期","lnd","brr","dlnd","dbrr")]
raw.data <- melt(raw.data, id = c("报告期", "公司名称"), variable.name = "Term", value.name = "Value", na.rm=T)
raw.data <- dcast(raw.data, 报告期 + 公司名称 ~ Term, fun.aggregate = mean) %>% as.data.frame
raw.data[is.na(raw.data)] <- 0
raw.data[raw.data < 0] <- 0
#sum(raw.data < 0)
data <- raw.data[,-c(1,2)]#/(10^10)#scale#
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
