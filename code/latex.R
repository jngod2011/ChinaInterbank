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
save(data,file = "data/Rdata/latex_ForestData.Rdata")

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
                               col_types = c(rep("text", 7),"numeric")) %>% as.data.frame
group.stockprice$Eclass <- factor(group.stockprice$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
group.stockprice <- group.stockprice[order(group.stockprice$Eclass,decreasing = F),] 
rownames(group.stockprice) <- group.stockprice$Abbr
group.stockprice$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(10)[-10], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6])
save(group.stockprice, file = "data/Rdata/latex_group.stockprice.Rdata")
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

save(is.higher,file = "data/Rdata/latex_is.higher.Rdata")

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
# bank list
#################################################################
load(file = "data/Rdata/latex_group.stockprice.Rdata")
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
# wind scale for 10 banks
#################################################################
load(file = "data/Rdata/latex_group.stockprice.Rdata")
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
  rownames(temp) <- temp.title$公司名称
  raw.wind.full[[i]] <- temp
  raw.wind.full[[i]] <- cbind(raw.wind.full[[i]],temp.title)
  #dim(temp)
  #temp <- temp[apply(temp, 1, function(x) !all(is.na(x))),]
  #dim(temp)
  #temp.title <- temp.title[apply(temp.title, 1, function(x) !all(is.na(x))),]
  
  
  #raw.wind.full[[i]]$name <- temp.title$
  locate <- sapply(group.wind, grepl, temp.title$公司名称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  temp <- temp[locate,]
  rownames(temp) <- bank10.abbr
  raw.wind[[i]] <- temp
}
raw.wind <- F.fill.up.NAs(raw.wind)
#raw.wind.full <- F.fill.up.NAs(raw.wind.full)

raw.wind %>% unlist %>%is.na %>% sum
#raw.wind.full %>%unlist%>% is.na %>% sum

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
  wind.full <- rbind(wind.full,temp)
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

    var.gir0 <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 0, span = "yearly")[[1]];var.gir0 <- matrix(data = unlist(var.gir0),nrow =10,ncol = 10)
    var.gir1 <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 1, span = "yearly")[[1]];var.gir1 <- matrix(data = unlist(var.gir1),nrow =10,ncol = 10)
    var.gir2 <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 2, span = "yearly")[[1]];var.gir2 <- matrix(data = unlist(var.gir2),nrow =10,ncol = 10)
    var.gir3 <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 3, span = "yearly")[[1]];var.gir3 <- matrix(data = unlist(var.gir3),nrow =10,ncol = 10)
    var.gir4 <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 4, span = "yearly")[[1]];var.gir4 <- matrix(data = unlist(var.gir4),nrow =10,ncol = 10)
    var.gir5 <- dy.VAR.GIR(data = g.sp[paste0(y.period[t],"/",y.period[t+1])], n.ahead = 5, span = "yearly")[[1]];var.gir5 <- matrix(data = unlist(var.gir5),nrow =10,ncol = 10)
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

#################################################################
# sp since 2000 is.listed
#################################################################
load(file = "data/Rdata/latex_group.stockprice.Rdata")
raw.stockprice <- read_excel(path = "data/stockprice.xlsx",
                             sheet = 1,
                             skip = 4,
                             col_names = F,
                             col_types = c("date", rep("numeric", 25))) %>% as.data.frame
names(raw.stockprice) <- c("Date",group.stockprice$Abbr)

sp.all <- raw.stockprice[,c("Date",group.stockprice$Abbr)]
Date <- sp.all$Date %>% as.character
Date <- substr(Date, 1, 4)# %>% unique#1991:2018
table(Date)
sp.all <- xts(raw.stockprice[,-1], as.Date(raw.stockprice$Date, format='%Y-%m-%d'))

Date.year <- paste0(c(1991:2018),"-01-01")
l.is.listed <- list()
l.is.listed0 <- list()
for (i in 1:(length(Date.year)-1)) {
  temp <- sp.all[paste0(Date.year[i],"/",Date.year[i+1])]
  is.listed <- apply(temp, MARGIN = 2, FUN = function(x){
    y <- sum(is.na(x))/length(x)
    return(y)
  })
  
  l.is.listed[[i]] <- names(is.listed[is.listed==0])
  l.is.listed0[[i]] <- names(is.listed[is.listed<0.5])
  #print(paste0(c("###########"),c(1991:2018)[i],c("###########")))
  #print(test)
}

names(l.is.listed) <- paste0("year",c(1991:2017))
lapply(l.is.listed0,length) %>% unlist-
  lapply(l.is.listed,length) %>% unlist


l.sp.all <- list()
l.sp.all.full <- list()
for (i in 1:length(c(1991:2017))) {
  l.sp.all.full[[i]] <- temp[,l.is.listed[[i]]] %>% na.omit
  temp <- sp.all[paste0(Date.year[i],"/",Date.year[i+1])]
  l.sp.all[[i]] <- temp[,l.is.listed[[i]]] %>% na.omit
  print(c(1991:2017)[[i]])
  print(dim(l.sp.all[[i]]))
}
names(l.sp.all) <- paste0("year",c(1991:2017))

l.sp.all <- l.sp.all[-c(1:length(1991:1999))]
l.sp.all.full <- l.sp.all.full[-c(1:length(1991:1999))]
l.is.listed <- l.is.listed[-c(1:length(1991:1999))]
names(l.sp.all)

save(l.is.listed,l.sp.all,l.sp.all.full,file = "data/Rdata/latex_sp.all.Rdata")
#################################################################
# wind scale for all availiable banks
#################################################################
load(file = "data/Rdata/latex_group.stockprice.Rdata")
load(file = "data/Rdata/latex_sp.all.Rdata")

l.is.listed <- l.is.listed[-c(18)]#"year2017"
l.is.listed <- l.is.listed[-c(1:length(2000:2006))] 
temp.list <- l.is.listed %>% unlist %>% unique
temp.Cwind <- group.stockprice[match(temp.list, group.stockprice$Abbr),"Cwind"]
temp.Abbr <- group.stockprice[match(temp.list, group.stockprice$Abbr),"Abbr"]

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
  temp$`向中央银行借款(万元)`[is.na(temp$`向中央银行借款(万元)`)] <- 0
  temp$`向中央银行借款净增加额(万元)`[is.na(temp$`向中央银行借款净增加额(万元)`)] <- 0
  temp$`向其他金融机构拆入资金净增加额(万元)`[is.na(temp$`向其他金融机构拆入资金净增加额(万元)`)] <- 0
  temp$`客户存款和同业存放款项净增加额(万元)`[is.na(temp$`客户存款和同业存放款项净增加额(万元)`)] <- 0
  temp$`客户贷款及垫款净增加额(万元)`[is.na(temp$`客户贷款及垫款净增加额(万元)`)] <- 0
  temp <- subset(temp, select =  -`应付利息(万元)`)
  temp <- subset(temp, select =  -`应收利息(万元)`)
  temp$`基本每股收益(万/股)` <- temp$`基本每股收益(万/股)` * (10^5)/10000*(12^10)#scale#
  temp$`稀释每股收益(元/股)` <- temp$`稀释每股收益(元/股)` * (10^5)/10000*(12^10)#scale#
  temp.title <- temp[,c("公司名称","数据来源","报告期")]
  temp <- subset(temp, select =  -`报告期`)
  temp <- subset(temp, select =  -`公司名称`)
  temp <- subset(temp, select =  -`数据来源`)

  
  onames <- names(temp)
  names(temp) <- wind.name[,2][match(onames,wind.name[,1])]
  temp <- temp*10000/(12^10)#scale#

  locate <- sapply(temp.Cwind, grepl, temp.title$公司名称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  temp <- temp[locate,]
  rownames(temp) <- temp.Abbr
  raw.wind[[i]] <- temp

}
raw.wind <- F.fill.up.NAs(raw.wind)
raw.wind %>% unlist %>%is.na %>% sum

for (i in 1:length(raw.wind)) {
  temp <- raw.wind[[i]]
  raw.wind[[i]] <- temp[l.is.listed[[i]],] 
  #print(dim(raw.wind[[i]]))
}

for (i in 1:length(raw.wind)) {
  raw.wind[[i]]$clnd <- raw.wind[[i]]$lnd + raw.wind[[i]]$dlnd
  raw.wind[[i]]$cbrr <- raw.wind[[i]]$brr + raw.wind[[i]]$dbrr
}

save(raw.wind, file = "data/Rdata/latex_raw.wind_all.Rdata")
#################################################################
# save yearly network y for all avaiable banks
#################################################################
load(file = "data/Rdata/latex_sp.all.Rdata")#
l.is.listed <- l.is.listed[-length(l.is.listed)]
l.is.listed <- l.is.listed[-c(1:length(2000:2006))]

l.sp.all <- l.sp.all[-length(l.sp.all)]
l.sp.all <- l.sp.all[-c(1:length(2000:2006))]

l.sp.all.full <- l.sp.all.full[-length(l.sp.all.full)]
l.sp.all.full <- l.sp.all.full[-c(1:length(2000:2006))]

var.myl.gir <- list()
vecm.myl.gir <- list()
network.y <- list()
y.period <- c(
  "2006-12-31",
  "2007-12-31",
  "2008-12-31",
  "2009-12-31",
  "2010-12-31",
  "2011-12-31",
  "2012-12-31",
  "2013-12-31",
  "2014-12-31",
  "2015-12-31",
  "2016-12-31"
  #,#"2017-12-31"
)

# the sample in 2018 is too small
for (t in 1:(length(y.period)-1)) {
  temp.abbr <- l.is.listed[[t]]
  temp.data <- l.sp.all.full[[t]]
  n.bank <- ncol(temp.data)
  try.error <- try(vecm.tsDyn <- VECM(data = temp.data, lag=2, estim="ML"),silent = TRUE)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
  rank.test <- vecm.tsDyn
  rank.eigen <- rank.test(vecm.tsDyn, cval = 0.01, type = "eigen")$r
  rank.trace <- rank.test(vecm.tsDyn, cval = 0.01, type = "trace")$r
  rank <- floor((rank.eigen + rank.trace)/2)#4
  temp.data <- l.sp.all[[t]]
  
  vecm.gir0 <- dy.VECM.GIR(data = temp.data, n.ahead = 0, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir0 <- matrix(data = unlist(vecm.gir0),nrow = n.bank, ncol = n.bank)
  vecm.gir1 <- dy.VECM.GIR(data = temp.data, n.ahead = 1, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir1 <- matrix(data = unlist(vecm.gir1),nrow = n.bank, ncol = n.bank)
  vecm.gir2 <- dy.VECM.GIR(data = temp.data, n.ahead = 2, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir2 <- matrix(data = unlist(vecm.gir2),nrow = n.bank, ncol = n.bank)
  vecm.gir3 <- dy.VECM.GIR(data = temp.data, n.ahead = 3, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir3 <- matrix(data = unlist(vecm.gir3),nrow = n.bank, ncol = n.bank)
  vecm.gir4 <- dy.VECM.GIR(data = temp.data, n.ahead = 4, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir4 <- matrix(data = unlist(vecm.gir4),nrow = n.bank, ncol = n.bank)
  vecm.gir5 <- dy.VECM.GIR(data = temp.data, n.ahead = 5, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir5 <- matrix(data = unlist(vecm.gir5),nrow = n.bank, ncol = n.bank)
  GIR <- array(c(vecm.gir0,
                 vecm.gir1,
                 vecm.gir2,
                 vecm.gir3,
                 vecm.gir4,
                 vecm.gir5), dim = c(n.bank,n.bank,6))
  temp <- weighted_gir(GIR, divided=1)$weighted.matrix
  temp <- matrix(temp %>% unlist,n.bank,n.bank) #%>% t
  vecm.myl.gir[[t]] <- temp
  colnames(vecm.myl.gir[[t]]) <- temp.abbr;rownames(vecm.myl.gir[[t]]) <- temp.abbr
  
  temp.data <- l.sp.all[[t]]
  temp.date <- index(l.sp.all[[t]])[-1]
  temp.data <- as.data.frame(apply(X = temp.data, MARGIN = 2, FUN = log_GrowthRate))
  temp.data <- xts(temp.data, as.Date(temp.date, format='%Y-%m-%d'))
  
  var.gir0 <- dy.VAR.GIR(data = temp.data, n.ahead = 0, span = "yearly")[[1]];var.gir0 <- matrix(data = unlist(var.gir0),nrow = n.bank,ncol = n.bank)
  var.gir1 <- dy.VAR.GIR(data = temp.data, n.ahead = 1, span = "yearly")[[1]];var.gir1 <- matrix(data = unlist(var.gir1),nrow = n.bank,ncol = n.bank)
  var.gir2 <- dy.VAR.GIR(data = temp.data, n.ahead = 2, span = "yearly")[[1]];var.gir2 <- matrix(data = unlist(var.gir2),nrow = n.bank,ncol = n.bank)
  var.gir3 <- dy.VAR.GIR(data = temp.data, n.ahead = 3, span = "yearly")[[1]];var.gir3 <- matrix(data = unlist(var.gir3),nrow = n.bank,ncol = n.bank)
  var.gir4 <- dy.VAR.GIR(data = temp.data, n.ahead = 4, span = "yearly")[[1]];var.gir4 <- matrix(data = unlist(var.gir4),nrow = n.bank,ncol = n.bank)
  var.gir5 <- dy.VAR.GIR(data = temp.data, n.ahead = 5, span = "yearly")[[1]];var.gir5 <- matrix(data = unlist(var.gir5),nrow = n.bank,ncol = n.bank)
  GIR <- array(c(var.gir0,
                 var.gir1,
                 var.gir2,
                 var.gir3,
                 var.gir4,
                 var.gir5), dim = c(n.bank,n.bank,6))
  temp <- weighted_gir(GIR, divided=1)$weighted.matrix
  temp <- matrix(temp %>% unlist,n.bank,n.bank) #%>% t
  var.myl.gir[[t]] <- temp
  colnames(var.myl.gir[[t]]) <- temp.abbr;rownames(var.myl.gir[[t]] ) <- temp.abbr
  
}

save(y.period,
     vecm.myl.gir,
     var.myl.gir,
     file = "data/Rdata/latex_yearly_networky_all.Rdata"
)
