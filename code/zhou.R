#################################################################
# load sp data
#################################################################
group.stockprice <- read_excel(path = "data/stockprice.xlsx",
                               sheet = "group",
                               skip = 0,
                               col_names = T,
                               col_types = rep("text", 9)) %>% as.data.frame
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

raw.stockprice <- raw.stockprice[,c(1, 18, 19, 20, 22, 23, 2, 6, 7, 8, 9, 15, 17, 21, 25, 3, 11, 12, 13, 16, 24, 4, 5, 10, 14, 26)]
names(raw.stockprice) <- c("Date", group.stockprice$Abbr)
sp <- raw.stockprice
m.sp <- (sp[-1,-1] + sp[-nrow(sp),-1])/2
g.m.sp <- log(m.sp[-1,])-log(m.sp[-nrow(m.sp),])
names(g.m.sp) <- group.stockprice$Abbr

g.m.sp <- xts(g.m.sp, as.Date(raw.stockprice$Date[-c(1,2)], format='%Y-%m-%d'))
sp <- xts(sp[,-1], as.Date(raw.stockprice$Date, format='%Y-%m-%d'))
sp <- sp["2013-01-01/"]
sp <- sp[, !is.na(sp[1,])]
names.sp <- names(sp)
#################################################################
# load sp data
#################################################################
g.m.sp <- g.m.sp["2012-01-01/2013-12-30"]
g.m.sp <- g.m.sp[,-which(is.na(g.m.sp[1,]))]
dim(g.m.sp)
names(g.m.sp)
#%>% na.omit
index.sp.gir.var <- index.spill(data = g.m.sp, mode="fix", span=250, divided=1)
index <- index.sp.gir.var$index.all %>% na.omit
names(index) <- group.stockprice$OEname[match(names(g.m.sp),group.stockprice$Abbr)]
dygraph(sp[,c("ABC","ICBC")])
#################################################################
# o.sp
#################################################################
o.sp <- sp
sp0 <- sp["2013-01-04"] %>% as.numeric
for (i in 1:dim(sp)[1]) {
  o.sp[i,] <- sp[i,]-sp["2013-01-04"] %>% as.numeric
}
#
color <- group.stockprice$color[match(names.sp,group.stockprice$Abbr)]
dygraph(o.sp) %>%
  dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", strokeWidth = 2, colors = color) %>%
  dyEvent("2013-06-07", "2014.06.07: JECE default on JECE", labelLoc = "bottom") %>%
  dyEvent("2013-06-20", "2014.06.20: Short Term Shibor Rates soared to astonishing levels", labelLoc = "bottom")
#
o.sp$so <- (o.sp[,c("SOABC", "SOBC",  "SOICBC", "SOCCB", "SOBOC")] %>% rowSums)/5
o.sp$je <- (o.sp[,c("JEPA",  "JEPF", "JEHB",  "JEMS",  "JECM",  "JECI",  "JECE")] %>% rowSums)/7
o.sp$ur <- (o.sp[,c("URNB", "URNJ",  "URBJ")] %>% rowSums)/3
#
so_je.sp <- o.sp[,c("so","je")]
names(so_je.sp) <- c("state owned banks","jointed equity banks")
dygraph(so_je.sp) %>%
  dyRangeSelector(dateWindow = c("2013-01-01", "2013-12-30"), height = 20)  %>%
  dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", strokeWidth = 2) %>%
  dyLegend(show = "auto", hideOnMouseOut = TRUE, width = 1000, labelsSeparateLines = FALSE) %>%
  dyEvent("2013-06-07", "2014.06.07: JECE default on JECE", labelLoc = "bottom") %>%
  dyEvent("2013-06-20", "2014.06.20: Short Term Shibor Rates soared to astonishing levels", labelLoc = "bottom")
#################################################################
# load shibor bids
#################################################################
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
#################################################################
# term spread
#################################################################
term.spread <- (shibor$`1Y` + shibor$`9M` + shibor$`6M` + shibor$`3M` - shibor$`1M` - shibor$`2W` - shibor$`1W`- shibor$`O/N`)/4
data <- cbind(shibor[,-1], term.spread)

names(data)[ncol(data)] <- "Term Spread"
dy.data <- xts(data, as.Date(shibor$Date, format='%Y-%m-%d'))[,"Term Spread"]
dy.main <- ""
#color <- c(colorRampPalette(c("red", "white"))(5)[-5],colorRampPalette(c("blue", "white"))(5)[-5], "black")
dygraph(dy.data)%>%
  dyAxis("y", label = "Term Spread") %>%#valueRange = 
  dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", colors = "black", strokeWidth = 2) %>%
  dyLegend(show = "auto", hideOnMouseOut = TRUE, width = 1000, labelsSeparateLines = FALSE) %>%
  dyHighlight(highlightCircleSize = 4, 
              highlightSeriesBackgroundAlpha = 0.7,
              hideOnMouseOut = TRUE,
              highlightSeriesOpts = list(strokeWidth = 2.5)) %>%
  dyEvent("2007-10-26", "2007.10.26", labelLoc = "bottom") %>%
  dyEvent("2011-1-30", "2011.01.30", labelLoc = "bottom") %>%
  dyEvent("2011-6-23", "2011.06.23", labelLoc = "bottom") %>%
  dyEvent("2012-1-18", "2012.01.18", labelLoc = "bottom") %>%
  dyEvent("2013-6-7", "2013.06.07", labelLoc = "bottom") %>%
  dyEvent("2013-12-23", "2013.12.23", labelLoc = "bottom") %>%
  dyEvent("2014-06-24", "2014.06.24", labelLoc = "bottom") %>%
  dyEvent("2014-12-23", "2014.12.23", labelLoc = "bottom") %>%
  dyEvent("2017-06-20", "2017.06.20", labelLoc = "bottom") %>%
  dyEvent("2017-12-29", "2017.12.29", labelLoc = "bottom") %>%
  dyLimit(0, color = "gray")%>% 
  dyAxis("y", label = "Term Spread" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Term Spread", label = "Term Spread", color = "black", strokeWidth = 0.01, fillGraph = 1,axis = "y")
#################################################################
# load wind data
#################################################################
wind.name <- read_excel(path = "data/windname.xlsx",sheet = "name",
                        skip = 0,
                        col_names = F,
                        col_types = c("text", "text" , "text")) %>% as.data.frame
fnlist.wind <- dir("data/wind")
raw.wind <- list()
raw.wind.full <- list()
length.fnlist <- length(fnlist.wind)
for(i in 1:length.fnlist){
  temp <- read_excel(path = paste0("data/wind/",fnlist.wind[i])) %>% as.data.frame
  temp$报告期 <- temp$报告期 %>% as.character
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
  temp$`基本每股收益(万/股)` <- temp$`基本每股收益(万/股)` * (10^4)/10000*(12^10)#scale#
  temp$`稀释每股收益(元/股)` <- temp$`稀释每股收益(元/股)` * 10000*(12^10)#scale#
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
  
  data <- temp
  wind.group <- rep("Urban Commercial Banks", nrow(data))
  
  locate <- which(grepl("农",rownames(data)))
  wind.group[locate] <- "Rural Commercial Banks"
  
  locate <- sapply(c(group.stockprice[group.stockprice$Eclass == "State-Owned Banks", "Cwind"],
                     "中国邮政储蓄银行股份有限公司"), 
                   grepl, rownames(data))
  locate <- apply(locate, 2, which) %>% unlist
  wind.group[locate] <- "State-Owned Banks"
  
  locate <- sapply(group.stockprice[group.stockprice$Eclass == "Joint-Equity Commercial Banks", "Cwind"], 
                   grepl, rownames(data))
  locate <- apply(locate, 2, which) %>% unlist
  wind.group[locate] <-  "Joint-Equity Commercial Banks"
  temp$Eclass <- wind.group
  
  #wind.group <- factor(wind.group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks", "Policy Banks"))

  raw.wind.full[[i]] <- temp
  raw.wind.full[[i]] <- cbind(raw.wind.full[[i]],temp.title)
  print(dim( raw.wind.full[[i]]))
}
#################################################################
# forest wind data
#################################################################
data <- rbindlist(raw.wind.full) %>% as.data.frame
data$Eclass <- factor(data$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
data <- data[,c("公司名称","报告期","Eclass","asst","lblt","lnd","brr","dlnd","dbrr")]
locate.numeric <- lapply(data,FUN=is.numeric) %>% unlist
temp <- data[,c("asst","lblt")]
sum(is.na(temp))
#
data[which(is.na(data[,"lblt"])),c("公司名称","报告期")]
#38 广东南粤银行股份有限公司 2007-12-31
temp <- data[data$公司名称=="广东南粤银行股份有限公司",]
temp[,locate.numeric] <- missForest(temp[,locate.numeric], variablewise = T, ntree = 500)$ximp %>% as.data.frame
data[data$公司名称=="广东南粤银行股份有限公司",] <- temp
#
data[which(is.na(data[,"asst"])),c("公司名称","报告期")]
#561 青海西宁农村商业银行股份有限公司 2010-12-31
#777 青海西宁农村商业银行股份有限公司 2011-12-31
#997             泰安银行股份有限公司 2012-12-31
temp <- data[data$公司名称=="青海西宁农村商业银行股份有限公司",]
temp[,locate.numeric] <- missForest(temp[,locate.numeric], variablewise = T, ntree = 500)$ximp %>% as.data.frame
data[data$公司名称=="青海西宁农村商业银行股份有限公司",] <- temp

temp <- data[data$公司名称=="泰安银行股份有限公司",]
temp[,locate.numeric] <- missForest(temp[,locate.numeric], variablewise = T, ntree = 500)$ximp %>% as.data.frame
data[data$公司名称=="泰安银行股份有限公司",] <- temp

save(data,file = "data/Rdata/latex_wind_forest_no_na.Rdata")
load(file = "data/Rdata/latex_wind_forest_no_na.Rdata")
#################################################################
# figure of wind 
#################################################################
dev.new() 
load(file = "data/Rdata/latex_wind_forest_no_na.Rdata")
data[is.na(data)] <- 0
color <- group.stockprice$color[c(5,14,18,25)-2]
#asset
data.asst <- tapply(data$asst,list(data$报告期,data$Eclass),sum) %>% as.data.frame
data.asst$year <- rownames(data.asst)
data.asst <- melt(data.asst, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(data.asst,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("asset") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#liability
data.lblt <- tapply(data$lblt,list(data$报告期,data$Eclass),sum) %>% as.data.frame
data.lblt$year <- rownames(data.lblt)
data.lblt <- melt(data.lblt, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(data.lblt,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("liability") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#lnd
data.lnd <- tapply(data$lnd,list(data$报告期,data$Eclass),sum) %>% as.data.frame
data.lnd$year <- rownames(data.lnd)
data.lnd <- melt(data.lnd, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(data.lnd,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("interbank lending (loan)") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#brr
data.brr <- tapply(data$brr,list(data$报告期,data$Eclass),sum) %>% as.data.frame
data.brr$year <- rownames(data.brr)
data.brr <- melt(data.brr, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(data.brr,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("interbank borrowing (loan)") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#dlnd
data.dlnd <- tapply(data$dlnd,list(data$报告期,data$Eclass),sum) %>% as.data.frame
data.dlnd$year <- rownames(data.dlnd)
data.dlnd <- melt(data.dlnd, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(data.dlnd,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("interbank lending (depoist)") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#dbrr
data.dbrr <- tapply(data$dbrr,list(data$报告期,data$Eclass),sum) %>% as.data.frame
data.dbrr$year <- rownames(data.dbrr)
data.dbrr <- melt(data.dbrr, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(data.dbrr,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("interbank borrowing (depoist)") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#lnd in asst 
data.lndIasst <- data.lnd
data.lndIasst$quant <- data.lnd$quant/data.asst$quant
p <- ggplot(data.lndIasst,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("ratio of lending (loan) in asset") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#dlnd in asst 
data.dlndIasst <- data.dlnd
data.dlndIasst$quant <- data.dlnd$quant/data.asst$quant
p <- ggplot(data.dlndIasst,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("ratio of lending (depoist) in asset") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#brr in lblt
data.brrIlblt <- data.brr
data.brrIlblt$quant <- data.brr$quant/data.lblt$quant
p <- ggplot(data.brrIlblt,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("ratio of borrowing (loan) in liability") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#dbrr in lblt
data.dbrrIlblt <- data.brr
data.dbrrIlblt$quant <- data.dbrr$quant/data.lblt$quant
p <- ggplot(data.dbrrIlblt,aes(x=year,y=quant,fill=type)) + geom_bar(position="dodge",stat="identity")
p + xlab("year") + ylab("ratio of borrowing (depoist) in liability") + labs(fill="banks") + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#
interbank.brr <- data.dbrr
interbank.brr$quant <- data.brr$quant + data.dbrr$quant
interbank.brr$brr <- data.brr$quant/interbank.brr$quant
interbank.brr$dbrr <- data.dbrr$quant/interbank.brr$quant
interbank.brr <- interbank.brr[,-c(2,3)]
names(interbank.brr) <- c("year","loan","deposit")
interbank.brr <- melt(interbank.brr, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(interbank.brr, aes(x = year, y = quant, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Pastel1")
p + xlab("year") + ylab("proportion") + labs(fill="types")

interbank.lnd <- data.dlnd
interbank.lnd$quant <- data.lnd$quant + data.dlnd$quant
interbank.lnd$lnd <- data.lnd$quant/interbank.lnd$quant
interbank.lnd$dlnd <- data.dlnd$quant/interbank.lnd$quant
interbank.lnd <- interbank.lnd[,-c(2,3)]
names(interbank.lnd) <- c("year","loan","deposit")
interbank.lnd <- melt(interbank.lnd, id = c("year"), variable.name = "type", value.name = "quant", na.rm=T)
p <- ggplot(interbank.lnd, aes(x = year, y = quant, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Pastel1")
p + xlab("year") + ylab("proportion") + labs(fill="types")# + scale_fill_manual(values=color)# + scale_fill_grey() + theme_classic()
#################################################################
# funding cost
#################################################################
load(file = "data/Rdata/latex_ALLshiborbid_RawShiborBid.Rdata")
load(file = "data/Rdata/latex_bond.Rdata")
load(file = "data/Rdata/latex_group.bid.full.Rdata")

data <- raw.shibor.bid.M9[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "M9"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-05-05", end = "2013-06-05", max = 10, type = type,head = NULL, standard = T,replace.group = "Before")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-06", end = "2013-07-06", max = 10, type = type,head = NULL, standard = T,replace.group = "During")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-20", end = "2013-07-20", max = 10, type = type,head = NULL, standard = T,replace.group = "During")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-07-20", end = "2013-08-20", max = 10, type = type,head = NULL, standard = T,replace.group = "After")

#M1
data <- raw.shibor.bid.M1[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "M1"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-07-20", end = "2013-08-20", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "After")

#M3
data <- raw.shibor.bid.M3[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "M3"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-05-05", end = "2013-06-05", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "Before")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-06", end = "2013-07-06", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "During")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-20", end = "2013-07-20", max = 10, type = type,head = NULL, standard = T, charge = -80,replace.group = "During")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-07-20", end = "2013-08-20", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "After")

#M6
data <- raw.shibor.bid.M6[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "M6"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-20", end = "2013-07-20", max = 10, type = type,head = NULL, standard = T, charge = -1,replace.group = "During")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-07-20", end = "2013-08-20", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "After")

#M9
data <- raw.shibor.bid.M9[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "M9"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-20", end = "2013-07-20", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "During")

#W1
data <- raw.shibor.bid.W1[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "W1"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-05-05", end = "2013-06-05", max = 10, type = type,head = NULL, standard = T, charge = -10,replace.group = "Before")
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-06", end = "2013-07-06", max = 10, type = type,head = NULL, standard = T, charge = -300,height.width = 700,replace.group = "During")

#Y1
data <- raw.shibor.bid.Y1[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "Y1"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-20", end = "2013-07-20", max = 10, type = type,head = NULL, standard = T, charge = -20,replace.group = "During")

#M1
data <- raw.shibor.bid.M1[[8]];
locate <- match(names(data),group.bid$Abbr);names(data) <- group.bid$OEname[locate];data <- data[,order(locate)]
type <- "M1"
x <- aenet.corrplot(data = data,bond = bond, start = "2013-06-20", end = "2013-07-20", max = 10, type = type,head = NULL, standard = T, charge = -1000,height.width = 800,replace.group = "During")






