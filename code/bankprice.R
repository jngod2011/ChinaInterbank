# load raw data
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

#match(c("Date",group.stockprice$Cname), c("Date","平安银行", "宁波银行", "江阴银行", "张家港行", "浦发银行", "华夏银行", "民生银行", "招商银行", "无锡银行", "江苏银行", "杭州银行", "南京银行", "常熟银行", "兴业银行", "北京银行", "上海银行", "农业银行", "交通银行", "工商银行", "光大银行", "建设银行", "中国银行", "贵阳银行", "中信银行", "吴江银行")) 
sp <- raw.stockprice[,c(1, 18, 19, 20, 22, 23, 2, 6, 7, 8, 9, 15, 17, 21, 25, 3, 11, 12, 13, 16, 24, 4, 5, 10, 14, 26)]
names(sp) <- c("Date", group.stockprice$Abre)
sp <- sp[sp$Date >="2007-09-25", ]
sp <- sp[, !is.na(sp[1,])]

group <- group.stockprice %>% t
group <- group[,names(sp)[-1]]

g.sp <- as.data.frame(lapply(sp[,-1], log_GrowthRate))

# index
index.1i250 <- index.spill(data = g.sp, mode = "fix", span = 250, divided = 1)
index.0i250 <- index.spill(g.sp, "fix", 250, 0)


# dygraph
## The Stock Price of 14 China's Banks from 2007 to 2018
dy.data <- xts(sp[,-1], as.Date(sp$Date, format='%Y-%m-%d'))
names(dy.data) <- group[2,]
dy.data <- merge(dy.data, is.higher$crisis.sma20)
names(dy.data)[ncol(dy.data)] <- "Crisis"
dy.main <- "Stock Price of 14 China's Banks from 2007 to 2018"
color <- c(c(group[6,] %>% as.character),c("black"))
dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Shanghai Interbank Offered Rate", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")

## The Growth Rate of Stock Price of 14 China's Banks from 2007 to 2018
dy.data <- xts(g.sp, as.Date(sp$Date, format='%Y-%m-%d')[-1])
names(dy.data) <- group[2,]
dy.main <- "The Growth Rate of Stock Price of 14 China's Banks from 2007 to 2018"
color <- group[6,] %>% as.character
dygraph.interbank(dy.data, dy.main, color)

## spill over index of 14 China's Banks from 2007 to 2018
dy.data <- xts(index.1i250[[1]][-c(1:250),], as.Date(sp$Date, format='%Y-%m-%d')[-c(1:251)])
dy.data <- merge(dy.data, is.higher$crisis.sma20)
names(dy.data) <- c(group[2,],"Crisis")
dy.main <- "spillover of 14 China's Banks from 2007 to 2018"
color <- c(group[6,],"black") %>% as.character
dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Shanghai Interbank Offered Rate", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")


## spill over index of 14 China's Banks from 2007 to 2018
dy.data <- xts(index.0i250[[1]][-c(1:250),], as.Date(sp$Date, format='%Y-%m-%d')[-c(1:251)])
names(dy.data) <- group[2,]
dy.main <- "spillover of 14 China's Banks from 2007 to 2018"
color <- group[6,] %>% as.character
dygraph.interbank(dy.data, dy.main, color)

# network
matrix.var <- index.1i250[[2]][[length(index.1i250[[2]])]]
matrix.var <- index.0i250[[2]][[length(index.0i250[[2]])]]
sd.matrix.var <-matrix.var/max(matrix.var)
data <- sd.matrix.var

# fevd
fevd.matrix <- fevd.var(g.sp, mode = "fix", span = 250, n.ahead=1)

#require(lattice)
#temp <- fevd.matrix[[1]]
#temp <- as.numeric(temp)
#hist(temp,breaks=20,col="red4",xlab="fevd", main="histogram")
#quantile(temp,c(0,0.2,0.5,0.8,1))
fevd.date <-xts(c(1:length(sp$Date[-c(1:251)])), as.Date(substr(sp$Date[-c(1:251)],1,10), format='%Y-%m-%d'))# 
crisis <- "crisis.sma20"
is.higher.fevd <- merge(fevd.date, is.higher[,crisis], join='left')
is.higher.fevd[,crisis] <- cut(as.numeric(is.higher.fevd[,crisis]),breaks = 5, include.lowest=F, labels = c(1:5)) 

if(sum(is.na(is.higher.fevd)) > 0){
  is.higher.fevd[,-1] <- apply(is.higher.fevd[,-1], 2, f.na.approx)
}
is.higher.fevd[,crisis] <- is.higher.fevd[,crisis] %>% ceiling

require(igraph)
for (i in seq(1,length(fevd.matrix),10)) {
  temp <- fevd.matrix[[i]] %>% t#temp[lower.tri(temp)] <- 0
  temp[temp < 0.07] <- 0
  diag(temp) <- 0
  
  igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
  networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
  networkD3.matrix[[2]]$group <- group[5,]
  networkD3.matrix[[2]]$name <- group[2,]#group[1,]
  
  networkD3.matrix[[2]]$size <- rowSums(temp)
  networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
  networkD3.matrix[[2]] <- networkD3.matrix[[2]][order(networkD3.matrix[[2]]$group,decreasing = F),] 
  networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*20
  networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu*20

  group.crisis=c(" ","  ","   ","    ","     ")
  group.crisis[is.higher.fevd[i,crisis]] <- index(is.higher.fevd[i,crisis]) %>% as.character
  
  matrix.date <- data.frame(name=c("","","","","",""),
                            c("Spillover Network", group.crisis),
                            size=c(0,0,0,0,0,0)) %>% as.data.frame
  names(matrix.date) <-c("name","group","size")
  networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
  
  ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white","#CCCCCC","#999999", "#666666","#333333","#000000"]);'

  rbokeh.bank <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                              Source = "source", Target = "target", Value = "value", 
                              arrows = T, linkDistance = JS("function(d){return 1 / d.value * 50 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                              NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                              Group = "group", legend = T, fontSize = 10, opacity = 0.8, opacityNoHover = 1,
                              height = 800 , width = 800, bounded = F, zoom = T, charge = -1000)
  widget2png(rbokeh.bank, file = paste0("latex/report/igraph/bank", (i-1)/10+1, ".png"), timeout = 2000)
  #time.end <- Sys.time()
  #running.time <- (time.end - time.start)
  #print(running.time)
  
  cat("\r", round(i/length(fevd.matrix),2))
  flush.console()
}

####is.crisis
fevd.date <- sp$Date[-c(1:251)]
is.crisis.T <- c()
is.crisis.F <- c()
fevd.matrix.diag0 <- fevd.matrix
for (i in 1:length(fevd.matrix)) {
  diag(fevd.matrix.diag0[[i]]) <- 0
  for (j in 1:nrow(crisis.date)) {
    if(as.Date(crisis.date$begin[j], format='%Y-%m-%d')<= as.Date(fevd.date[i],format='%Y-%m-%d')  & 
       as.Date(fevd.date[i], format='%Y-%m-%d') <= as.Date(crisis.date$end[j], format='%Y-%m-%d')){
      #cat(as.character(fevd.date[i]))
      #print("TRUE")
      is.crisis.T<-c(is.crisis.T, i)
    }else{
      is.crisis.F<-c(is.crisis.F, i)
    }
  }
}

fevd.crisis.T <- fevd.matrix.diag0[is.crisis.T] %>% unlist %>% as.data.frame
fevd.crisis.T$is.crisis <- "During Crisis"
fevd.crisis.F <- fevd.matrix.diag0[is.crisis.F] %>% unlist %>% as.data.frame
fevd.crisis.F$is.crisis <- "Usual Time"
fevd.crisis <- rbind(fevd.crisis.T, fevd.crisis.F)
fevd.crisis <- fevd.crisis[-which(fevd.crisis[,1]==0),]
fevd.crisis <- fevd.crisis[-which(fevd.crisis[,1]<0.07),]

fevd.crisis$is.crisis <- factor(fevd.crisis$is.crisis, order = TRUE, levels = c("Usual Time","During Crisis"))
ggplot(fevd.crisis, aes(x = ., fill = is.crisis)) +
  geom_density(alpha = 0.3)


#######whole sample matrix
VAR <- VAR(sp[,-1], p = 2, type = c("none"), season = NULL, exogen = NULL)
temp <- genFEVD(est = VAR, n.ahead = 1, no.corr = F)  %>%  as.data.frame
rownames(temp) <- group[2,names(temp)]
colnames(temp) <- group[2,names(temp)]

tab <- temp * 100
#tab[tab == 0] <- NA
diag(tab) <- 0
tab <- round(tab, 0)
tab[names(tab)] <- lapply(tab[names(tab)], function(x)
  paste0("\\cellcolor{", ColorRange.MinMax(x, color = "red",min.x=0, max.x=100), "}", x))
tab[tab == "\\cellcolor{NA}NA"] <- "\\cellcolor{white}"
tab[tab == "\\cellcolor{NA}0"] <- "\\cellcolor{white} "
library(xtable)
tab <- xtable(tab,
              caption = "generalized forecast error variance",
              label = "fevd"
)

print(tab, 
      file = "latex/report/table/fevd.tex",
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      include.rownames = T,
      include.colnames = T
)
#######whole sample matrix corrplot
#install.packages("corrplot")
library(corrplot)
VAR <- VAR(g.sp, p = 2, type = c("none"), season = NULL, exogen = NULL)
temp <- genFEVD(est = VAR, n.ahead = 1, no.corr = F)
colnames(temp) <- group[2,]
rownames(temp) <- group[2,]
#delete <- quantile(unlist(temp), 0.5)
delete <- 0.07

#diag(temp0) <- 0
p.mat0 <- matrix(rep(0,ncol(temp)*ncol(temp)),ncol(temp),ncol(temp))
diag(p.mat0) <- 1
p.mat <- p.mat0
p.mat[temp< delete]<- 1
diag(p.mat) <- 1

png(file="latex/report/figure/fevd-whole.png")
corrplot(temp, method = "color", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = T,
         cl.lim = c(min(temp, na.rm = TRUE), max(temp, na.rm = TRUE)),
         tl.col = "black",
         col = colorRampPalette(c("white", "red"))(20),
         is.corr = FALSE,
         na.label = "o")
dev.off()

png(file="latex/report/figure/fevd-whole-0.png")
figure <- corrplot(temp, method = "color", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         p.mat = p.mat0, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = T,
         cl.lim = c(min(temp, na.rm = TRUE), max(temp, na.rm = TRUE)),
         tl.col = "black",
         col = colorRampPalette(c("white", "red"))(20),
         is.corr = FALSE,
         na.label = "o")
dev.off()

#rolling test
dy.genFEVD250fix1 <- dy.genFEVD(g.sp, "fix", 250, n.ahead = 1)

#the latter network ~ the former network 
## for binary network object
coef.ergm.l <- c()
p.ergm.l <- c()
total <- length(dy.genFEVD250fix1)-1
pb <- txtProgressBar(min = 0, max = total, style = 3)
for (i in 1:(length(dy.genFEVD250fix1)-1)) {
  
  temp <- dy.genFEVD250fix1[[i+1]]
  temp[temp < 0.07] <- 0
  temp <- network(temp,                  
                       directed = TRUE, 
                       loops = FALSE,# self loop
                       matrix.type = "adjacency")
  result.ergm <- ergm(temp ~ edgecov(dy.genFEVD250fix1[[i]]),#gwesp(0.25,fixed=T) + 
       verbose=F)
  #plot(gof(result.ergm))
  #mcmc.diagnostics(result.ergm)
  coef.ergm.l <- c(coef.ergm.l,result.ergm$coef)
  p.ergm.l <- c(p.ergm.l,summary(result.ergm)$coefs[4])
  
  setTxtProgressBar(pb, i)
}
close(pb)

data <- cbind(unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(p.ergm.l)) %>% as.data.frame
data <- rbind(data[1,],data)
names(data) <- c("coef0.01","coef0.05","coef0.1","p-value")
data[data$"p-value">0.01,1] <- NA
data[data$"p-value">0.05 | data$"p-value"<0.01,2] <- NA
data[data$"p-value">0.1 | data$"p-value"<0.05,3] <- NA
data <- xts(data, as.Date(sp$Date[-c(1:250)], format='%Y-%m-%d')[-1])
dy.data <- merge(data, is.higher$crisis.sma20)
names(dy.data)[ncol(dy.data)] <- "Crisis"

dy.main <- "Rolling ERGM: network(t+1) ~ network(t)"
color <- c("purple","blue","green","yellow")

dygraph.interbank(dy.data, dy.main, color, begin.dateWindow = index(data)[1]) %>%
  dyShading(from = 0, to = 0.01, axis = "y", color = colorRampPalette(c("red", "white"))(4)[1]) %>%
  dyShading(from = 0.01, to = 0.05, axis = "y", color = colorRampPalette(c("red", "white"))(4)[2]) %>%
  dyShading(from = 0.05, to = 0.1, axis = "y", color = colorRampPalette(c("red", "white"))(4)[3]) %>%
  dyAxis("y", label = "Coefficients", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")

# the latter network ~ the former network 
## for valued network object
## Rolling ERGMs: the latter network ~ the former network with edge (as constant term)
data <- dy.genFEVD250fix1
coef.ergm.l <- c()
p.ergm.l <- c()
total <- length(data)-1
max.l <- c()
pb <- txtProgressBar(min = 0, max = total, style = 3)
for (i in 1:(total-1)) {
  
  temp.y <- data[[i+1]] * 100  
  temp.y <- apply(temp.y, c(1,2), round)
  diag(temp.y) <- 0
  max.temp.y <- max(temp.y)
  temp.y <- network(temp.y,                  
                    directed = TRUE, 
                    loops = FALSE,# self loop
                    matrix.type = "adjacency",
                    ignore.eval=FALSE, 
                    names.eval="weight")
  
  temp.x <- data[[i]] * 100  
  temp.x <- apply(temp.x, c(1,2), round)
  diag(temp.x) <- 0
  
  p <- sum(temp.y %e% "weight")/max.temp.y/network.dyadcount(temp.y)
  init <- log(p/(1-p)) 
  
  result.vergm <-
    ergm(temp.y ~ sum + edgecov(temp.x),
         response="weight", 
         reference=~Binomial(max.temp.y),
         control=control.ergm(init=c(init, rep(0, 1))),
         verbose=F)
  
  #plot(gof(result.ergm))
  #mcmc.diagnostics(result.vergm)
  coef.ergm.l <- c(coef.ergm.l,result.vergm$coef[2])#rbind
  p.vergm <- summary(result.vergm)$coefs[2,4]
  p.ergm.l <- c(p.ergm.l, p.vergm)#rbind
  max.l <- c(max.l, max.temp.y)
  setTxtProgressBar(pb, i)
}
close(pb)
## 
data <- cbind(unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(p.ergm.l)) %>% as.data.frame
data <- rbind(data[1,],data)
names(data) <- c("coef0.01","coef0.05","coef0.1","p-value")
data[data$"p-value">0.01,1] <- NA
data[data$"p-value">0.05 | data$"p-value"<0.01,2] <- NA
data[data$"p-value">0.1 | data$"p-value"<0.05,3] <- NA
data <- xts(data, as.Date(sp$Date[-c(1:250)], format='%Y-%m-%d')[-1])
dy.data <- merge(data, is.higher$crisis.sma20)
names(dy.data)[ncol(dy.data)] <- "Crisis"

dy.main <- "Rolling ERGM: network(t+1) ~ network(t)"
color <- c("purple","blue","green","yellow")

dygraph.interbank(dy.data, dy.main, color, begin.dateWindow = index(data)[1]) %>%
  dyShading(from = 0, to = 0.01, axis = "y", color = colorRampPalette(c("red", "white"))(4)[1]) %>%
  dyShading(from = 0.01, to = 0.05, axis = "y", color = colorRampPalette(c("red", "white"))(4)[2]) %>%
  dyShading(from = 0.05, to = 0.1, axis = "y", color = colorRampPalette(c("red", "white"))(4)[3]) %>%
  dyAxis("y", label = "Coefficients", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")




#ergms for nodal covariate and edge covariate
data <- xts(sp[,-1], as.Date(sp$Date, format='%Y-%m-%d'))
period <- c(#"2006-12-31",
            "2007-12-31",
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
annual.fevd <- list()
annual.n <- list()
delete <- 0.07
for (i in 1:(length(period)-1)) {
  temp.data <- data[paste0(period[i],"/",period[i+1])] %>% as.data.frame
  g.temp.data <- as.data.frame(lapply(temp.data, log_GrowthRate))
  VAR <- VAR(g.temp.data, p = 2, type = c("none"), season = NULL, exogen = NULL)
  annual.fevd[[i]] <- genFEVD(est = VAR, n.ahead = 1, no.corr = F)
  annual.n[[i]] <- annual.fevd[[i]]
  annual.n[[i]][annual.n[[i]] < 0.07] <- 0
  annual.n[[i]] <- annual.n[[i]]*100
  annual.n[[i]] <- network(annual.n[[i]],                  
                  directed = TRUE, 
                  loops = FALSE,# self loop
                  matrix.type = "adjacency",
                  ignore.eval=FALSE,
                  names.eval='weight')
}
p.matrix.temp <- list()
for (i in 1:(length(annual.n)-2)) {
  data.interbank14.temp <-  data.interbank14[[i+8]]
  data.interbank14.temp[data.interbank14.temp==0] <- 1
  wind.temp <- raw.wind[[i+1]]
  annual.n[[i]] %v% "csh" <- data.interbank14.temp$现金及存放中央银行款项 %>% log
  annual.n[[i]] %v% "drv" <- data.interbank14.temp$衍生金融资产 %>% log
  annual.n[[i]] %v% "ast" <- data.interbank14.temp$资产总计 %>% log
  annual.n[[i]] %v% "bcb" <- data.interbank14.temp$向中央银行借款 %>% log
  annual.n[[i]] %v% "stl" <- data.interbank14.temp$短期借款 %>% log
  #annual.n[[i]] %v% "test" <- data.interbank14.temp$长期借款 %>% log
  annual.n[[i]] %v% "lib" <- data.interbank14.temp$负债合计 %>% log
  annual.n[[i]] %v% "udp" <- data.interbank14.temp$未分配利润 %>% log
  annual.n[[i]] %v% "pra" <- wind.temp$"净利润(万元)"/wind.temp$"资产总计(万元)"*100
  annual.n[[i]] %v% "dra" <- wind.temp$"负债合计(万元)"/wind.temp$"资产总计(万元)"*100
  annual.n[[i]] %v% "len" <- wind.temp$"拆出资金(万元)"
  annual.n[[i]] %v% "bor" <- wind.temp$"拆入资金(万元)"
  p.matrix.temp[[i+8]] <- p.matrix[[i+8]]
  p.matrix.temp[[i+8]][p.matrix.temp[[i+8]]==0] <- 1
  }

temp <- ergm(annual.n[[i]] ~ 
       nodecov('pra') ,
       verbose=F,
       response="weight", 
       reference=~Binomial(1),
       estimate="MLE")

summary(temp)


table.all <- matrix(NA,6,9) %>% as.data.frame
for (i in 1:9) {
  ergm.temp <- ergm(annual.n[[i]] ~ 
                 nodecov('pra') + 
                 nodecov('dra') +
                 edgecov(log(p.matrix.temp[[i+8]]))
               ,verbose=F
  )
  #summary(temp)
  result.temp <- summary(ergm.temp)$coefs
  result.temp <- result[,-3]
  
  sig <- lapply(result[,3],FUN=significance) %>%  unlist
  result <- round(result,2)
  result <- result[,c(1,2)]
  result[,1] <- paste0(result[,1], "$^{",sig,"}$")
  result[,2] <- paste0("(",result[,2],")")
  table <- matrix(NA,6,1) %>% as.data.frame
  table[1,1] <- result[1,1]
  table[2,1] <- result[1,2]
  table[3,1] <- result[2,1]
  table[4,1] <- result[2,2]
  table[5,1] <- result[3,1]
  table[6,1] <- result[3,2]
  
  
  table.all[,i] <- table
}

tab <- cbind(c("pra","","dra","","int",""),table.all)
names(tab) <-c("",paste0("",c(2008:2016)))


tab.label <- "ergm"
library(xtable)
tab <- xtable(tab,
              caption = paste("result of annual ergm"),
              label = tab.label
)
align(tab) <- "lllllllllll"

print(tab, 
      file = paste0("latex/report/table/",tab.label,".tex"),
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      include.rownames = F,
      include.colnames = T
)

#################################################################
# sp since 2000 is.listed
#################################################################
group.stockprice <- read_excel(path = "data/stockprice.xlsx",
                               sheet = "group",
                               skip = 0,
                               col_names = T,
                               col_types = rep("text", 5)) %>% as.data.frame
group.stockprice$Eclass <- factor(group.stockprice$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks"))
group.stockprice.all <- group.stockprice[order(group.stockprice$Eclass,decreasing = F),] 
rownames(group.stockprice.all) <- group.stockprice.all$Abre
group.stockprice.all$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(10)[-10], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6])

raw.stockprice <- read_excel(path = "data/stockprice.xlsx",
                             sheet = 1,
                             skip = 4,
                             col_names = F,
                             col_types = c("date", rep("numeric", 25))) %>% as.data.frame
names(raw.stockprice) <- c("Date",group.stockprice$Abbr)

sp.all <- raw.stockprice[,c("Date",group.stockprice.all$Abbr)]
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
for (i in 1:length(c(1991:2017))) {
   temp <- sp.all[paste0(Date.year[i],"/",Date.year[i+1])]
   l.sp.all[[i]] <- temp[,l.is.listed[[i]]] %>% na.omit
   print(c(1991:2017)[[i]])
   print(dim(l.sp.all[[i]]))
}
names(l.sp.all) <- paste0("year",c(1991:2017))

l.sp.all <- l.sp.all[-c(1:length(1991:1999))]
l.is.listed <- l.is.listed[-c(1:length(1991:1999))]
names(l.sp.all)

save(l.is.listed,l.sp.all,file = "data/Rdata/latex_sp.all.Rdata")
