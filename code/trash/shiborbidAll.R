# load raw data
raw.name.bid <- read_excel(path = "data/bindgroup.xlsx",
                           sheet = "name",
                           skip = 0,
                           col_names = T,
                           col_types = rep("text", 5)) %>% as.data.frame
raw.name.bid$Eclass <- factor(raw.name.bid$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks","Foreign Banks"))
raw.name.bid <- raw.name.bid[order(raw.name.bid$Eclass,decreasing = F),] 
rownames(raw.name.bid) <- raw.name.bid$Cname
raw.name.bid$color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FF9800", "white"))(11)[-11], colorRampPalette(c("#33691E", "white"))(7)[-7],colorRampPalette(c("#1A237E", "white"))(6)[-6],"#2196F3")

## shibor rate bid price
fnlist.shibor.bid <- dir("data/bid")
raw.shibor.bid <- list()
shibor.bid <- c()
all.bid.banks <- c()
all.shibor <- c()
locate <- c()
for(i in 3:length(fnlist.shibor.bid)){
  if(as.numeric(substr(fnlist.shibor.bid[i],4,7))<=2015){
    raw.shibor.bid[[i]] <- read_excel(path = paste0("data/bid/",fnlist.shibor.bid[i]),
                                      sheet = "Sheet",
                                      skip = 0,
                                      col_names = T,
                                      col_types = c("date", "text", rep("numeric", 16)))[-c(1,2),-c(seq(4,18,2))] %>% as.data.frame
    shibor.bid <- rbind(shibor.bid,raw.shibor.bid[[i]])
  }else{
    raw.shibor.bid[[i]] <- read_excel(path = paste0("data/bid/",fnlist.shibor.bid[i]),
                                      sheet = "Sheet",
                                      skip = 0,
                                      col_names = T,
                                      col_types = c("date", "text", rep("numeric", 8))) %>% as.data.frame
    shibor.bid <- rbind(shibor.bid,raw.shibor.bid[[i]])
  }
  
  #print(substr(fnlist.shibor.bid[i],4,7))
  #print(dim(raw.shibor.bid[[i]]))
  #print(unique(raw.shibor.bid[[i]]$报价银行))
  #print(length(unique(raw.shibor.bid[[i]]$报价银行)))
  #temp <- match(unique(raw.shibor.bid[[i]]$报价银行), all.bid.banks)
  #locate <- c(temp,locate)
  #print("===========================================")
  all.bid.banks <- c(all.bid.banks,unique(raw.shibor.bid[[i]]$报价银行)) %>% unique
}
shibor.bid$日期<- substr(shibor.bid$日期, 1, 10)

for(i in 3:length(fnlist.shibor.bid)){
  print(substr(fnlist.shibor.bid[i],4,7))
  print(dim(raw.shibor.bid[[i]]))
  print(unique(raw.shibor.bid[[i]]$报价银行))
  print(length(unique(raw.shibor.bid[[i]]$报价银行)))
  temp <- match(unique(raw.shibor.bid[[i]]$报价银行), all.bid.banks)
  locate <- c(temp,locate)
  print("===========================================")
}


# locate samples
all.bid.banks
locate
table.locate <- table(locate) %>% as.data.frame
rownames(table.locate) <- all.bid.banks

#
all.bid <- c()
for(i in 3:length(fnlist.shibor.bid)){
  all.bid <- rbind(all.bid,raw.shibor.bid[[i]])
}

names(all.bid) <- c("Date", "Bank", "ON", "W1", "W2", "M1", "M3", "M6", "M9", "Y1")
all.bid$Date <- substr(all.bid$Date,1,10)
all.bid <- melt(all.bid, id = c("Date", "Bank"), variable.name = "Term", value.name = "Bid", na.rm=T)
all.bid <- dcast(all.bid, Date + Term ~ Bank , fun.aggregate = mean) %>% as.data.frame
names(all.bid)
c("上海银行", "中信银行", "中国银行", "交通银行", "光大银行", "兴业银行", "农业银行", 
  "北京银行", "华夏银行", "南京银行", "国开行", "工商银行", "广发银行", "建设银行", 
  "德银中国", "招商银行", "民生银行", "汇丰中国", "浦发银行", "渣打银行", "邮储银行")


all.bid.ON <- all.bid[all.bid$Term == "ON",-c(2)]
aggr(all.bid.ON, prop=FALSE, numbers=TRUE)  
data.bond.ON <- merge(all.bid.ON, bond, by = intersect(names(all.bid), names(bond)))
data.bond.ON[,-1] <- apply(data.bond.ON[,-1], 2, f.na.approx)


dy.data <- xts(data.bond.ON[,-1], as.Date(data.bond.ON$Date, format='%Y-%m-%d'))
dy.main <- "Shanghai Interbank Offered Rate from 2006 to 2018"
dygraph.interbank(dy.data, dy.main, color)


all.bid.W1 <- all.bid[all.bid$Term == "W1",-c(2)]
data.bond.W1 <- merge(all.bid.W1, bond, by = intersect(names(all.bid), names(bond)))
data.bond.W1[,-1] <- apply(data.bond.W1[,-1], 2, f.na.approx)

all.bid.W2 <- all.bid[all.bid$Term == "W2",-c(2)]
data.bond.W2 <- merge(all.bid.W2, bond, by = intersect(names(all.bid), names(bond)))
data.bond.W2[,-1] <- apply(data.bond.W2[,-1], 2, f.na.approx)

all.bid.M1 <- all.bid[all.bid$Term == "M1",-c(2)]
data.bond.M1 <- merge(all.bid.M1, bond, by = intersect(names(all.bid), names(bond)))
data.bond.M1[,-1] <- apply(data.bond.M1[,-1], 2, f.na.approx)

all.bid.M3 <- all.bid[all.bid$Term == "M3",-c(2)]
data.bond.M3 <- merge(all.bid.M3, bond, by = intersect(names(all.bid), names(bond)))
data.bond.M3[,-1] <- apply(data.bond.M3[,-1], 2, f.na.approx)

all.bid.M6 <- all.bid[all.bid$Term == "M6",-c(2)]
data.bond.M6 <- merge(all.bid.M6, bond, by = intersect(names(all.bid), names(bond)))
data.bond.M6[,-1] <- apply(data.bond.M6[,-1], 2, f.na.approx)

all.bid.M9 <- all.bid[all.bid$Term == "M9",-c(2)]
data.bond.M9 <- merge(all.bid.M9, bond, by = intersect(names(all.bid), names(bond)))
data.bond.M9[,-1] <- apply(data.bond.M9[,-1], 2, f.na.approx)

all.bid.Y1 <- all.bid[all.bid$Term == "Y1",-c(2)]
data.bond.Y1 <- merge(all.bid.Y1, bond, by = intersect(names(all.bid), names(bond)))
data.bond.Y1[,-1] <- apply(data.bond.Y1[,-1], 2, f.na.approx)
# funding cost spillover
## VAR gives too much negative coefficients
data <- all.bid[,-1]
VAR <- VAR(data, p=1, type = "const", exogen = bond.merge)
su.VAR <- summary(VAR)$varresult
coef.VAR <- data.frame(matrix(NA,ncol(data),ncol(data)))
p.VAR <- data.frame(matrix(NA,ncol(data),ncol(data)))
n <- ncol(data)
for (i in 1:n) {
  temp <- su.VAR[[i]]$coefficients %>% as.data.frame
  coef.VAR[i,] <- temp[1:n,1]
  p.VAR[i,] <- temp[1:n,4]
}
names(coef.VAR) <- names(data)
coef.VAR[p.VAR>= 0.001] <- NA
coef.VAR1 <- coef.VAR
View(coef.VAR1)  

# funding cost spillover
## adaptive elastic net 
data.bond <- data.bond.ON
data.bond <- data.bond.W1
data.bond <- data.bond.W2
data.bond <- data.bond.M1
data.bond <- data.bond.M3
data.bond <- data.bond.M6
data.bond <- data.bond.M9
data.bond <- data.bond.Y1

data.aenet <- xts(data.bond[,-1], as.Date(data.bond$Date, format='%Y-%m-%d'))
coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet)-1)
m.coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet))

for (i in 1:(ncol(data.aenet)-1)) {
  temp.x <- data.aenet[-1,-i] %>% as.matrix
  temp.y <- stats::lag(data.aenet[,i],1)[-1,]
  aenet.fit <- aenet(
    x = temp.x, y = temp.y,lower.limits = 0,family = "gaussian",
    alphas = 0.5, seed = 1002,init = "enet",rule = "lambda.min",tune = "cv", nfolds = 10
  )
  coef.aenet[i,] <- coef(aenet.fit)
}
coef.aenet <- coef.aenet %>% as.data.frame

#coef.aenet[lower.tri(coef.aenet, diag = F)] <- 1
#coef.aenet[upper.tri(coef.aenet, diag = T)] <- 0
m.coef.aenet[lower.tri(m.coef.aenet, diag = F)] <- coef.aenet[lower.tri(coef.aenet, diag = F)]
m.coef.aenet[upper.tri(m.coef.aenet, diag = F)] <- coef.aenet[upper.tri(coef.aenet, diag = T)]
m.coef.aenet <- m.coef.aenet %>% as.data.frame()
names(m.coef.aenet) <- names(data.bond[,-1])
rownames(m.coef.aenet) <- names(data.aenet)
m.coef.aenet[m.coef.aenet == 0] <- NA
View(m.coef.aenet)

# funding cost spillover
## glmnet 
data.bond <- data
data.aenet <- xts(data.bond[,-1], as.Date(data.bond$Date, format='%Y-%m-%d'))
data.aenet<- data.aenet[,!is.na(data.aenet[1,])]
#data.aenet <- data.aenet[,-c(9,11)] %>% na.omit
coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet)-1)
m.coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet))

for (i in 1:(ncol(data.aenet)-1)) {
  temp.x <- data.aenet[-1,-i] %>% as.matrix
  temp.y <- stats::lag(data.aenet[,i],1)[-1,]
  cv <- cv.glmnet(x = temp.x, y = temp.y,type.measure="mse")
  fit <- glmnet(
    x = temp.x, y = temp.y,lower.limits = 0,lambda = cv$lambda.min, alpha = 0.5,  family="gaussian"
  )
  coef.aenet[i,] <- coef(fit)[-1]
}
coef.aenet <- coef.aenet %>% as.data.frame


m.coef.aenet[lower.tri(m.coef.aenet, diag = F)] <- coef.aenet[lower.tri(coef.aenet, diag = F)]
m.coef.aenet[upper.tri(m.coef.aenet, diag = F)] <- coef.aenet[upper.tri(coef.aenet, diag = T)]
m.coef.aenet <- m.coef.aenet %>% as.data.frame()
names(m.coef.aenet) <- names(data.aenet)
rownames(m.coef.aenet) <- names(data.aenet)

m.coef.aenet[m.coef.aenet == 0] <- NA
View(m.coef.aenet)

# dynamic funding cost using aenet
span <- list(c("2008-01-01","2009-01-01"),
             c("2009-01-01","2010-01-01"),
             c("2010-01-01","2011-01-01"),
             c("2011-01-01","2012-01-01"),
             c("2012-01-01","2013-01-01"),
             c("2013-01-01","2014-01-01"),
             c("2014-01-01","2015-01-01"),
             c("2015-01-01","2016-01-01"),
             c("2016-01-01","2017-01-01"),
             c("2017-01-01","2018-01-01"))

# There are difference to 3 order if I swich from method cv to aic
aenet.ON <- list()  
for (t in 1:length(span)) {
  aenet.ON[[t]] <- aenet.span(data.bond.ON, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.W1 <- list()  
for (t in 1:length(span)) {
  aenet.W1[[t]] <- aenet.span(data.bond.W1, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.W2 <- list()  
for (t in 1:length(span)) {
  aenet.W2[[t]] <- aenet.span(data.bond.W2, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.M1 <- list()  
for (t in 1:length(span)) {
  aenet.M1[[t]] <- aenet.span(data.bond.M1, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.M3 <- list()  
for (t in 1:length(span)) {
  aenet.M3[[t]] <- aenet.span(data.bond.M3, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.M6 <- list()  
for (t in 1:length(span)) {
  aenet.M6[[t]] <- aenet.span(data.bond.M6, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.M9 <- list()  
for (t in 1:length(span)) {
  aenet.M9[[t]] <- aenet.span(data.bond.M9, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}

aenet.Y1 <- list()  
for (t in 1:length(span)) {
  aenet.Y1[[t]] <- aenet.span(data.bond.Y1, start.point = span[[t]][1], end.point = span[[t]][2]) %>% as.matrix
}


group.bid <- raw.name.bid[colnames(aenet.Y1[[1]][1:15,1:15]),]
group.bid$Eclass <- factor(group.bid$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks", "Rural Commercial Banks","Foreign Banks"))
group.bid <- group.bid[order(group.bid$Eclass, decreasing = F),]

for (t in 1:length(span)) {
  temp <- aenet.Y1[[1]][1:15,1:15]
  temp <- temp[group.bid$Cname,group.bid$Cname] %>% t
  temp[is.na(temp)] <- 0
  igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
  networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
  networkD3.matrix[[2]]$group <- group.bid[,5]
  networkD3.matrix[[2]]$name <- group.bid[,2]#group[1,]
  
  networkD3.matrix[[2]]$size <- rowSums(temp)
  
  networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*10
  networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu*5
  
  matrix.date <- data.frame(name="",size=0, group=as.character(substr(fevd.date[i],1,10)))
  networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
  
  ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","#3F51B5","white"]);'
  decide.color <- c()  
  for (j in 1:nrow(crisis.date)) {
    if(as.Date(crisis.date$begin[j], format='%Y-%m-%d')<= as.Date(fevd.date[i],format='%Y-%m-%d')  & 
       as.Date(fevd.date[i], format='%Y-%m-%d') <= as.Date(crisis.date$end[j], format='%Y-%m-%d')){
      #cat(as.character(fevd.date[i]))
      #print("TRUE")
      decide.color<-c(decide.color,1)
    }else{
      decide.color<-c(decide.color,0)
    }
  }
  if(sum(decide.color)>=1){
    ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","black"]);'
  }
  
  #time.start <- Sys.time()
  
  rbokeh.bank <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                              Source = "source", Target = "target", Value = "value", 
                              arrows = T, linkDistance = JS("function(d){return 1 / d.value * 200 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                              NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                              Group = "group", legend = T, fontSize = 15, opacity = 0.8, opacityNoHover = 1,
                              height = 700 , width = 700, bounded = F, zoom = T, charge = -100)
  
  widget2png(rbokeh.bank, file = paste0("latex/report/igraph/bank", (i-1)/10+1, ".png"), timeout = 2000)
  #time.end <- Sys.time()
  #running.time <- (time.end - time.start)
  #print(running.time)
  
  cat("\r", round(i/length(fevd.matrix),2))
  flush.console()
}



