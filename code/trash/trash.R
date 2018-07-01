# fevd
fevd.matrix <- fevd.var(g.sp, mode = "fix", span = 250, n.ahead=1)
require(lattice)
temp <- fevd.matrix[[1]]
temp <- as.numeric(temp)
hist(temp,breaks=20,col="red4",xlab="fevd", main="histogram")
quantile(temp,c(0,0.2,0.5,0.8,1))

fevd.date <- sp$Date[-c(1:251)]
crisis.date<- rbind(
  c(begin = "2006-01-01", end = "2008-01-24"),
  c(begin = "2010-11-08", end = "2011-03-04"),
  c(begin = "2011-05-05", end = "2012-03-05"),
  c(begin = "2013-06-05", end = "2014-05-27"),
  c(begin = "2014-12-17", end = "2015-04-10"),
  c(begin = "2017-01-10", end = "2017-07-07"),
  c(begin = "2017-11-08", end = "2018-01-03")
) %>% as.data.frame

for (i in seq(1231,length(fevd.matrix),10)) {
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
  matrix.date <- data.frame(name="",size=0, group=as.character(substr(fevd.date[i],1,10)))
  networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
  
  ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white"]);'
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
                              arrows = T, linkDistance = JS("function(d){return 1 / d.value * 20 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                              NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                              Group = "group", legend = T, fontSize = 10, opacity = 0.8, opacityNoHover = 1,
                              height = 700 , width = 700, bounded = F, zoom = T, charge = -1000)
  widget2png(rbokeh.bank, file = paste0("latex/report/igraph/bank", (i-1)/10+1, ".png"), timeout = 2000)
  #time.end <- Sys.time()
  #running.time <- (time.end - time.start)
  #print(running.time)
  
  cat("\r", round(i/length(fevd.matrix),2))
  flush.console()
}



#the former network ~ the latter network # not used
coef.ergm.f <- c()
p.ergm.f <- c()
total <- length(dy.genFEVD250fix1)-1
pb <- txtProgressBar(min = 0, max = total, style = 3)
for (i in 1:(length(dy.genFEVD250fix1)-1)) {
  
  temp <- dy.genFEVD250fix1[[i]]
  temp[temp < 0.07] <- 0
  temp <- network(temp,                  
                  directed = TRUE, 
                  loops = FALSE,# self loop
                  matrix.type = "adjacency")
  result.ergm <- ergm(temp~ edgecov(dy.genFEVD250fix1[[i+1]]),
                      verbose=F)
  coef.ergm.f <- c(coef.ergm.f,result.ergm$coef)
  p.ergm.f <- c(p.ergm.f,summary(result.ergm)$coefs[4])
  
  setTxtProgressBar(pb, i)
}
close(pb)

data <- cbind(unlist(coef.ergm.f),unlist(p.ergm.f)) %>% as.data.frame
data <- rbind(data[1,],data)
names(data) <- c("coef","p-value")
data[data$"p-value">0.1,1] <- NA
dy.data <- xts(data, as.Date(sp$Date[-c(1:250)], format='%Y-%m-%d')[-1])

dy.main <- "Rolling ERGM: network(t) ~ network(t+1)"
color <- c("black","gray")

dygraph.interbank(dy.data, dy.main, color) %>%
  dyShading(from = 0, to = 0.01, axis = "y", color = colorRampPalette(c("red", "white"))(4)[1]) %>%
  dyShading(from = 0.01, to = 0.05, axis = "y", color = colorRampPalette(c("red", "white"))(4)[2]) %>%
  dyShading(from = 0.05, to = 0.1, axis = "y", color = colorRampPalette(c("red", "white"))(4)[3])



###########
#the latter network ~ the former network with edge (as constant term)
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
  result.ergm <- ergm(temp~ edges + edgecov(dy.genFEVD250fix1[[i]]),
                      verbose=F)
  coef.ergm.l <- c(coef.ergm.l, result.ergm$coef[2])
  p.ergm.l <- c(p.ergm.l,summary(result.ergm)$coefs[2,4])
  
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
data <- dy.genFEVD250fix1
max.temp.y <- max(unlist(data))
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
  result.ergm <- ergm(temp~ edgecov(dy.genFEVD250fix1[[i]]),
                      verbose=F)
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





#dygraph###########################################################################################

dy.coef <- coef.ergm.l# 0
#dy.coef[p.ergm.l > 0.01] <- NA # 1
#dy.coef[p.ergm.l > 0.05 | p.ergm.l < 0.01] <- NA # 5
#dy.coef[p.ergm.l > 0.1 | p.ergm.l < 0.05] <- NA # 10
dy.coef <- xts(dy.coef, as.Date(Date[-(1:251)], format='%Y-%m-%d'))
dy.coef <- merge(dy.coef, is.higher$crisis.sma20,join = "left")
names(dy.coef)[length(names(dy.coef))] <- "Crisis"

color <- colorRampPalette(c("red", "blue"))(8)
dy.main <- " "

saveWidget.dygraph <- 
  dygraph.interbank(dy.data=dy.coef, dy.main, color, begin.dateWindow = index(dy.coef)[1]) %>%
  dyAxis("y", label = "Coefficients", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")

file.path<-paste0("html/", set,".html")
saveWidget(saveWidget.dygraph, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = T, libdir = NULL,background = "white", title = set, knitrOptions = list())


###############
dyCoefErgm.yearly <- function(data, set, 
                              inclusion.edgecov,
                              inclusion.nodecov,
                              inclusion.nodeocov,
                              inclusion.nodecov,
                              Date = y.period[-c(1,11)], is.higher, wind,
                              csv = FALSE, fig = FALSE, tab = FALSE)
{
  if(missing(csv)){csv = FALSE}
  if(missing(fig)){fig = FALSE}
  if(missing(tab)){tab = FALSE}
  
  total <- length(data)
  p.ergm.l <- c()
  max.l <- c()
  
  # formula
  ergm.formula <- "temp.y~sum"
  if(!missing(inclusion.edgecov) & !missing(inclusion.nodecov)){
    n.inclusion.edgecov <- length(inclusion.edgecov)
    n.inclusion.nodecov <- length(inclusion.nodecov)
    n.inclusion <- n.inclusion.edgecov + n.inclusion.nodecov
    for (i in 1:n.inclusion.edgecov) {
      temp.formula <- paste0("+edgecov(temp.",inclusion.edgecov[i],")")
      ergm.formula <- paste0(ergm.formula,temp.formula)
    }
    for (i in 1:n.inclusion.nodecov) {
      temp.formula <- paste0("+nodecov('",inclusion.nodecov[i],"')")
      ergm.formula <- paste0(ergm.formula,temp.formula)
    }
    ergm.formula <- as.formula(ergm.formula)
    inclusion <- c(inclusion.edgecov, inclusion.nodecov)
  }
  
  if(!missing(inclusion.edgecov) & missing(inclusion.nodecov)){
    n.inclusion.edgecov <- length(inclusion.edgecov)
    n.inclusion <- n.inclusion.edgecov
    for (i in 1:n.inclusion.edgecov) {
      temp.formula <- paste0("+edgecov(temp.",inclusion.edgecov[i],")")
      ergm.formula <- paste0(ergm.formula,temp.formula)
    }
    ergm.formula <- as.formula(ergm.formula)
    inclusion <- inclusion.edgecov
  }
  
  if(missing(inclusion.edgecov) & !missing(inclusion.nodecov)){
    n.inclusion.nodecov <- length(inclusion.nodecov)
    n.inclusion <- n.inclusion.nodecov
    for (i in 1:n.inclusion.nodecov) {
      temp.formula <- paste0("+nodecov('",inclusion.nodecov[i],"')")
      ergm.formula <- paste0(ergm.formula,temp.formula)
    }
    ergm.formula <- as.formula(ergm.formula)
    inclusion <- inclusion.nodecov
  }
  
  coef.ergm.l <- data.frame(matrix(NA,total,n.inclusion)); names(coef.ergm.l) <- paste0("coef.",inclusion) 
  p.ergm.l <- data.frame(matrix(NA,total,n.inclusion));    names(p.ergm.l) <- paste0("p.",inclusion) 
  std.ergm.l <- data.frame(matrix(NA,total,n.inclusion));  names(std.ergm.l) <- paste0("std.",inclusion) 
  result.vergm.l <- list()
  min.max <- lapply(data, abs)
  min.max <- min(lapply(min.max, max) %>% unlist)
  scale <- floor(log(1/min.max,10))+2
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  for (i in 1:total) {
    print(paste0("##########################",i,"##########################"))
    
    temp.y <- data[[i]] * (10^scale)
    temp.y <- apply(temp.y, c(1,2), round)
    diag(temp.y) <- 0
    max.temp.y <- max(temp.y)
    temp.y <- network(temp.y,
                      directed = TRUE,
                      loops = FALSE,# self loop
                      matrix.type = "adjacency",
                      ignore.eval=FALSE,
                      names.eval="weight")
    ########################################################################
    if(!missing(wind)){
      temp.wind <- wind[[i]]
      temp.y %v% "ass" <- temp.wind$ass %>% log #资产总计(万元)
      temp.y %v% "dbt" <- temp.wind$dbt %>% log #负债合计(万元)
      temp.y %v% "rsr" <- temp.wind$rsr  %>% log #一般风险准备(万元)
      temp.y %v% "sty" <- temp.wind$income.fee/temp.wind$income #手续费及佣金收入(万元)/营业收入(万元)
      temp.y %v% "ltc" <- temp.wind$ltc  %>% log #现金及存放中央银行款项(万元)
      temp.y %v% "bfc" <- temp.wind$bfc %>% log #"向中央银行借款(万元)"
      temp.y %v% "lnd" <- temp.wind$lnd %>% log #"拆出资金(万元)"
      temp.y %v% "brr" <- temp.wind$brr  %>% log #"拆入资金(万元)"
    }
    ########################################################################
    p <- sum(abs(temp.y %e% "weight"))/max.temp.y/network.dyadcount(temp.y)
    init <- log(p/(1-p))
    #if(!is.na(match("ON",inclusion.edgecov))){}
    if(!missing(inclusion.edgecov)){if(!is.na(match("ON",inclusion.edgecov))){
      temp.ON <- aenet.myl.ON[[i]] * 100  ;temp.ON <- apply(temp.ON, c(1,2), round);diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
    }
      if(!is.na(match("W1",inclusion.edgecov))){
        temp.W1 <- aenet.myl.W1[[i]] * 100  ;temp.W1 <- apply(temp.W1, c(1,2), round);diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
      }
      if(!is.na(match("W2",inclusion.edgecov))){
        temp.W2 <- aenet.myl.W2[[i]] * 100  ;temp.W2 <- apply(temp.W2, c(1,2), round);diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
      }
      if(!is.na(match("M1",inclusion.edgecov))){
        temp.M1 <- aenet.myl.M1[[i]] * 100  ;temp.M1 <- apply(temp.M1, c(1,2), round);diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
      }
      if(!is.na(match("M3",inclusion.edgecov))){
        temp.M3 <- aenet.myl.M3[[i]] * 100  ;temp.M3 <- apply(temp.M3, c(1,2), round);diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
      }
      if(!is.na(match("M6",inclusion.edgecov))){
        temp.M6 <- aenet.myl.M6[[i]] * 100  ;temp.M6 <- apply(temp.M6, c(1,2), round);diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
      }
      if(!is.na(match("M9",inclusion.edgecov))){
        temp.M9 <- aenet.myl.M9[[i]] * 100  ;temp.M9 <- apply(temp.M9, c(1,2), round);diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
      }
      if(!is.na(match("Y1",inclusion.edgecov))){
        temp.Y1 <- aenet.myl.Y1[[i]] * 100  ;temp.Y1 <- apply(temp.Y1, c(1,2), round);diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
      }
      if(!is.na(match("loan",inclusion.edgecov))){
        temp.loan <- loan.network[[i]] %>% log  ;temp.loan <- apply(temp.loan, c(1,2), round);diag(temp.loan) <- 0;temp.Y1[is.na(temp.loan)] <- 0
      }}
    
    result.vergm <-
      ergm(ergm.formula,
           response="weight",
           reference=~Binomial(max.temp.y),
           control=control.ergm(init=c(init, rep(0, n.inclusion))),
           verbose=F)
    #plot(gof(result.ergm))
    #mcmc.diagnostics(result.vergm)
    result.vergm.l[[i]] <- result.vergm
    coef.ergm.l[i,] <- result.vergm$coef[2:(n.inclusion+1)]
    p.ergm.l[i,] <- summary(result.vergm)$coefs[2:(n.inclusion+1),4]
    std.ergm.l[i,] <- summary(result.vergm)$coefs[2:(n.inclusion+1),2]
    
    max.l <- c(max.l, max.temp.y)
    setTxtProgressBar(pb, i)
    
    if(csv){
      timeend <- Sys.time()
      runningtime<-timeend-timestart
      if(runningtime/60 > 60*36-30){
        write.csv(coef.ergm.l, file = paste0("Coef",set,".csv"))
        write.csv(p.ergm.l, file = paste0("Pvalue",set,".csv"))
        write.csv(std.ergm.l, file = paste0("Std",set,".csv"))
      }
    }
  }
  close(pb)
  
  if(csv){
    write.csv(coef.ergm.l, file = paste0("Coef",set,".csv"))
    write.csv(p.ergm.l, file = paste0("Pvalue",set,".csv"))
  }
  
  #dygraph###########################################################################################
  # only one varialbe
  if(fig){
    if(n.inclusion.edgecov == 1){
      file.name <- paste0(set, "_", inclusion.edgecov)
      
      dy.coef <- cbind(unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(p.ergm.l)) %>% as.data.frame
      names(dy.coef) <- c("coef0.01","coef0.05","coef0.1","p-value")
      
      dy.coef[dy.coef$"p-value" > 0.01, 1] <- NA
      dy.coef[dy.coef$"p-value" > 0.05 | dy.coef$"p-value" < 0.01,2] <- NA
      dy.coef[dy.coef$"p-value" > 0.1 | dy.coef$"p-value" < 0.05,3] <- NA
      dy.coef <- xts(dy.coef, as.Date(Date, format='%Y-%m-%d'))
      
      is.higher.year <- apply.yearly(is.higher[,-1], mean)
      is.higher.year <- is.higher.year[paste0(Date[1],"/",Date[length(Date)])]
      is.higher.year <- xts(is.higher.year, as.Date(Date, format='%Y-%m-%d'))
      dy.coef <- merge(dy.coef, is.higher.year$crisis.sma20, join = "left")
      names(dy.coef)[length(names(dy.coef))] <- "Crisis"
      
      color <- colorRampPalette(c("red", "blue"))(3)
      dy.main <- set
      
      saveWidget.dygraph <- 
        dygraph.interbank(dy.data=subset(dy.coef, select=-p.value), dy.main, color, begin.dateWindow = index(dy.coef)[1]) %>%
        dyAxis("y", label = "Coefficients", independentTicks = T) %>%
        dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
        dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
      
      file.path<-paste0("html/", file.name,".html")
      saveWidget(saveWidget.dygraph, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = F, libdir = NULL,background = "white", title = set, knitrOptions = list())
      
    }
    
    # more than one variable
    if(n.inclusion.edgecov != 1){
      for (i in 1:4) {
        if(i==1){
          dy.coef <- coef.ergm.l # 0
          file.name <- paste0(set,"sig0")
        }
        if(i==2){
          dy.coef <- coef.ergm.l # 0
          dy.coef[p.ergm.l > 0.01] <- NA # 1
          file.name <- paste0(set,"sig1")
        }
        if(i==3){
          dy.coef <- coef.ergm.l # 0
          dy.coef[p.ergm.l > 0.05] <- NA # 1
          file.name <- paste0(set,"sig5")
        }
        if(i==4){
          dy.coef <- coef.ergm.l # 0
          dy.coef[p.ergm.l > 0.1] <- NA # 1
          file.name <- paste0(set,"sig10")
        }
        
        dy.coef <- xts(dy.coef, as.Date(Date, format='%Y-%m-%d'))
        is.higher.year <- apply.yearly(is.higher[,-1], mean)
        is.higher.year <- is.higher.year[paste0(Date[1],"/",Date[length(Date)])]
        is.higher.year <- xts(is.higher.year, as.Date(Date, format='%Y-%m-%d'))
        dy.coef <- merge(dy.coef, is.higher.year$crisis.sma20, join = "left")
        names(dy.coef)[length(names(dy.coef))] <- "Crisis"
        
        color <- colorRampPalette(c("red", "blue"))(n.inclusion.edgecov)
        dy.main <- set
        
        saveWidget.dygraph <- 
          dygraph.interbank(dy.data=dy.coef, dy.main, color, begin.dateWindow = index(dy.coef)[1]) %>%
          dyAxis("y", label = "Coefficients", independentTicks = T) %>%
          dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
          dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
        
        file.path<-paste0("html/", file.name,".html")
        saveWidget(saveWidget.dygraph, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = F, libdir = NULL,background = "white", title = set, knitrOptions = list())
      }
    }
  }
  
  if(tab){
    tab.ergm <- list()
    tab.ergm$coef <- coef.ergm.l
    tab.ergm$pvalue <- p.ergm.l
    tab.ergm$std <- std.ergm.l
    return.result <- list(result.vergm.l,tab.ergm,scale)
    names(return.result) <- c("result.vergm.l","tab.ergm","scale")
  }
  if(!tab){
    return.result <- list(result.vergm.l, scale, ergm.formula)
    names(return.result) <- c("result.vergm.l","scale", "formula")
  }
  return(return.result)
}

##################################################################
install.packages(missForest, dependencies = TRUE)
library(missForest)
test.data <- all.bid.ON[,1:10]# %>% as.data.frame
test <- missForest(test.data, variablewise = T)
names(test)
test$ximp
dygraph(test$ximp) %>%
  dyOptions(colors = c("gray","black","black","black","red","black","black","black","black","black","black"))
names(test$ximp)



md.pattern(data)

##############################################2018年06月02日19:57:30
mdp10 <- matrix(rep(NA,(dim(mdp)[1]-1)*(dim(mdp)[2]-1)),dim(mdp)[1]-1,dim(mdp)[2]-1)
mdp10[mdp[-dim(mdp)[1],-dim(mdp)[2]]==as.integer(1)] <- FALSE
mdp10[mdp[-dim(mdp)[1],-dim(mdp)[2]]==as.integer(0)] <- TRUE
mdp10 <- matrix(mdp10,dim(mdp)[1]-1,dim(mdp)[2]-1)

is.na.data <- is.na(data)
nrow.na <- c()
for (i in 1:nrow(is.na.data)) {
  temp.na <- is.na.data[i,] == mdp10[dim(mdp10)[1],]
  if(sum(temp.na)==dim(mdp10)[2]){
    nrow.na <- c(nrow.na,i)
  }
}
x <- test[1,] ==y
x <- all.equal(test[1,],y)

###################################################################2018年06月03日
##################
file.name <- "yearly_vecm_gir"
result <- dyErgm.vecm.gir$result.vergm.l
################
stargazer(result, title = "results of yearly ERGMs",
          dep.var.labels = c("spillover of listed bank"),
          column.labels = c(2009:2016)%>%as.character,
          covariate.labels = c("cns",inclusion.edgecov,inclusion.nodecov),
          notes = "",
          digits = 2,
          label = paste0("tab:",file.name),
          out = paste0("latex/SHIBORbid/table/",file.name,".tex"),
          table.placement = "H", 
          #keep.stat=c("aic", "rsq", "n"),
          #float.env = "sidewaystable",#if sidewaystable then table.placement should be FALSE
          out.header = FALSE,
          no.space = TRUE,
          nobs = TRUE,
          model.numbers=FALSE,
          #style = "aer",#GULY
          type = "latex")#type




###
inclusion.edgecov <- c("loan")#c("ON","M1","Y1","loan")##"ON", "W1","W2","M1","M3","M6","M9","Y1"
inclusion.nodecov <- c("ass", "dbt", "sty") 
dyErgm.vecm.gir1 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, inclusion.edgecov = inclusion.edgecov, inclusion.nodecov = inclusion.nodecov, Date = y.period[-c(1,10,11)], is.higher = is.higher, wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE)
result <- dyErgm.vecm.gir1$result.vergm.l
file.name <- "yearly_vecm_gir1"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.edgecov,inclusion.nodecov), notes = "", digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

###
inclusion.edgecov <- c("loan")#c("ON","M1","Y1","loan")##"ON", "W1","W2","M1","M3","M6","M9","Y1"
rm(inclusion.nodecov)
dyErgm.vecm.gir2 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, inclusion.edgecov = inclusion.edgecov, 
                                      #inclusion.nodecov = inclusion.nodecov, 
                                      Date = y.period[-c(1,10,11)], is.higher = is.higher, wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE)
result <- dyErgm.vecm.gir2$result.vergm.l
file.name <- "yearly_vecm_gir2"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.edgecov), notes = "", digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

###
inclusion.edgecov <- c("ON", "W1","W2","M1","M3","M6","M9","Y1","loan")#c("ON","M1","Y1","loan")##"ON", "W1","W2","M1","M3","M6","M9","Y1"
rm(inclusion.nodecov)
dyErgm.vecm.gir3 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, inclusion.edgecov = inclusion.edgecov, 
                                      #inclusion.nodecov = inclusion.nodecov, 
                                      Date = y.period[-c(1,10,11)], is.higher = is.higher, wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE)
result <- dyErgm.vecm.gir3$result.vergm.l
file.name <- "yearly_vecm_gir3"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.edgecov), notes = "", digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

###
inclusion.edgecov <- c("ON", "W1","W2","M1","M3","M6","M9","Y1")#c("ON","M1","Y1","loan")##"ON", "W1","W2","M1","M3","M6","M9","Y1"
rm(inclusion.nodecov)
dyErgm.vecm.gir4 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, inclusion.edgecov = inclusion.edgecov, 
                                      #inclusion.nodecov = inclusion.nodecov, 
                                      Date = y.period[-c(1,10,11)], is.higher = is.higher, wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE)
result <- dyErgm.vecm.gir4$result.vergm.l
file.name <- "yearly_vecm_gir4"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.edgecov), notes = "", digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

###
rm(inclusion.edgecov) 
inclusion.nodecov <- c("ass", "dbt", "sty","lnd","brr") 
dyErgm.vecm.gir5 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, 
                                      #inclusion.edgecov = inclusion.edgecov, 
                                      inclusion.nodecov = inclusion.nodecov,
                                      Date = y.period[-c(1,10,11)], is.higher = is.higher, wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE)
result <- dyErgm.vecm.gir5$result.vergm.l
file.name <- "yearly_vecm_gir5"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.nodecov), notes = "", digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

##
inclusion.edgecov <- c("ON", "W1","W2","M1","M3","M6","M9","Y1","loan")#c("ON","M1","Y1","loan")##"ON", "W1","W2","M1","M3","M6","M9","Y1"
inclusion.nodecov <- c("ass", "dbt", "sty")#,"lnd","brr") 
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
dyErgm.vecm.gir6 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, 
                                      inclusion.edgecov = inclusion.edgecov,
                                      inclusion.nodecov = inclusion.nodecov,
                                      inclusion.nodeocov = inclusion.nodeocov,
                                      inclusion.nodeicov = inclusion.nodeicov,
                                      trans = trans,
                                      Date = y.period[-c(1,10,11)], is.higher = is.higher, 
                                      wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE,
                                      BidType = "aenet")
result <- dyErgm.vecm.gir6$result.vergm.l
file.name <- "yearly_vecm_gir6"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.edgecov,inclusion.nodecov), 
          notes = "",
          digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

##
inclusion.edgecov <- c("loan")#c("ON","M1","Y1","loan")##"ON", "W1","W2","M1","M3","M6","M9","Y1"
inclusion.nodecov <- c("ass", "dbt", "sty","lnd","brr") 
dyErgm.vecm.gir6 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:8], set = set, 
                                      inclusion.edgecov = inclusion.edgecov, 
                                      inclusion.nodecov = inclusion.nodecov,
                                      Date = y.period[-c(1,10,11)], is.higher = is.higher, wind = raw.wind[3:10], csv = FALSE, fig = FALSE, tab = TRUE)
result <- dyErgm.vecm.gir7$result.vergm.l
file.name <- "yearly_vecm_gir7"
stargazer(result, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), column.labels = c(2009:2016) %>% as.character, covariate.labels = c("cns",inclusion.edgecov,inclusion.nodecov), notes = "", digits = 2, label = paste0("tab:",file.name), out = paste0("latex/SHIBORbid/table/",file.name,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type



#Load yaerly data################################################################################








