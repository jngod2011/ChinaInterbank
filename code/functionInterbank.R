#################################################################
# dygraph.interbank
#################################################################
dygraph.interbank <- function(dy.data, dy.main, color, begin.dateWindow = NULL, end.dateWindow = NULL){
  if(missing(begin.dateWindow)){
    begin.dateWindow <- index(dy.data)[1]
  }
  if(missing(end.dateWindow)){
    end.dateWindow <- index(dy.data)[nrow(dy.data)]
  }
  dygraph(data = dy.data, main = dy.main) %>%
    dyRangeSelector(dateWindow = c(begin.dateWindow, end.dateWindow), height = 20)  %>%
    
    dyAxis("y", label = " ") %>%#valueRange = 
    dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", colors = color, strokeWidth = 2) %>%
    dyLegend(show = "auto", hideOnMouseOut = TRUE, width = 900, labelsSeparateLines = FALSE) %>%
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.7,
                hideOnMouseOut = TRUE,
                highlightSeriesOpts = list(strokeWidth = 2.5)) %>%
    dyEvent("2007-10-26", "2007.10.26:Liquidity Crisis     ", labelLoc = "top") %>%
    #dyEvent("2008-9-14", "2008.9.14:Lehman Brothers Went Bankrupt     ", labelLoc = "top", color = "gray") %>%
    #dyEvent("2008-11-25", "2008.11.25:QE1 of US     ", labelLoc = "top", color = "gray") %>%
    #dyEvent("2010-11-4", "2010.11.4:QE2 of US     ", labelLoc = "top", color = "gray") %>%
    dyEvent("2011-1-30", "2011.01.30:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2011-6-23", "2011.06.23:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2012-1-18", "2012.01.18:Liquidity Crisis     ", labelLoc = "top") %>%
    #dyEvent("2012-9-14", "2012.9.14:QE3 of US     ", labelLoc = "top", color = "gray") %>%
    #dyEvent("2012-12-13", "2012.12.13:QE4 of US     ", labelLoc = "top", color = "gray") %>%
    dyEvent("2013-6-7", "2013.06.07:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2013-12-23", "2013.12.23:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2014-06-24", "2014.06.24:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2014-12-23", "2014.12.23:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2017-06-20", "2017.06.20:Liquidity Crisis     ", labelLoc = "top") %>%
    dyEvent("2017-12-29", "2017.12.29:Liquidity Crisis     ", labelLoc = "top") %>%
    #dyEvent("2015-12-16", "2015.12.16:US Fed Raises Interest Rates     ", labelLoc = "top", color = "gray") %>%
    #dyEvent("2017-3-15", "2017.3.15:US Fed Raises Interest Rates     ", labelLoc = "top", color = "gray") %>%
    #dyShading(from = "2006-01-01", to = "2008-1-24") %>%
    #dyShading(from = "2010-11-08", to = "2011-3-4") %>%
    #dyShading(from = "2011-05-05", to = "2012-3-5") %>%
    #dyShading(from = "2013-06-05", to = "2014-5-27") %>%
    #dyShading(from = "2014-12-17", to = "2015-4-10") %>%
    #dyShading(from = "2017-01-10", to = "2017-07-07") %>%
    #dyShading(from = "2017-11-08", to = "2018-01-03") %>%
    dyLimit(0, color = "gray")
}
#################################################################
# fevd.var
#################################################################
fevd.var <- function(data, mode, span, n.ahead){

  matrix.list <- list()
  n <- nrow(data) - span
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in 1:n){
    if (mode == "iteration") {VAR <- VAR(data[1:(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
    if (mode == "fix") {VAR <- VAR(data[(1 + i):(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
    
    matrix.list[[i]] <- genFEVD(est = VAR, n.ahead = n.ahead, no.corr = F)
    
    setTxtProgressBar(pb, i)
  }
  toc()  
  close(pb)
  return(matrix.list)
}

#################################################################
# aenet.span
#################################################################
aenet.span <- function(data,start.point = "NULL", end.point = "NULL"){
  if(start.point == "NULL") {start.point <- data$Date[1]}
  #if(start.point < data$Date[1]) {start.point <- data$Date[1]}
  if(end.point == "NULL") {end.point <- data$Date[nrow(data)]}
  data <- data[data$Date >= start.point & data$Date < end.point,]
  
  data.aenet <- xts(data[,-1], as.Date(data$Date, format='%Y-%m-%d'))
  coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet)-1)
  m.coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet))
  
  for (i in 1:(ncol(data.aenet)-1)) {
    temp.x <- data.aenet[-1,-i] %>% as.matrix
    temp.y <- stats::lag(data.aenet[,i],1)[-1,]
    aenet.fit <- aenet(
      x = temp.x, y = temp.y,lower.limits = 0,family = "gaussian",
      alphas = 0.5, seed = 1,init = "enet",tune = "aic"#tune = "cv",rule = "lambda.min", nfolds = 10
    )
    coef.aenet[i,] <- coef(aenet.fit)
  }
  coef.aenet <- coef.aenet %>% as.data.frame
  m.coef.aenet[lower.tri(m.coef.aenet, diag = F)] <- coef.aenet[lower.tri(coef.aenet, diag = F)]
  m.coef.aenet[upper.tri(m.coef.aenet, diag = F)] <- coef.aenet[upper.tri(coef.aenet, diag = T)]
  m.coef.aenet <- m.coef.aenet %>% as.data.frame()
  names(m.coef.aenet) <- names(data.aenet)
  rownames(m.coef.aenet) <- names(data.aenet)
  m.coef.aenet[m.coef.aenet == 0] <- NA
  m.coef.aenet <- as.matrix(m.coef.aenet)
  year.aenet <- substr(data$Date[1],1,4)
  return.result <- list(m.coef.aenet,year.aenet)
  names(return.result) <- c("m.coef.aenet","year.aenet")
  
  return(return.result)
}

#################################################################
# aenet.fix
#################################################################
aenet.fix <- function(data, start.point = "NULL", end.point = "NULL", fix){
  tic()
  n <- nrow(data)
  pb <- txtProgressBar(min = 0, max = ifelse(fix == "yearly", 1, n-fix), 
                       style = 3)
  if(class(data)[[1]]=="data.frame"){
    if(start.point == "NULL") {start.point <- data$Date[1]}
    #if(start.point < data$Date[1]) {start.point <- data$Date[1]}
    if(end.point == "NULL") {end.point <- data$Date[nrow(data)]}
    data <- data[data$Date >= start.point & data$Date <= end.point,]
    class.data <- "data.frame"
  }
  
  if(class(data)[[1]]=="xts"){
    if(start.point == "NULL") {start.point <- index(data)[1] %>% as.character}
    #if(start.point < data$Date[1]) {start.point <- data$Date[1]}
    if(end.point == "NULL") {end.point <- index(data)[nrow(data)] %>% as.character}
    data <- data[paste0(start.point,"/",end.point)]
    class.data <- "xts"
  }
  
  list.aenet <- list()
  list.Date <- list()
  
  if(fix == "yearly"){
    if(class.data == "data.frame"){data.aenet <- xts(data, as.Date(data$Date, format='%Y-%m-%d'))}
    if(class.data == "xts"){data.aenet <- data}
    list.Date[[1]] <- index(data.aenet)
    
    coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet)-1)
    m.coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet))
    for (i in 1:(ncol(data.aenet)-1)) {
      temp.x <- data.aenet[-1,-i] %>% as.matrix
      temp.y <- stats::lag(data.aenet[,i],1)[-1,]
      require(forecast)
      if(is.constant(temp.y)){temp.y <- temp.y+rnorm(fix, mean = 0, sd = 0.000001)}
      aenet.fit <- aenet(
        x = temp.x, y = temp.y,lower.limits = 0,family = "gaussian",
        alphas = 0.5, seed = 1,init = "enet",tune = "aic"#tune = "cv",rule = "lambda.min", nfolds = 10
      )
      coef.aenet[i,] <- coef(aenet.fit)
    }
    coef.aenet <- coef.aenet %>% as.data.frame
    m.coef.aenet[lower.tri(m.coef.aenet, diag = F)] <- coef.aenet[lower.tri(coef.aenet, diag = F)]
    m.coef.aenet[upper.tri(m.coef.aenet, diag = F)] <- coef.aenet[upper.tri(coef.aenet, diag = T)]
    m.coef.aenet <- m.coef.aenet %>% as.data.frame()
    names(m.coef.aenet) <- names(data.aenet)
    rownames(m.coef.aenet) <- names(data.aenet)
    m.coef.aenet[m.coef.aenet == 0] <- NA 
    list.aenet[[1]] <- m.coef.aenet
    
    setTxtProgressBar(pb, t)
  }else{
    for (t in 1:(dim(data)[1]-fix)) {
      if(class.data == "data.frame"){data.aenet <- xts(data[t:(t+fix),-1], as.Date(data$Date[t:(t+fix)], format='%Y-%m-%d'))}
      if(class.data == "xts"){data.aenet <- data[t:(t+fix)]}
      list.Date[[t]] <- index(data.aenet[fix+1,])
      
      coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet)-1)
      m.coef.aenet <- matrix(NA, ncol(data.aenet), ncol(data.aenet))
      for (i in 1:(ncol(data.aenet)-1)) {
        temp.x <- data.aenet[-1,-i] %>% as.matrix
        temp.y <- stats::lag(data.aenet[,i],1)[-1,]
        require(forecast)
        if(is.constant(temp.y)){temp.y <- temp.y+rnorm(fix, mean = 0, sd = 0.000001)}
        aenet.fit <- aenet(
          x = temp.x, y = temp.y,lower.limits = 0,family = "gaussian",
          alphas = 0.5, seed = 1,init = "enet",tune = "aic"#tune = "cv",rule = "lambda.min", nfolds = 10
        )
        coef.aenet[i,] <- coef(aenet.fit)
      }
      coef.aenet <- coef.aenet %>% as.data.frame
      m.coef.aenet[lower.tri(m.coef.aenet, diag = F)] <- coef.aenet[lower.tri(coef.aenet, diag = F)]
      m.coef.aenet[upper.tri(m.coef.aenet, diag = F)] <- coef.aenet[upper.tri(coef.aenet, diag = T)]
      m.coef.aenet <- m.coef.aenet %>% as.data.frame()
      names(m.coef.aenet) <- names(data.aenet)
      rownames(m.coef.aenet) <- names(data.aenet)
      m.coef.aenet[m.coef.aenet == 0] <- NA 
      list.aenet[[t]] <- m.coef.aenet
      
      setTxtProgressBar(pb, t)
    }
  }
  
  
  return.result <- list(list.aenet, list.Date)
  names(return.result) <- c("Coef", "Date")

  toc()  
  close(pb)
  
  return(return.result)
}

#################################################################
# f.na.approx
#################################################################
f.na.approx <- function(x){
  z <- is.na(x) %>% as.numeric
  if(is.na(x[1])==T){x[1] <- x[which(z==0)[1]]}
  if(is.na(x[length(x)])==T){x[length(x)] <- rev(x)[which(rev(z)==0)[1]]}
  y <- na.approx(x, na.rm = FALSE)
  return(y)
}
#na.approx(x) #对缺失值进行线性插值
#na.spline(x) #对缺失值进行样条插值
#na.locf(x) #末次观测值结转法
###############################################################
# forceNetwork.anent
###############################################################
forceNetwork.anent <- function(data, aenet.matrix, maturity, file.path, is.higher, group.bid, crisis){
  is.higher.aenet <- merge(data, is.higher, by= "Date", all.x = T)[,c("Date","sum",crisis)]
  is.higher.aenet[,crisis] <- cut(as.numeric(is.higher.aenet[,crisis]),breaks = 5, include.lowest=F, labels = c(1:5))
  if(sum(is.na(is.higher.aenet)) > 0){
    is.higher.aenet[,-1] <- apply(is.higher.aenet[,-1], 2, f.na.approx)
  }
  n <- length(aenet.matrix$Coef)
  for (t in seq(1941, n, 10)) {
    temp <- aenet.matrix$Coef[[t]]
    temp.date <- aenet.matrix$Date[[t]]
    temp <- temp[1:15,1:15]
    temp <- temp[group.bid$Cname, group.bid$Cname] %>% t
    spillover <- !is.na(temp)
    temp[is.na(temp)] <- 0
    igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
    networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
    networkD3.matrix[[2]]$group <- group.bid[,5]
    networkD3.matrix[[2]]$name <- group.bid[,2] # group[1,]
    networkD3.matrix[[2]]$size <- rowSums(spillover) # rowSums(temp)
    networkD3.matrix[[1]]$value[networkD3.matrix[[1]]$value < 0.2] <- 0.2
    networkD3.matrix[[2]]$size[networkD3.matrix[[2]]$size < 2] <- 2 # & networkD3.matrix[[2]]$size > 0
    networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size
    networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu * 3
    
    group.crisis=c(" ","  ","   ","    ","     ")
    index.crisis <- is.higher.aenet[,crisis][is.higher.aenet$Date == temp.date] %>% as.numeric %>% ceiling
    group.crisis[index.crisis] <- temp.date %>% as.character
    matrix.date <- data.frame(name=c("","","","","",""),
                              c(maturity, group.crisis),
                              size=c(0,0,0,0,0,0)) %>% as.data.frame
    names(matrix.date) <-c("name","group","size")
    networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)
    
    ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","#3F51B5","white","#CCCCCC","#999999", "#666666","#333333","#000000"]);'
    #colorRampPalette(c("black", "white"))(6)[-6]
    rbokeh.bid <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                               Source = "source", Target = "target", Value = "value", 
                               arrows = T, linkDistance = JS("function(d){return 1 / d.value * 300 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                               NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                               Group = "group", legend = T, fontSize = 15, opacity = 0.8, opacityNoHover = 1,
                               height = 700 , width = 700, bounded = F, zoom = T, charge = -100)
    
    widget2png(rbokeh.bid, file = paste0(file.path, t, ".png"), timeout = 2000)
    
    cat("\r", round(t/n,2))
    flush.console()
  }
}
###############################################################
#forceNetwork.anent.yearly
###############################################################
forceNetwork.anent.yearly <- function(aenet.matrix, maturity, file.path, group.bid){
  n <- length(aenet.matrix)
  for (t in 1:n) {
    temp <- aenet.matrix[[t]][[1]] %>% as.matrix %>% t
    temp <- temp[1:15,1:15]
    spillover <- !is.na(temp)
    temp[is.na(temp)] <- 0
    igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
    networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
    networkD3.matrix[[2]]$group <- group.bid[,5]
    networkD3.matrix[[2]]$name <- group.bid[,2]#group[1,]
    #networkD3.matrix[[2]]$size <- rowSums(spillover)#rowSums(temp)
    networkD3.matrix[[1]]$value[networkD3.matrix[[1]]$value < 0.2] <- 0.2
    networkD3.matrix[[2]]$size[networkD3.matrix[[2]]$size < 2] <- 2 # & networkD3.matrix[[2]]$size > 0
    networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size
    networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu * 3
   
    matrix.date <- data.frame(name=c("",""),
                              c(maturity,aenet.matrix[[t]][[2]]),
                              size=c(0,0)) %>% as.data.frame
    names(matrix.date) <-c("name","group","size")
    networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]],matrix.date)
    ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","#3F51B5","white","white"]);'
    #colorRampPalette(c("black", "white"))(6)[-6]
    rbokeh.bid <- forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                               Source = "source", Target = "target", Value = "value", 
                               arrows = T, linkDistance = JS("function(d){return 1 / d.value * 200 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                               NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                               Group = "group", legend = T, fontSize = 15, opacity = 0.8, opacityNoHover = 1,
                               height = 700 , width = 700, bounded = F, zoom = T, charge = -100)
    
    widget2png(rbokeh.bid, file = paste0(file.path, t, ".png"), timeout = 2000)
    
    cat("\r", round(t/n,2))
    flush.console()
  }
}
###############################################################
# dygraph.anent
###############################################################
dygraph.anent <- function(data, is.higher, dy.main, group.bid){
  ts.data <- c()
  temp.Date <- c()
  for (t in 1:length(data$Coef)) {
    temp.data <- data$Coef[[t]][1:15,1:15] %>% as.matrix
    temp.data <- .colSums(temp.data, m=15, n=15, na.rm = T)
    ts.data <- rbind(ts.data, temp.data)
    temp.Date <- c(temp.Date, as.character(data$Date[[t]]))
  }
  ts.data <- xts(ts.data, as.Date(temp.Date , format='%Y-%m-%d'))
  ts.data <- merge(ts.data, is.higher$crisis.sma20, join='left')
  
  names(ts.data) <-c(group.bid$Abre,"Crisis") 
  
  dy.main <- dy.main
  color <- c(colorRampPalette(c("#B71C1C", "white"))(6)[-6], colorRampPalette(c("#FFC107", "white"))(9)[-9], "#33691E","#3F51B5")    
  
  dygraph.interbank(ts.data, dy.main, color) %>% 
    dyAxis("y", label = "Shanghai Interbank Offered Rate", independentTicks = T) %>%
    dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
    dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 1,axis = "y2")
}

#rolling test
dy.genFEVD <- function(data, mode, span, n.ahead){
  n <- nrow(data) - span
  tic()
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  result <- list()
  for (i in 1:n){
    if (mode == "iteration") {VAR <- VAR(data[1:(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
    if (mode == "fix") {VAR <- VAR(data[(1 + i):(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
    result[[i]] <- genFEVD(est = VAR, n.ahead = n.ahead, no.corr = F)
    
    setTxtProgressBar(pb, i)
  }
  toc()  
  close(pb)
  return(result)
}
#################################################################
#replaceNA0
#################################################################
replaceNA0 <- function(x){
  x[is.na(x)] <- 0
  return(x)
}
#################################################################
# replace.na.near
#################################################################
replace.na.near <- function(x){
  for (i in 2:length(x)) {
    if(is.na(x[i])){
      x[i] <- x[i-1]
    }
  }
  x <- x[length(x):1]
  for (i in 2:length(x)) {
  if(is.na(x[i])){
    x[i] <- x[i-1]
  }
  }
  x <- x[length(x):1]
  return(x)
}
replace.na.near(c(NA,NA,1,1))
#################################################################
#aenet.dygraph
#################################################################
aenet.dygraph <- function(x,title){
  index <- c()
  for (i in 1:length(x)) {
    temp <- x[[i]][1:10,1:10] 
    temp[is.na(temp)] <- 0
    temp <- t(temp)
    temp <- rowSums(temp)
    index <- rbind(index,temp)
  }
  dy.data <- xts(index, as.Date(Date[-c(1:250)], format='%Y-%m-%d'))
  dy.data <- merge(dy.data, is.higher$crisis.sma20) %>% na.omit
  colnames(dy.data) <- c(bank10.abbr,"Crisis")
  dy.main <- c("Funding Cost", title)
  color <- c("#C54949","#D37676","#E2A4A4","#F0D1D1","#FFA31C","#FFAE38","#FFC571","#FFD18D","#FFF3E2", "#BBCDB3", "gray")#c(group.stockprice[bank10.abbr,"color"],"gray")
  dygraph.interbank(dy.data, dy.main, color) %>% 
    dyAxis("y", label = "Spillover Index", independentTicks = T) %>%
    dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
    dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
}

