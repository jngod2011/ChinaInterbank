#################################################################
# function that delete NAs columns
#################################################################
deleteNAcols <- function(x){
  y <- c()
  for (i in 1:NCOL(x)){
    if (sum(is.na(x[, i])) == nrow(x)){y <- c(y, i)}
  }
  x <- x[, -y]
  return(x)
}
#################################################################
# Interpolation too slow
#################################################################
Interpolation <- function(x)
{
  
  y <- c()#给最终输出值分配内存空间
  z <- as.numeric(is.na(x))#建立中间变量z
  MaxContinuousOnes <- sum(z)#最大可能有多少个NA
  #如果第一个数就是NA，无法在匀速增长的假设下估计缺失值，就假设第一个数及接连其后的未知数等于最近的已知数据 
  count <- 0
  if(z[1] == 1){for (j in 0:MaxContinuousOnes) {ifelse(z[1+j] == 1, count <- count+1, break)}}
  y[1:count] <- x[count+1]
  
  #如果第一个数是NA，那么第count个数是最后一个连续的NA，第count+1个数应该是已知数，可以从第count+2个数开始判断是否为NA。
  #但需要为第count+1个数开始赋值
  #如果第一个数不是NA那么count为0，从第二个数开始判断即可。
  for(i in count+1:length(x)){
    count <- 0
    for (j in 0:MaxContinuousOnes) {ifelse(z[i+j] == 1, count <- count+1, break)}
    #从第i个数本身开始循环，如果第一个数不是NA则跳出循环，如果第i个数是NA则开始计算包括第i个数在内一共连续有多少个NA
    ifelse(z[i] == 1, 
           y[i] <- y[i-1] + (x[i+count]-y[i-1])/(count+1), 
           y[i] <- x[i])
  }   
  
  x <- rev(y)
  z <- as.numeric(is.na(x))
  count <- 0
  if(z[1] == 1){for (j in 0:MaxContinuousOnes) {ifelse(z[1+j] == 1, count <- count+1, break)}}
  x[1:count] <- x[count+1]
  y <- rev(x)
  
  return(y)
}

col_apply <- function(x = data.frame()){
  y <- x
  ncol.x <- NCOL(x)
  for(i in 1:ncol.x){
    y[, i] <- Interpolation(x[, i])  
  }
  return(y)
}
#################################################################
# names of file
#################################################################
last_list <- function(x = list()){
  n <- length(x)
  y <- c()
  for(i in 1:n){
    y <- c(y, x[[i]][NROW(x[[i]])])
  }
  y <- as.character(y)
  return(y)
}

#################################################################
# multijion together
#################################################################
multijion <- function(data = list(), ...){
  nrow_list <- numeric()
  for(i in 1:length(data)){
    nrow_list[i] <- nrow(data[[i]])
  }
  jiondata <- data[[which.max(nrow_list)]]
  data[[1]] <- NULL
  for(i in data){
    jiondata <- full_join(jiondata, i, ...)#There should not have NAs.
  }
  return(jiondata)
}

#################################################################
## Generalised impulse response 
#################################################################
irf_generalised <- function(model, n.ahead) 
{
  if (is.na(match(class(model),c("varest","vec2var")))) {
    return("The model class is not varest and vec2var!")
  }
  A <- Phi(model, n.ahead)
  epsilon <- residuals(model)
  Sigma <- t(epsilon)%*%epsilon / (model$obs - model$p*model$K)
  gi <- array(0, dim(A))
  sigmas <- sqrt(diag(Sigma))
  for (j in 1:dim(A)[3]) {
    gi[,,j] <- t( t( A[,,j]%*%Sigma ) / sigmas )
  }
  return(gi)
}

#################################################################
##weighted_gir
#################################################################
weighted_gir <- function(GIR, divided) 
{
  if (divided == 1) {weighted.matrix <- t(t((3*GIR[,,1]+2*GIR[,,2]+GIR[,,3]+GIR[,,4]+GIR[,,5]+GIR[,,6])/3)/diag(GIR[,,1])) %>% matrix(nrow = dim(GIR)[1], ncol = dim(GIR)[1])}
  if (divided == 0) {weighted.matrix <- (3*GIR[,,1]+2*GIR[,,2]+GIR[,,3]+GIR[,,4]+GIR[,,5]+GIR[,,6])/3 %>% matrix(nrow = dim(GIR)[1], ncol = dim(GIR)[1])}
  diag(weighted.matrix) <- 0
  to <- colSums(weighted.matrix)
  from <- rowSums(weighted.matrix)
  net_average <- (to - from)/(dim(GIR)[1]-1)
  to_average <- to/(dim(GIR)[1]-1)
  from_average <- from/(dim(GIR)[1]-1)
  return.result <- list(net_average, to_average, from_average, weighted.matrix)
  names(return.result) <- c("net_average", "to_average", "from_average", "weighted.matrix")
  return(return.result)
}

#################################################################
##index.spill
#################################################################
index.spill <- function(data, mode, span, divided){
  if(missing(span)){span <- 0}
  tic()
  n <- nrow(data) - span
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  weighted.matrix <- list()
  if(mode == "yearly"){
    index.all <- as.data.frame(matrix(NA, nrow = 1, ncol = NCOL(data)))
    VAR <- VAR(data, p = 2, type = c("none"), season = NULL, exogen = NULL)
    GIR <- irf_generalised(VAR, n.ahead = 5)
    weighted.gir <- weighted_gir(GIR, divided)
    index.all[1, ] <- weighted.gir[[1]] %>% t %>% as.data.frame
    weighted.matrix <- weighted.gir[[4]] %>% as.matrix
    setTxtProgressBar(pb, n)
  }
  index.all <- as.data.frame(matrix(NA, nrow = NROW(data), ncol = NCOL(data)))
  if(mode == "iteration" | mode == "fix"){
    for (i in 1:n){
      if (mode == "iteration") {VAR <- VAR(data[1:(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
      if (mode == "fix") {VAR <- VAR(data[(1 + i):(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
      GIR <- irf_generalised(VAR, n.ahead = 5)
      weighted.gir <- weighted_gir(GIR, divided)
      index.all[i + span, ] <- weighted.gir[[1]] %>% t %>% as.data.frame
      weighted.matrix[[i]] <- weighted.gir[[4]] %>% as.matrix
      setTxtProgressBar(pb, i)
    }
  }
  
  toc()  
  close(pb)
  result <- list(index.all, weighted.matrix)
  names(result) <- c("index.all", "weighted.matrix")
  return(result)
}
#################################################################
##rearrange_matrix.list
#################################################################
rearrange_matrix.list <- function(x = list(), country = numeric()){
  y.to <- matrix(NA, nrow = length(x), ncol =  NCOL(x[[1]])) %>% as.data.frame
  y.from <- y.to
  y.net <- y.to
  for(i in 1:length(x)){
    y.to[i, ] <- x[[i]][, country]
    y.from[i, ] <- x[[i]][country, ]
  }
  y.net <- y.to - y.from
  return(list(y.net, y.to, y.from))
}
#################################################################
##seasonallyadjusted
#################################################################
seasonallyadjusted <- function(x){
  y <- data.frame(matrix(NA, 144, 16))
  for(i in 1:16){
    ts.x <- ts(x[, i], frequency = 12, start = c(2005, 1))
    de.x <- stats::decompose(ts.x)
    ad.x <- ts.x - de.x$seasonal
    y[, i] <- ad.x
  }
  return(y)
}

#################################################################
##fevd_generalised
#################################################################
fevd_generalised <- function(model, n.ahead, normalize=TRUE) {
  if (is.na(match(class(model),c("varest","vec2var")))) {
    return("The model class is not varest and vec2var!")
  }
  A <- Phi(model, n.ahead)
  epsilon <- residuals(model)
  Sigma <- t(epsilon)%*%epsilon / (model$obs - model$p*model$K)
  gi <- array(0, dim(A))
  sigmas <- sqrt(diag(Sigma))
  for (j in 1:dim(A)[3]) {
    gi[,,j] <- t( t( A[,,j]%*%Sigma ) / sqrt(sigmas) )
  }
  d <- array(0, dim(A)[c(2,3)])
  for (j in 1:dim(d)[2]) {
    d[,j] <- diag(A[,,j]%*%Sigma%*%t(A[,,j]))
  }
  
  num <- apply(gi^2,1:2,sum)
  den <- c(apply(d,1,sum))
  fevd <- num/den
  if (normalize) {
    return(fevd/apply(fevd, 1, sum))
  } else {
    return(fevd)
  }
}
#################################################################
#GrowthRate
#################################################################
log_GrowthRate <- function(x)
{
  y <- c()
  y <- log(x[-1])-log(x[-length(x)])
}
#################################################################
#dy.VAR.FEVD
#################################################################
dy.VAR.FEVD <- function(data, n.ahead, mode, span){
  tic()
  n <- nrow(data)
  pb <- txtProgressBar(min = 0, max = ifelse(span == "yearly", 1, n-span), 
                       style = 3)
  fevd.matrix <- list()
  data <- data %>% na.omit
  
  if(class(data)[[1]] == "data.frame" & !is.na(match("Date",names(data)))){
    Date <- data$Date[-c(1:span)]
    data <- subset(data, select = -Date)
  }
  if(class(data)[[1]] == "xts"){Date <- index(data)}
  
  if(span == "yearly"){
    VAR <- VAR(data, p = 2, type = c("none"), season = NULL, exogen = NULL)
    FEVD <- fevd_generalised(VAR, n.ahead=n.ahead, normalize=TRUE)
    fevd.matrix[[1]] <- FEVD %>% as.matrix
    setTxtProgressBar(pb, 1)
  }else{
    for (i in 1:(n-span)){
      if (mode == "iteration") {VAR <- VAR(data[1:(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
      if (mode == "fix") {VAR <- VAR(data[(1 + i):(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
      FEVD <- fevd_generalised(VAR, n.ahead=n.ahead, normalize=TRUE)
      fevd.matrix[[i]] <- FEVD %>% as.matrix
      setTxtProgressBar(pb, i)
    }
  }
  
  toc()  
  close(pb)
  result <- list(fevd.matrix, Date)
  names(result) <- c("fevd.matrix", "Date")
  return(result)
}
#################################################################
#dy.VAR.GIR
#################################################################
dy.VAR.GIR <- function(data, n.ahead, mode, span){ 
  tic()
  n.ahead <- n.ahead + 1
  n <- nrow(data)
  pb <- txtProgressBar(min = 0, max = ifelse(span == "yearly", 1, n-span), 
                       style = 3)
  gir.matrix <- list()
  data <- data %>% na.omit
  
  if(class(data)[[1]] == "data.frame" & !is.na(match("Date",names(data)))){
    Date <- data$Date[-c(1:span)]
    data <- subset(data, select = -Date)
  }
  if(class(data)[[1]] == "xts"){Date <- index(data)}
  
  if(span == "yearly"){
    VAR <- VAR(data, p = 2, type = c("none"), season = NULL, exogen = NULL)
    GIR <- irf_generalised(VAR, n.ahead = n.ahead)
    gir.matrix[[1]] <- GIR[,,n.ahead] %>% as.matrix
    setTxtProgressBar(pb, 1)
  }else{
    for (i in 1:(n-span)){
      if (mode == "iteration") {VAR <- VAR(data[1:(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
      if (mode == "fix") {VAR <- VAR(data[(1 + i):(span + i),], p = 2, type = c("none"), season = NULL, exogen = NULL)}
      GIR <- irf_generalised(VAR, n.ahead = n.ahead)
      gir.matrix[[i]] <- GIR[,,n.ahead] %>% as.matrix
      setTxtProgressBar(pb, i)
    }
  }
  toc()  
  close(pb)
  result <- list(gir.matrix, Date)
  names(result) <- c("gir.matrix", "Date")
  return(result)
}
#################################################################
#dyCoefErgm daily
#################################################################
dyCoefErgm <- function(data, set, 
                       inclusion.edgecov = NULL,#NULL or variable names
                       inclusion.nodecov = NULL,#NULL or variable names
                       inclusion.nodeocov = NULL,#NULL or variable names
                       inclusion.nodeicov = NULL,#NULL or variable names
                       inclusion.absdiff = NULL,#NULL or variable names
                       inclusion.cyclicalweights = NULL,#NULL or TRUE
                       inclusion.mutual = NULL,#NULL or TRUE
                       inclusion.nodecovar = NULL,#NULL or TRUE
                       inclusion.nodeicovar = NULL,#NULL or TRUE
                       inclusion.nodeocovar = NULL,#NULL or TRUE
                       inclusion.nodeisqrtcovar = NULL,#NULL or TRUE
                       inclusion.nodeosqrtcovar = NULL,#NULL or TRUE
                       inclusion.nodesqrtcovar = NULL,#NULL or TRUE
                       Date,
                       trans = NULL, 
                       BidType, 
                       MCMLE.maxit = MCMLE.maxit, 
                       triangle,
                       directed)
{
  if(missing(trans)){trans <- NULL} # NULL c("y")
  if(missing(BidType)){BidType <- "aenet"} # c("aenet","vecm.gir","vecm.fevd","var.gir","var.fevd")
  if(missing(MCMLE.maxit)){MCMLE.maxit <- 100}
  if(missing(triangle)){triangle <- "all"} # c("lower","upper")
  if(missing(directed)){directed <- TRUE} # TRUE FALSE
  
  total <- length(data)-1
  p.ergm.l <- c()
  max.l <- c()
  
  min.max <- lapply(data,abs)
  min.max <- min(lapply(min.max, max) %>% unlist)
  scale <- ifelse(is.infinite(floor(log(1/min.max,10))),0,floor(log(1/min.max,10))) + 2
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  # formula
  ergm.formula <- "temp.y~sum(pow=1)"
  inclusion <- c()
  n.inclusion <- 0
  
  if(!missing(inclusion.edgecov)){
    if(!is.null(inclusion.edgecov)){
      n.inclusion.edgecov <- length(inclusion.edgecov)
      for (i in 1:n.inclusion.edgecov) {
        temp.formula <- paste0("+edgecov(temp.",inclusion.edgecov[i],",form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, inclusion.edgecov)
      n.inclusion <-  n.inclusion + n.inclusion.edgecov
    }
  }
  
  if(!missing(inclusion.nodecov)){
    if(!is.null(inclusion.nodecov)){
      n.inclusion.nodecov <- length(inclusion.nodecov)
      for (i in 1:n.inclusion.nodecov) {
        temp.formula <- paste0("+nodecov('",inclusion.nodecov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, inclusion.nodecov)
      n.inclusion <-  n.inclusion + n.inclusion.nodecov
    }
  }
  
  if(!missing(inclusion.nodeocov)){
    if(!is.null(inclusion.nodeocov)){
      n.inclusion.nodeocov <- length(inclusion.nodeocov)
      for (i in 1:n.inclusion.nodeocov) {
        temp.formula <- paste0("+nodeocov('",inclusion.nodeocov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, paste0("O.",inclusion.nodeocov))
      n.inclusion <-  n.inclusion + n.inclusion.nodeocov
    }
  }
  
  if(!missing(inclusion.nodeicov)){
    if(!is.null(inclusion.nodeicov)){
      n.inclusion.nodeicov <- length(inclusion.nodeicov)
      for (i in 1:n.inclusion.nodeicov) {
        temp.formula <- paste0("+nodeicov('",inclusion.nodeicov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, paste0("I.",inclusion.nodeicov))
      n.inclusion <-  n.inclusion + n.inclusion.nodeicov
    }
  }
  
  if(!missing(inclusion.absdiff)){
    if(!is.null(inclusion.absdiff)){
      n.inclusion.absdiff <- length(inclusion.absdiff)
      for (i in 1:n.inclusion.absdiff) {
        temp.formula <- paste0("+absdiff('",inclusion.absdiff[i],"',pow=1 ,form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, paste0("AD.",inclusion.absdiff))
      n.inclusion <-  n.inclusion + n.inclusion.absdiff
    }  
  }
  
  if(!missing(inclusion.cyclicalweights)){
    if(!is.null(inclusion.cyclicalweights) & inclusion.cyclicalweights == TRUE){
      ergm.formula <- paste0(ergm.formula,"+cyclicalweights(twopath='min',combine='max',affect='min')")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "cyclicalweights")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.mutual)){
    if(!is.null(inclusion.mutual) & inclusion.mutual == TRUE){
      ergm.formula <- paste0(ergm.formula,"+mutual(form='min',threshold=0)")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "mutual")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodecovar)){
    if(!is.null(inclusion.nodecovar) & inclusion.nodecovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodecovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodecovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeicovar)){
    if(!is.null(inclusion.nodeicovar) & inclusion.nodeicovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeicovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeicovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeocovar)){
    if(!is.null(inclusion.nodeocovar) & inclusion.nodeocovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeocovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeocovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeisqrtcovar)){
    if(!is.null(inclusion.nodeisqrtcovar) & inclusion.nodeisqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeisqrtcovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeisqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeosqrtcovar)){
    if(!is.null(inclusion.nodeosqrtcovar) & inclusion.nodeosqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeosqrtcovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeosqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodesqrtcovar)){
    if(!is.null(inclusion.nodesqrtcovar) & inclusion.nodesqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodesqrtcovar(center=TRUE)")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodesqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  ergm.formula <- as.formula(ergm.formula)
  
  n.inclusion <- length(inclusion)
  coef.ergm.l <- data.frame(matrix(NA,total,n.inclusion));names(coef.ergm.l) <- paste0("coef.",inclusion) 
  p.ergm.l <- data.frame(matrix(NA,total,n.inclusion));names(p.ergm.l) <- paste0("p.",inclusion) 
  names(p.ergm.l) <- paste0("p.",inclusion) 
  
  for (i in 1:total) {
    print(paste0("##########################",i,"##########################"))
    
    temp.y <- data[[i+1]] * (10^scale)
    nnode <- dim(temp.y)[1]
    temp.y <- apply(temp.y, c(1,2), round)
    temp.y <- apply(temp.y, c(1,2), abs)
    
    diag(temp.y) <- 0
    max.temp.y <- ifelse(max(abs(temp.y)) != 0, max(abs(temp.y)), return("all elements are zeros"))
    temp.y <- network(temp.y,
                      directed = directed,
                      loops = FALSE,# self loop
                      matrix.type = "adjacency",
                      ignore.eval=FALSE,
                      names.eval="weight")
    p <- sum(abs(temp.y %e% "weight"))/max.temp.y/network.dyadcount(temp.y)
    init <- ifelse(p/(1-p) >0 , log(p/(1-p)), 0)
    #if(!is.na(match("ON",inclusion))){}
    if(!missing(inclusion) & !is.null(inclusion)){
      if(BidType == "aenet"){
        if(!is.na(match("ON",inclusion))){
          temp.ON <- aenet.myl.ON[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion))){
          temp.W1 <- aenet.myl.W1[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion))){
          temp.W2 <- aenet.myl.W2[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion))){
          temp.M1 <- aenet.myl.M1[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion))){
          temp.M3 <- aenet.myl.M3[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion))){
          temp.M6 <- aenet.myl.M6[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion))){
          temp.M9 <- aenet.myl.M9[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion))){
          temp.Y1 <- aenet.myl.Y1[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
        if(!is.na(match("loan",inclusion))){
          temp.loan <- loan.network[[i]] %>% log  ;#temp.loan <- apply(temp.loan, c(1,2), round);
          diag(temp.loan) <- 0;temp.loan[is.na(temp.loan)] <- 0
          temp.loan <- as.matrix(temp.loan,nnode,nnode)
          if(!is.na(match("loan",trans))){temp.loan <- t(temp.loan)}
        }
      }
      if(BidType == "vecm.gir"){
        
        if(!is.na(match("ON",inclusion))){
          temp.ON <- vecm.gir.myl.ON[[i]] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0; temp.ON <- abs(temp.ON)
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion))){
          temp.W1 <- vecm.gir.myl.W1[[i]] * (10^scale)  ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0; temp.W1 <- abs(temp.W1)
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion))){
          temp.W2 <- vecm.gir.myl.W2[[i]] * (10^scale)  ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0; temp.W2 <- abs(temp.W2)
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion))){
          temp.M1 <- vecm.gir.myl.M1[[i]] * (10^scale)  ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0; temp.M1 <- abs(temp.M1)
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion))){
          temp.M3 <- vecm.gir.myl.M3[[i]] * (10^scale)  ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0; temp.M3 <- abs(temp.M3)
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion))){
          temp.M6 <- vecm.gir.myl.M6[[i]] * (10^scale)  ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0; temp.M6 <- abs(temp.M6)
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion))){
          temp.M9 <- vecm.gir.myl.M9[[i]] * (10^scale)  ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0; temp.M9 <- abs(temp.M9)
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion))){
          temp.Y1 <- vecm.gir.myl.Y1[[i]] * (10^scale)  ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0; temp.Y1 <- abs(temp.Y1)
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
        if(!is.na(match("loan",inclusion))){
          temp.loan <- loan.network[[i]] %>% log  ;#temp.loan <- apply(temp.loan, c(1,2), round);
          diag(temp.loan) <- 0;temp.loan[is.na(temp.loan)] <- 0
          temp.loan <- as.matrix(temp.loan,nnode,nnode)
          if(!is.na(match("loan",trans))){temp.loan <- t(temp.loan)}
        }
      }
      if(BidType == "vecm.fevd"){
        
        if(!is.na(match("ON",inclusion))){
          temp.ON <- vecm.fevd.myl.ON[[i]] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion))){
          temp.W1 <- vecm.fevd.myl.W1[[i]] * (10^scale)  ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion))){
          temp.W2 <- vecm.fevd.myl.W2[[i]] * (10^scale) ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion))){
          temp.M1 <- vecm.fevd.myl.M1[[i]] * (10^scale)  ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion))){
          temp.M3 <- vecm.fevd.myl.M3[[i]] * (10^scale)  ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion))){
          temp.M6 <- vecm.fevd.myl.M6[[i]] * (10^scale)  ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion))){
          temp.M9 <- vecm.fevd.myl.M9[[i]] * (10^scale)  ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion))){
          temp.Y1 <- vecm.fevd.myl.Y1[[i]] * (10^scale)  ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
        if(!is.na(match("loan",inclusion))){
          temp.loan <- loan.network[[i]] %>% log  ;#temp.loan <- apply(temp.loan, c(1,2), round);
          diag(temp.loan) <- 0;temp.loan[is.na(temp.loan)] <- 0
          temp.loan <- as.matrix(temp.loan,nnode,nnode)
          if(!is.na(match("loan",trans))){temp.loan <- t(temp.loan)}
        }
      }
      if(BidType == "var.gir"){
        
        if(!is.na(match("ON",inclusion))){
          temp.ON <- var.gir.myl.ON[[i]] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0;#temp.ON <- abs(temp.ON)
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion))){
          temp.W1 <- var.gir.myl.W1[[i]] * (10^scale) ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;#temp.W1[is.na(temp.W1)] <- 0;temp.W1 <- abs(temp.W1)
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion))){
          temp.W2 <- var.gir.myl.W2[[i]] * (10^scale) ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0;temp.W2 <- abs(temp.W2)
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion))){
          temp.M1 <- var.gir.myl.M1[[i]] * (10^scale) ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0;temp.M1 <- abs(temp.M1)
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion))){
          temp.M3 <- var.gir.myl.M3[[i]] * (10^scale) ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0;temp.M3 <- abs(temp.M3)
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion))){
          temp.M6 <- var.gir.myl.M6[[i]] * (10^scale) ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0;temp.M6 <- abs(temp.M6)
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion))){
          temp.M9 <- var.gir.myl.M9[[i]] * (10^scale) ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0;temp.M9 <- abs(temp.M9)
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion))){
          temp.Y1 <- var.gir.myl.Y1[[i]] * (10^scale) ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0;temp.Y1 <- abs(temp.Y1)
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
        if(!is.na(match("loan",inclusion))){
          temp.loan <- loan.network[[i]] %>% log ;#temp.loan <- apply(temp.loan, c(1,2), round);
          diag(temp.loan) <- 0;temp.loan[is.na(temp.loan)] <- 0
          temp.loan <- as.matrix(temp.loan,nnode,nnode)
          if(!is.na(match("loan",trans))){temp.loan <- t(temp.loan)}
        }
      }
      if(BidType == "var.fevd"){
        
        if(!is.na(match("ON",inclusion))){
          temp.ON <- var.fevd.myl.ON[[i]] * (10^scale) ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion))){
          temp.W1 <- var.fevd.myl.W1[[i]] * (10^scale) ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion))){
          temp.W2 <- var.fevd.myl.W2[[i]] * (10^scale)  ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion))){
          temp.M1 <- var.fevd.myl.M1[[i]] * (10^scale)  ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion))){
          temp.M3 <- var.fevd.myl.M3[[i]] * (10^scale)  ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion))){
          temp.M6 <- var.fevd.myl.M6[[i]] * (10^scale)  ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion))){
          temp.M9 <- var.fevd.myl.M9[[i]] * (10^scale)  ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion))){
          temp.Y1 <- var.fevd.myl.Y1[[i]] * (10^scale)  ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
        if(!is.na(match("loan",inclusion))){
          temp.loan <- loan.network[[i]] %>% log  ;#temp.loan <- apply(temp.loan, c(1,2), round);
          diag(temp.loan) <- 0;temp.loan[is.na(temp.loan)] <- 0
          temp.loan <- as.matrix(temp.loan,nnode,nnode)
          if(!is.na(match("loan",trans))){temp.loan <- t(temp.loan)}
        }
      }
    }
    
    if(triangle == "upper"){
      if(!is.na(match("ON",inclusion))){
        temp.ON[base::lower.tri(temp.ON)] <- 0
      }
      if(!is.na(match("W1",inclusion))){
        temp.W1[base::lower.tri(temp.W1)] <- 0
      }
      if(!is.na(match("W2",inclusion))){
        temp.W2[base::lower.tri(temp.W2)] <- 0
      }
      if(!is.na(match("M1",inclusion))){
        temp.M1[base::lower.tri(temp.M1)] <- 0
      }
      if(!is.na(match("M3",inclusion))){
        temp.M3[base::lower.tri(temp.M3)] <- 0
      }
      if(!is.na(match("M6",inclusion))){
        temp.M6[base::lower.tri(temp.M6)] <- 0
      }
      if(!is.na(match("M9",inclusion))){
        temp.M9[base::lower.tri(temp.M9)] <- 0
      }
      if(!is.na(match("Y1",inclusion))){
        temp.Y1[base::lower.tri(temp.Y1)] <- 0
      }
      if(!is.na(match("loan",inclusion))){
        temp.loan[base::lower.tri(temp.loan)] <- 0
      }
    }
    
    if(triangle == "lower"){
      if(!is.na(match("ON",inclusion))){
        temp.ON[base::upper.tri(temp.ON)] <- 0
      }
      if(!is.na(match("W1",inclusion))){
        temp.W1[base::upper.tri(temp.W1)] <- 0
      }
      if(!is.na(match("W2",inclusion))){
        temp.W2[base::upper.tri(temp.W2)] <- 0
      }
      if(!is.na(match("M1",inclusion))){
        temp.M1[base::upper.tri(temp.M1)] <- 0
      }
      if(!is.na(match("M3",inclusion))){
        temp.M3[base::upper.tri(temp.M3)] <- 0
      }
      if(!is.na(match("M6",inclusion))){
        temp.M6[base::upper.tri(temp.M6)] <- 0
      }
      if(!is.na(match("M9",inclusion))){
        temp.M9[base::upper.tri(temp.M9)] <- 0
      }
      if(!is.na(match("Y1",inclusion))){
        temp.Y1[base::upper.tri(temp.Y1)] <- 0
      }
      if(!is.na(match("loan",inclusion))){
        temp.loan[base::upper.tri(temp.loan)] <- 0
      }
    }
    
    result.vergm <-
      ergm(ergm.formula,
           response="weight",
           reference=~Binomial(max.temp.y),
           control=control.ergm(init=c(init, rep(0, n.inclusion)),
                                MCMLE.maxit = MCMLE.maxit),
           verbose=F)
    #plot(gof(result.ergm))
    #mcmc.diagnostics(result.vergm)
    coef.ergm.l[i,] <- result.vergm$coef[2:(n.inclusion+1)]
    p.ergm.l[i,] <- summary(result.vergm)$coefs[2:(n.inclusion+1),4]
    max.l <- c(max.l, max.temp.y)
    setTxtProgressBar(pb, i)
    
    timeend <- Sys.time()
    runningtime<-timeend-timestart
    if(runningtime/60 > 60*36-30){
      write.csv(coef.ergm.l, file = paste0("Coef",set,".csv"))
      write.csv(p.ergm.l, file = paste0("Pvalue",set,".csv"))
    }
  }
  close(pb)
  
  write.csv(coef.ergm.l, file = paste0("Coef",set,".csv"))
  write.csv(p.ergm.l, file = paste0("Pvalue",set,".csv"))
  #dygraph###########################################################################################
  # only one varialbe
  if(n.inclusion == 1){
    file.name <- paste0(set, "_", inclusion)
    
    dy.coef <- cbind(unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(p.ergm.l)) %>% as.data.frame
    names(dy.coef) <- c("coef0.01","coef0.05","coef0.1","p-value")
    
    dy.coef[dy.coef$"p-value">0.01 | is.na(dy.coef$"p-value") == 1, 1] <- NA
    dy.coef[dy.coef$"p-value">0.05 | dy.coef$"p-value"<0.01 | is.na(dy.coef$"p-value") == 1, 2] <- NA
    dy.coef[dy.coef$"p-value">0.1 | dy.coef$"p-value"<0.05 | is.na(dy.coef$"p-value") == 1, 3] <- NA
    dy.coef <- xts(dy.coef, as.Date(Date[-(1:251)], format='%Y-%m-%d'))
    dy.coef <- merge(dy.coef, is.higher$crisis.sma20,join = "left")
    names(dy.coef)[length(names(dy.coef))] <- "Crisis"
    
    color <- colorRampPalette(c("red", "blue"))(3)
    dy.main <- set
    
    saveWidget.dygraph <- 
      dygraph.interbank(dy.data=subset(dy.coef, select = -p.value), dy.main, color, begin.dateWindow = index(dy.coef)[1]) %>%
      dyAxis("y", label = "Coefficients", independentTicks = T) %>%
      dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
      dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")
    
    file.path<-paste0("html/", file.name,".html")
    saveWidget(saveWidget.dygraph, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = F, libdir = NULL,background = "white", title = set, knitrOptions = list())
    
  }
  
  # more than one variable
  if(n.inclusion != 1){
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
      
      dy.coef <- xts(dy.coef, as.Date(Date[-(1:251)], format='%Y-%m-%d'))
      dy.coef <- merge(dy.coef, is.higher$crisis.sma20,join = "left")
      names(dy.coef)[length(names(dy.coef))] <- "Crisis"
      
      color <- colorRampPalette(c("red", "blue"))(n.inclusion.edgecov)
      if(n.inclusion- n.inclusion.edgecov >0){
        color <- c(color, colorRampPalette(c("white", "purple"))(n.inclusion- n.inclusion.edgecov)[-1])
      }
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
  return.result <- scale
  names(return.result) <- "scale"
  return(return.result)
}
#################################################################
#dyCoefErgm.yearly
#################################################################
dyCoefErgm.yearly <- function(data, set, 
                              inclusion.edgecov = NULL,#NULL or variable names
                              inclusion.nodecov = NULL,#NULL or variable names
                              inclusion.nodeocov = NULL,#NULL or variable names
                              inclusion.nodeicov = NULL,#NULL or variable names
                              inclusion.absdiff = NULL,#NULL or variable names
                              inclusion.cyclicalweights = NULL,#NULL or TRUE
                              inclusion.mutual = NULL,#NULL or TRUE
                              inclusion.nodecovar = NULL,#NULL or TRUE
                              inclusion.nodeicovar = NULL,#NULL or TRUE
                              inclusion.nodeocovar = NULL,#NULL or TRUE
                              inclusion.nodeisqrtcovar = NULL,#NULL or TRUE
                              inclusion.nodeosqrtcovar = NULL,#NULL or TRUE
                              inclusion.nodesqrtcovar = NULL,#NULL or TRUE
                              Date = y.period[-c(1,11)], is.higher, wind,
                              csv = FALSE, fig = FALSE, tab = FALSE, trans = NULL, 
                              BidType, 
                              MCMLE.maxit, 
                              triangle,
                              directed
)#BidType = c("aenet","vecm.gir","vecm.fevd","var.gir","var.fevd")#triangle = c("upper","lower")
{
  if(missing(csv)){csv <- FALSE}
  if(missing(fig)){fig <- FALSE}
  if(missing(tab)){tab <- FALSE}
  
  if(missing(trans)){trans <- NULL}# NULL c("y")
  if(missing(BidType)){BidType <- NULL} # NULL or c("aenet","vecm.gir","vecm.fevd","var.gir","var.fevd")
  if(missing(MCMLE.maxit)){MCMLE.maxit <- 100}
  if(missing(triangle)){triangle <- "all"} # c("lower","upper")
  if(missing(directed)){directed <- TRUE} # TRUE FALSE
  
  
  total <- length(data)
  p.ergm.l <- c()
  max.l <- c()
  
  # formula
  ergm.formula <- "temp.y~sum(pow=1)"
  inclusion <- c()
  n.inclusion <- 0
  
  if(!missing(inclusion.edgecov)){
    if(!is.null(inclusion.edgecov)){
      n.inclusion.edgecov <- length(inclusion.edgecov)
      for (i in 1:n.inclusion.edgecov) {
        temp.formula <- paste0("+edgecov(temp.",inclusion.edgecov[i],",form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, inclusion.edgecov)
      n.inclusion <-  n.inclusion + n.inclusion.edgecov
    }
  }
  
  if(!missing(inclusion.nodecov)){
    if(!is.null(inclusion.nodecov)){
      n.inclusion.nodecov <- length(inclusion.nodecov)
      for (i in 1:n.inclusion.nodecov) {
        temp.formula <- paste0("+nodecov('",inclusion.nodecov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, inclusion.nodecov)
      n.inclusion <-  n.inclusion + n.inclusion.nodecov
    }
  }
  
  if(!missing(inclusion.nodeocov)){
    if(!is.null(inclusion.nodeocov)){
      n.inclusion.nodeocov <- length(inclusion.nodeocov)
      for (i in 1:n.inclusion.nodeocov) {
        temp.formula <- paste0("+nodeocov('",inclusion.nodeocov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, paste0("O.",inclusion.nodeocov))
      n.inclusion <-  n.inclusion + n.inclusion.nodeocov
    }
  }
  
  if(!missing(inclusion.nodeicov)){
    if(!is.null(inclusion.nodeicov)){
      n.inclusion.nodeicov <- length(inclusion.nodeicov)
      for (i in 1:n.inclusion.nodeicov) {
        temp.formula <- paste0("+nodeicov('",inclusion.nodeicov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, paste0("I.",inclusion.nodeicov))
      n.inclusion <-  n.inclusion + n.inclusion.nodeicov
    }
  }
  
  if(!missing(inclusion.absdiff)){
    if(!is.null(inclusion.absdiff)){
      n.inclusion.absdiff <- length(inclusion.absdiff)
      for (i in 1:n.inclusion.absdiff) {
        temp.formula <- paste0("+absdiff('",inclusion.absdiff[i],"',pow=1 ,form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, paste0("AD.",inclusion.absdiff))
      n.inclusion <-  n.inclusion + n.inclusion.absdiff
    }  
  }
  
  if(!missing(inclusion.cyclicalweights)){
    if(!is.null(inclusion.cyclicalweights) & inclusion.cyclicalweights == TRUE){
      ergm.formula <- paste0(ergm.formula,"+cyclicalweights(twopath='min',combine='max',affect='min')")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "cyclicalweights")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.mutual)){
    if(!is.null(inclusion.mutual) & inclusion.mutual == TRUE){
      ergm.formula <- paste0(ergm.formula,"+mutual(form='min',threshold=0)")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "mutual")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodecovar)){
    if(!is.null(inclusion.nodecovar) & inclusion.nodecovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodecovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodecovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeicovar)){
    if(!is.null(inclusion.nodeicovar) & inclusion.nodeicovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeicovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeicovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeocovar)){
    if(!is.null(inclusion.nodeocovar) & inclusion.nodeocovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeocovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeocovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeisqrtcovar)){
    if(!is.null(inclusion.nodeisqrtcovar) & inclusion.nodeisqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeisqrtcovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeisqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeosqrtcovar)){
    if(!is.null(inclusion.nodeosqrtcovar) & inclusion.nodeosqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeosqrtcovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeosqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodesqrtcovar)){
    if(!is.null(inclusion.nodesqrtcovar) & inclusion.nodesqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodesqrtcovar(center=TRUE)")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodesqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  ergm.formula <- as.formula(ergm.formula)
  #ergm.formula
  coef.ergm.l <- data.frame(matrix(NA,total,n.inclusion)); names(coef.ergm.l) <- paste0("coef.",inclusion) 
  p.ergm.l <- data.frame(matrix(NA,total,n.inclusion));    names(p.ergm.l) <- paste0("p.",inclusion) 
  std.ergm.l <- data.frame(matrix(NA,total,n.inclusion));  names(std.ergm.l) <- paste0("std.",inclusion) 
  result.vergm.l <- list()
  data <- lapply(data, function(x){
    x[is.na(x)] <- 0
    return(x)
  })
  min.max <- lapply(data, abs)
  min.max <- min(lapply(min.max, max) %>% unlist)
  scale <- ifelse(is.infinite(floor(log(1/min.max,10))),0,floor(log(1/min.max,10))) + 2
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  for (i in 1:total) {
    print(paste0("##########################",i,"##########################"))
    
    temp.y <- data[[i]] * (10^scale)
    nnode <- dim(temp.y)[1]
    temp.y <- apply(temp.y, c(1,2), round)
    diag(temp.y) <- 0
    temp.y <- abs(temp.y)
    max.temp.y <- max(temp.y)
    if(!is.na(match("y",trans))){temp.y <- t(temp.y)}
    if(triangle == "upper"){
      temp.y[base::lower.tri(temp.y)] <- 0
    }
    if(triangle == "lower"){
      temp.y[base::upper.tri(temp.y)] <- 0
    }
    temp.y <- network(temp.y,
                      directed = directed,
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
      temp.y %v% "ecl" <- c("SOB","SOB","SOB","SOB","JECB","JECB","JECB","JECB","JECB",
                            "UCB")
    }
    ########################################################################
    p <- sum(abs(temp.y %e% "weight"))/max.temp.y/network.dyadcount(temp.y)
    init <- log(p/(1-p))
    
    if(!missing(inclusion.edgecov) & !is.null(inclusion.edgecov)){
      if(!is.na(match("loan",inclusion.edgecov))){
        temp.loan <- loan.network[[i]] %>% log  ;#temp.loan <- apply(temp.loan, c(1,2), round);
        diag(temp.loan) <- 0;temp.loan[is.na(temp.loan)] <- 0
        temp.loan <- as.matrix(temp.loan,nnode,nnode)
        if(!is.na(match("loan",trans))){temp.loan <- t(temp.loan)}
      }
      
      if(!is.null(BidType)){
      if(BidType == "aenet"){
        if(!is.na(match("ON",inclusion.edgecov))){
          temp.ON <- aenet.myl.ON[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion.edgecov))){
          temp.W1 <- aenet.myl.W1[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion.edgecov))){
          temp.W2 <- aenet.myl.W2[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion.edgecov))){
          temp.M1 <- aenet.myl.M1[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion.edgecov))){
          temp.M3 <- aenet.myl.M3[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion.edgecov))){
          temp.M6 <- aenet.myl.M6[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion.edgecov))){
          temp.M9 <- aenet.myl.M9[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion.edgecov))){
          temp.Y1 <- aenet.myl.Y1[[i]][1:nnode,1:nnode] * (10^scale) ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
        if(!is.na(match("short",inclusion.edgecov))){
          temp.short <- aenet.myl.short[[i]][1:nnode,1:nnode] * (10^scale) ;
          diag(temp.short) <- 0;temp.short[is.na(temp.short)] <- 0
          if(!is.na(match("short",trans))){temp.short <- t(temp.short)}
        }
        if(!is.na(match("long",inclusion.edgecov))){
          temp.long <- aenet.myl.long[[i]][1:nnode,1:nnode] * (10^scale) ;
          diag(temp.long) <- 0;temp.long[is.na(temp.long)] <- 0
          if(!is.na(match("long",trans))){temp.long <- t(temp.long)}
        }
      }
      if(BidType == "vecm.gir"){
        
        if(!is.na(match("ON",inclusion.edgecov))){
          temp.ON <- vecm.gir.myl.ON[[i]] * (10^scale) ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0; temp.ON <- abs(temp.ON)
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion.edgecov))){
          temp.W1 <- vecm.gir.myl.W1[[i]] * (10^scale) ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0; temp.W1 <- abs(temp.W1)
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion.edgecov))){
          temp.W2 <- vecm.gir.myl.W2[[i]] * (10^scale) ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0; temp.W2 <- abs(temp.W2)
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion.edgecov))){
          temp.M1 <- vecm.gir.myl.M1[[i]] * (10^scale) ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0; temp.M1 <- abs(temp.M1)
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion.edgecov))){
          temp.M3 <- vecm.gir.myl.M3[[i]] * (10^scale) ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0; temp.M3 <- abs(temp.M3)
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion.edgecov))){
          temp.M6 <- vecm.gir.myl.M6[[i]] * (10^scale) ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0; temp.M6 <- abs(temp.M6)
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion.edgecov))){
          temp.M9 <- vecm.gir.myl.M9[[i]] * (10^scale) ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0; temp.M9 <- abs(temp.M9)
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion.edgecov))){
          temp.Y1 <- vecm.gir.myl.Y1[[i]] * (10^scale) ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0; temp.Y1 <- abs(temp.Y1)
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
      }
      if(BidType == "vecm.fevd"){
        
        if(!is.na(match("ON",inclusion.edgecov))){
          temp.ON <- vecm.fevd.myl.ON[[i]] * (10^scale) ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion.edgecov))){
          temp.W1 <- vecm.fevd.myl.W1[[i]] * (10^scale) ;#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion.edgecov))){
          temp.W2 <- vecm.fevd.myl.W2[[i]] * (10^scale) ;#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion.edgecov))){
          temp.M1 <- vecm.fevd.myl.M1[[i]] * (10^scale) ;#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion.edgecov))){
          temp.M3 <- vecm.fevd.myl.M3[[i]] * (10^scale) ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion.edgecov))){
          temp.M6 <- vecm.fevd.myl.M6[[i]] * (10^scale) ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion.edgecov))){
          temp.M9 <- vecm.fevd.myl.M9[[i]] * (10^scale) ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion.edgecov))){
          temp.Y1 <- vecm.fevd.myl.Y1[[i]] * (10^scale) ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
      }
      if(BidType == "var.gir"){
        
        if(!is.na(match("ON",inclusion.edgecov))){
          temp.ON <- var.gir.myl.ON[[i]] * (10^scale) ;#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0;#temp.ON <- abs(temp.ON)
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion.edgecov))){
          temp.W1 <- var.gir.myl.W1[[i]] * (10^scale);#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;#temp.W1[is.na(temp.W1)] <- 0;temp.W1 <- abs(temp.W1)
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion.edgecov))){
          temp.W2 <- var.gir.myl.W2[[i]] * (10^scale);#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0;temp.W2 <- abs(temp.W2)
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion.edgecov))){
          temp.M1 <- var.gir.myl.M1[[i]] * (10^scale);#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0;temp.M1 <- abs(temp.M1)
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion.edgecov))){
          temp.M3 <- var.gir.myl.M3[[i]] * (10^scale);#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0;temp.M3 <- abs(temp.M3)
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion.edgecov))){
          temp.M6 <- var.gir.myl.M6[[i]] * (10^scale);#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0;temp.M6 <- abs(temp.M6)
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion.edgecov))){
          temp.M9 <- var.gir.myl.M9[[i]] * (10^scale);#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0;temp.M9 <- abs(temp.M9)
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion.edgecov))){
          temp.Y1 <- var.gir.myl.Y1[[i]] * (10^scale);#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0;temp.Y1 <- abs(temp.Y1)
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
      }
      if(BidType == "var.fevd"){
        
        if(!is.na(match("ON",inclusion.edgecov))){
          temp.ON <- var.fevd.myl.ON[[i]] * (10^scale);#temp.ON <- apply(temp.ON, c(1,2), round);
          diag(temp.ON) <- 0;temp.ON[is.na(temp.ON)] <- 0
          if(!is.na(match("ON",trans))){temp.ON <- t(temp.ON)}
        }
        if(!is.na(match("W1",inclusion.edgecov))){
          temp.W1 <- var.fevd.myl.W1[[i]] * (10^scale);#temp.W1 <- apply(temp.W1, c(1,2), round);
          diag(temp.W1) <- 0;temp.W1[is.na(temp.W1)] <- 0
          if(!is.na(match("W1",trans))){temp.W1 <- t(temp.W1)}
        }
        if(!is.na(match("W2",inclusion.edgecov))){
          temp.W2 <- var.fevd.myl.W2[[i]]  * (10^scale);#temp.W2 <- apply(temp.W2, c(1,2), round);
          diag(temp.W2) <- 0;temp.W2[is.na(temp.W2)] <- 0
          if(!is.na(match("W2",trans))){temp.W2 <- t(temp.W2)}
        }
        if(!is.na(match("M1",inclusion.edgecov))){
          temp.M1 <- var.fevd.myl.M1[[i]]  * (10^scale);#temp.M1 <- apply(temp.M1, c(1,2), round);
          diag(temp.M1) <- 0;temp.M1[is.na(temp.M1)] <- 0
          if(!is.na(match("M1",trans))){temp.M1 <- t(temp.M1)}
        }
        if(!is.na(match("M3",inclusion.edgecov))){
          temp.M3 <- var.fevd.myl.M3[[i]] * (10^scale) ;#temp.M3 <- apply(temp.M3, c(1,2), round);
          diag(temp.M3) <- 0;temp.M3[is.na(temp.M3)] <- 0
          if(!is.na(match("M3",trans))){temp.M3 <- t(temp.M3)}
        }
        if(!is.na(match("M6",inclusion.edgecov))){
          temp.M6 <- var.fevd.myl.M6[[i]] * (10^scale) ;#temp.M6 <- apply(temp.M6, c(1,2), round);
          diag(temp.M6) <- 0;temp.M6[is.na(temp.M6)] <- 0
          if(!is.na(match("M6",trans))){temp.M6 <- t(temp.M6)}
        }
        if(!is.na(match("M9",inclusion.edgecov))){
          temp.M9 <- var.fevd.myl.M9[[i]] * (10^scale) ;#temp.M9 <- apply(temp.M9, c(1,2), round);
          diag(temp.M9) <- 0;temp.M9[is.na(temp.M9)] <- 0
          if(!is.na(match("M9",trans))){temp.M9 <- t(temp.M9)}
        }
        if(!is.na(match("Y1",inclusion.edgecov))){
          temp.Y1 <- var.fevd.myl.Y1[[i]] * (10^scale) ;#temp.Y1 <- apply(temp.Y1, c(1,2), round);
          diag(temp.Y1) <- 0;temp.Y1[is.na(temp.Y1)] <- 0
          if(!is.na(match("Y1",trans))){temp.Y1 <- t(temp.Y1)}
        }
      }
    }
    }
    
    if(triangle == "upper"){
      if(!is.na(match("ON",inclusion.edgecov))){
        temp.ON[base::lower.tri(temp.ON)] <- 0}
      if(!is.na(match("W1",inclusion.edgecov))){
        temp.W1[base::lower.tri(temp.W1)] <- 0
      }
      if(!is.na(match("W2",inclusion.edgecov))){
        temp.W2[base::lower.tri(temp.W2)] <- 0
      }
      if(!is.na(match("M1",inclusion.edgecov))){
        temp.M1[base::lower.tri(temp.M1)] <- 0
      }
      if(!is.na(match("M3",inclusion.edgecov))){
        temp.M3[base::lower.tri(temp.M3)] <- 0
      }
      if(!is.na(match("M6",inclusion.edgecov))){
        temp.M6[base::lower.tri(temp.M6)] <- 0
      }
      if(!is.na(match("M9",inclusion.edgecov))){
        temp.M9[base::lower.tri(temp.M9)] <- 0
      }
      if(!is.na(match("Y1",inclusion.edgecov))){
        temp.Y1[base::lower.tri(temp.Y1)] <- 0
      }
      if(!is.na(match("loan",inclusion.edgecov))){
        temp.loan[base::lower.tri(temp.loan)] <- 0
      }
    }
    
    if(triangle == "lower"){
      if(!is.na(match("ON",inclusion.edgecov))){
        temp.ON[base::upper.tri(temp.ON)] <- 0}
      if(!is.na(match("W1",inclusion.edgecov))){
        temp.W1[base::upper.tri(temp.W1)] <- 0
      }
      if(!is.na(match("W2",inclusion.edgecov))){
        temp.W2[base::upper.tri(temp.W2)] <- 0
      }
      if(!is.na(match("M1",inclusion.edgecov))){
        temp.M1[base::upper.tri(temp.M1)] <- 0
      }
      if(!is.na(match("M3",inclusion.edgecov))){
        temp.M3[base::upper.tri(temp.M3)] <- 0
      }
      if(!is.na(match("M6",inclusion.edgecov))){
        temp.M6[base::upper.tri(temp.M6)] <- 0
      }
      if(!is.na(match("M9",inclusion.edgecov))){
        temp.M9[base::upper.tri(temp.M9)] <- 0
      }
      if(!is.na(match("Y1",inclusion.edgecov))){
        temp.Y1[base::upper.tri(temp.Y1)] <- 0
      }
      if(!is.na(match("loan",inclusion.edgecov))){
        temp.loan[base::upper.tri(temp.loan)] <- 0
      }
    }
    
    result.vergm <-
      ergm(ergm.formula,
           response="weight",
           reference=~Binomial(max.temp.y),
           control=control.ergm(init=c(init, rep(0, n.inclusion)),
                                MCMLE.maxit=MCMLE.maxit),
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

###################################################################
# fill blanks in matrix
###################################################################
F.fill.up.NAs <- function(data = list()){
  n <- length(data)
  nrow.data <- nrow(data[[1]])
  ncol.data <- ncol(data[[1]])
  for(i in (n-1):1){
    for(nr in 1:nrow.data){
      for(nc in 1:ncol.data){
        test <- is.na(data[[i]][nr,nc])
        if(test == TRUE){data[[i]][nr,nc] <- data[[i+1]][nr,nc]}
      }
    }
  }
  
  for(i in 2:n){
    for(nr in 1:nrow.data){
      for(nc in 1:ncol.data){
        test <- is.na(data[[i]][nr,nc])
        if(test == TRUE){data[[i]][nr,nc] <- data[[i-1]][nr,nc]}
      }
    }
  }
  return(data)
}
####################################################################
#dyCoefErgm.RMB
####################################################################
dyCoefErgm.RMB <- function(data,
                           inclusion.edgecov = NULL,#NULL or variable names
                           inclusion.nodecov = NULL,#NULL or variable names
                           inclusion.nodeocov = NULL,#NULL or variable names
                           inclusion.nodeicov = NULL,#NULL or variable names
                           inclusion.absdiff = NULL,#NULL or variable names
                           inclusion.cyclicalweights = FALSE,#FALSE or TRUE
                           inclusion.mutual = FALSE,#FALSE or TRUE
                           inclusion.nodecovar = FALSE,#FALSE or TRUE
                           inclusion.nodeicovar = FALSE,#FALSE or TRUE
                           inclusion.nodeocovar = FALSE,#FALSE or TRUE
                           inclusion.nodeisqrtcovar = FALSE,#FALSE or TRUE
                           inclusion.nodeosqrtcovar = FALSE,#FALSE or TRUE
                           inclusion.nodesqrtcovar = FALSE,#FALSE or TRUE
                           Date, 
                           list.node,
                           tab = FALSE, 
                           trans = NULL, 
                           MCMLE.maxit, 
                           directed = TRUE
)#BidType = c("aenet","vecm.gir","vecm.fevd","var.gir","var.fevd")#triangle = c("upper","lower")
{
  if(missing(tab)){tab <- FALSE}
  if(missing(trans)){trans <- NULL}# NULL c("y")
  if(missing(MCMLE.maxit)){MCMLE.maxit <- 20}
  if(missing(directed)){directed <- TRUE} # TRUE FALSE
  
  
  total <- length(data)
  p.ergm.l <- c()
  max.l <- c()
  ###############################################################
  # formula
  ###############################################################
  ergm.formula <- "temp.y~sum(pow=1)"
  inclusion <- c()
  n.inclusion <- 0
  
  if(!missing(inclusion.edgecov)){
    if(!is.null(inclusion.edgecov)){
      n.inclusion.edgecov <- length(inclusion.edgecov)
      for (i in 1:n.inclusion.edgecov) {
        temp.formula <- paste0("+edgecov(temp.",inclusion.edgecov[i],",form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- knitr::combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, inclusion.edgecov)
      n.inclusion <-  n.inclusion + n.inclusion.edgecov
    }
  }
  
  if(!missing(inclusion.nodecov)){
    if(!is.null(inclusion.nodecov)){
      n.inclusion.nodecov <- length(inclusion.nodecov)
      for (i in 1:n.inclusion.nodecov) {
        temp.formula <- paste0("+nodecov('",inclusion.nodecov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, inclusion.nodecov)
      n.inclusion <-  n.inclusion + n.inclusion.nodecov
    }
  }
  
  if(!missing(inclusion.nodeocov)){
    if(!is.null(inclusion.nodeocov)){
      n.inclusion.nodeocov <- length(inclusion.nodeocov)
      for (i in 1:n.inclusion.nodeocov) {
        temp.formula <- paste0("+nodeocov('",inclusion.nodeocov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "") 
      inclusion <- c(inclusion, paste0("O.",inclusion.nodeocov))
      n.inclusion <-  n.inclusion + n.inclusion.nodeocov
    }
  }
  
  if(!missing(inclusion.nodeicov)){
    if(!is.null(inclusion.nodeicov)){
      n.inclusion.nodeicov <- length(inclusion.nodeicov)
      for (i in 1:n.inclusion.nodeicov) {
        temp.formula <- paste0("+nodeicov('",inclusion.nodeicov[i],"',form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, paste0("I.",inclusion.nodeicov))
      n.inclusion <-  n.inclusion + n.inclusion.nodeicov
    }
  }
  
  if(!missing(inclusion.absdiff)){
    if(!is.null(inclusion.absdiff)){
      n.inclusion.absdiff <- length(inclusion.absdiff)
      for (i in 1:n.inclusion.absdiff) {
        temp.formula <- paste0("+absdiff('",inclusion.absdiff[i],"',pow=1 ,form = 'sum')")
        ergm.formula <- paste0(ergm.formula,temp.formula)
      }
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, paste0("AD.",inclusion.absdiff))
      n.inclusion <-  n.inclusion + n.inclusion.absdiff
    }  
  }
  
  if(!missing(inclusion.cyclicalweights)){
    if(inclusion.cyclicalweights == TRUE){
      ergm.formula <- paste0(ergm.formula,"+cyclicalweights(twopath='min',combine='max',affect='min')")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "cyclicalweights")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.mutual)){
    if(inclusion.mutual == TRUE){
      ergm.formula <- paste0(ergm.formula,"+mutual(form='min',threshold=0)")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "mutual")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodecovar)){
    if(inclusion.nodecovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodecovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodecovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeicovar)){
    if(inclusion.nodeicovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeicovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeicovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeocovar)){
    if(inclusion.nodeocovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeocovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeocovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeisqrtcovar)){
    if(inclusion.nodeisqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeisqrtcovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeisqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodeosqrtcovar)){
    if(inclusion.nodeosqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodeosqrtcovar")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodeosqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  
  if(!missing(inclusion.nodesqrtcovar)){
    if(inclusion.nodesqrtcovar == TRUE){
      ergm.formula <- paste0(ergm.formula,"+nodesqrtcovar(center=TRUE)")
      ergm.formula <- combine_words(ergm.formula, sep = "", and = "")
      inclusion <- c(inclusion, "nodesqrtcovar")
      n.inclusion <-  n.inclusion + 1
    }
  }
  ergm.formula <- as.formula(ergm.formula)
  ###############################################################
  #ergm
  ###############################################################
  coef.ergm.l <- data.frame(matrix(NA,total,n.inclusion)); names(coef.ergm.l) <- paste0("coef.",inclusion) 
  p.ergm.l <- data.frame(matrix(NA,total,n.inclusion));    names(p.ergm.l) <- paste0("p.",inclusion) 
  std.ergm.l <- data.frame(matrix(NA,total,n.inclusion));  names(std.ergm.l) <- paste0("std.",inclusion) 
  result.vergm.l <- list()
  data <- lapply(data, function(x){
    x[is.na(x)] <- 0
    return(x)
  })
  min.max <- lapply(data, abs)
  min.max <- min(lapply(min.max, max) %>% unlist)
  scale <- ifelse(is.infinite(floor(log(1/min.max,10))),0,floor(log(1/min.max,10))) + 2
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  for (i in 1:total) {
    print(paste0("##########################",i,"##########################"))
    
    temp.y <- data[[i]] * (10^scale)
    nnode <- dim(temp.y)[1]
    temp.y <- apply(temp.y, c(1,2), round)
    diag(temp.y) <- 0
    temp.y <- abs(temp.y)
    max.temp.y <- max(temp.y)
    if(!is.na(match("y",trans))){temp.y <- t(temp.y)}
    temp.y <- network(temp.y,
                      directed = directed,
                      loops = FALSE,# self loop
                      matrix.type = "adjacency",
                      ignore.eval=FALSE,
                      names.eval="weight")
    ########################################################################
    if(!missing(list.node)){
      temp.node <- list.node[[i]]
      
      temp.y %v% "RIR" <- temp.node$RIR #"Real interest rate (%)" #RIR
      temp.y %v% "GDP" <- temp.node$GDP %>% log #"GDP (current US$)" #GDP 
      temp.y %v% "GGR" <- temp.node$GGR #"GDP growth (annual %)" #GGR
      temp.y %v% "POP" <- temp.node$POP %>% log #"Population, total" #POP
      temp.y %v% "IGD" <- temp.node$IGD #"Inflation, GDP deflator (annual %)"   #IGD 
      temp.y %v% "ICP" <- temp.node$ICP #"Inflation, consumer prices (annual %)" #ICP
      temp.y %v% "LND" <- temp.node$LND %>% log  #"Land area (sq. km)" #LND
      temp.y %v% "BND" <- temp.node$BND #"Bound rate, simple mean, all products (%)"  @BND
      temp.y %v% "TRF" <- temp.node$TRF #"Tariff rate, applied, simple mean, all products (%)" @TRF 
      temp.y %v% "LIR" <- temp.node$LIR #"Lending interest rate (%)"    #LIR
      temp.y %v% "IRS" <- temp.node$IRS #"Interest rate spread (lending rate minus deposit rate, %)" #IRS
      temp.y %v% "IOF" <- temp.node$IOF 
      temp.y %v% "IIF" <- temp.node$IIF 
      temp.y %v% "IOS" <- temp.node$IOS 
      temp.y %v% "IIS" <- temp.node$IIS
    }
    #c("RIR","GDP","GGR","POP","IGD","ICP","LND","BND","TRF","LIR","IRS")
    ########################################################################
    p <- sum(abs(temp.y %e% "weight"))/max.temp.y/network.dyadcount(temp.y)
    init <- log(p/(1-p))
    if(!missing(inclusion.edgecov) & !is.null(inclusion.edgecov)){
      if(!is.na(match("out",inclusion.edgecov))){
        temp.out<- list.out[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.out) <- 0;temp.out[is.na(temp.out)] <- 0
        if(!is.na(match("out",trans))){temp.out <- t(temp.out)}
      }
      if(!is.na(match("im",inclusion.edgecov))){
        temp.im <- list.im[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.im) <- 0;temp.im[is.na(temp.im)] <- 0
        if(!is.na(match("im",trans))){temp.im <- t(temp.im)}
      }
      if(!is.na(match("distw",inclusion.edgecov))){
        temp.distw <- list.distw[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.distw) <- 0;temp.distw[is.na(temp.distw)] <- 0
        if(!is.na(match("distw",trans))){temp.distw <- t(temp.distw)}
      }
      if(!is.na(match("distw",inclusion.edgecov))){
        temp.distw <- list.distw[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.distw) <- 0;temp.distw[is.na(temp.distw)] <- 0
        if(!is.na(match("distw",trans))){temp.distw <- t(temp.distw)}
      }
      if(!is.na(match("contig",inclusion.edgecov))){
        temp.contig <- list.contig[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.contig) <- 0;temp.contig[is.na(temp.contig)] <- 0
        if(!is.na(match("contig",trans))){temp.contig <- t(temp.contig)}
      }
      if(!is.na(match("comlang_off",inclusion.edgecov))){
        temp.comlang_off <- list.comlang_off[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.comlang_off) <- 0;temp.comlang_off[is.na(temp.comlang_off)] <- 0
        if(!is.na(match("comlang_off",trans))){temp.comlang_off <- t(temp.comlang_off)}
      }
      if(!is.na(match("comlang_ethno",inclusion.edgecov))){
        temp.comlang_ethno <- list.comlang_ethno[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.comlang_ethno) <- 0;temp.comlang_ethno[is.na(temp.comlang_ethno)] <- 0
        if(!is.na(match("comlang_ethno",trans))){temp.comlang_ethno <- t(temp.comlang_ethno)}
      }
      if(!is.na(match("conflict",inclusion.edgecov))){
        temp.conflict <- list.conflict[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.conflict) <- 0;temp.conflict[is.na(temp.conflict)] <- 0
        if(!is.na(match("conflict",trans))){temp.conflict <- t(temp.conflict)}
      }
      if(!is.na(match("comrelig",inclusion.edgecov))){
        temp.comrelig <- list.comrelig[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.comrelig) <- 0;temp.comrelig[is.na(temp.comrelig)] <- 0
        if(!is.na(match("comrelig",trans))){temp.comrelig <- t(temp.comrelig)}
      }
      if(!is.na(match("tdiff",inclusion.edgecov))){
        temp.tdiff <- list.tdiff[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.tdiff) <- 0;temp.tdiff[is.na(temp.tdiff)] <- 0
        if(!is.na(match("tdiff",trans))){temp.tdiff <- t(temp.tdiff)}
      }
      if(!is.na(match("comleg_posttrans",inclusion.edgecov))){
        temp.comleg_posttrans <- list.comleg_posttrans[[i]];
        diag(temp.comleg_posttrans) <- 0;temp.comleg_posttrans[is.na(temp.comleg_posttrans)] <- 0
        if(!is.na(match("comleg_posttrans",trans))){temp.comleg_posttrans <- t(temp.comleg_posttrans)}
      }
      if(!is.na(match("fta_hmr",inclusion.edgecov))){
        temp.fta_hmr <- list.fta_hmr[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.fta_hmr) <- 0;temp.fta_hmr[is.na(temp.fta_hmr)] <- 0
        if(!is.na(match("fta_hmr",trans))){temp.fta_hmr <- t(temp.fta_hmr)}
      }
      if(!is.na(match("entry_cost_o",inclusion.edgecov))){
        temp.entry_cost_o <- list.entry_cost_o[[i]][1:nnode,1:nnode] * (10^scale) ;
        diag(temp.entry_cost_o) <- 0;temp.entry_cost_o[is.na(temp.entry_cost_o)] <- 0
        if(!is.na(match("entry_cost_o",trans))){temp.entry_cost_o <- t(temp.entry_cost_o)}
      }
    }
    
    result.vergm <-
      ergm(ergm.formula,
           response="weight",
           reference=~Binomial(max.temp.y),
           control=control.ergm(init=c(init, rep(0, n.inclusion)),
                                MCMLE.maxit=MCMLE.maxit),
           verbose=F)
    #plot(gof(result.ergm))
    #mcmc.diagnostics(result.vergm)
    result.vergm.l[[i]] <- result.vergm
    coef.ergm.l[i,] <- result.vergm$coef[2:(n.inclusion+1)]
    p.ergm.l[i,] <- summary(result.vergm)$coefs[2:(n.inclusion+1),4]
    std.ergm.l[i,] <- summary(result.vergm)$coefs[2:(n.inclusion+1),2]
    
    max.l <- c(max.l, max.temp.y)
    setTxtProgressBar(pb, i)
    
  }
  close(pb)
  #tab###########################################################################################
  
  if(tab){
    tab.ergm <- list()
    tab.ergm$coef <- coef.ergm.l
    tab.ergm$pvalue <- p.ergm.l
    tab.ergm$std <- std.ergm.l
    return.result <- list(result.vergm.l,tab.ergm,scale, ergm.formula)
    names(return.result) <- c("result.vergm.l","tab.ergm","scale", "formula")
  }
  if(!tab){
    return.result <- list(result.vergm.l, scale, ergm.formula)
    names(return.result) <- c("result.vergm.l","scale", "formula")
  }
  return(return.result)
}
####################################################################
# mean without NAs
####################################################################
mean.without.nas <- function(x, is.row){
  y <- c()
  #
  if(is.row == TRUE){
    for (i in 1:dim(x)[1]) {
      n.na <- is.na(x[i,]) %>% sum
      if(n.na == dim(x)[2]){y[i] <- NA} else {
        locate <- which(!is.na(x[i,]))
        y[i] <- sum(x[i,locate])/length(locate)
      }
    }
  }
  #
  if(is.row == FALSE){
    for (i in 1:dim(x)[2]) {
      n.na <- is.na(x[,i]) %>% sum
      if(n.na == dim(x)[1]){y[i] <- NA} else {
        locate <- which(!is.na(x[,i]))
        y[i] <- sum(x[locate,i])/length(locate)
      }
    }
  }
  #
  return(y)
}
#mean.without.nas(x,T)
#mean.without.nas(x,F)
#test <- sample(x = c(1:10,NA), size = 3*3, replace = FALSE, prob = NULL)
#x <- matrix(test,3,3)
####################################################################
# split series into equal sized groups
####################################################################
split.equally <- function(x, size){
  if(length(x)%%size > 0){
    n.list <- length(x)%/%size + 1
  }else{
    n.list <- length(x)%/%size
  }
    y <- split(x, cut(x, n.list))
    list.return <- list(y,n.list)
    names(list.return) <- c("y","n.list")
  return(list.return)
} 








  
