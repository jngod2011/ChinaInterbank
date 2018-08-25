#################################################################
# fevd.matrix
#################################################################
fevd2matrix <- function(x, n.ahead){
 n <- length(x)
 fevd.matrix <- matrix(rep(NA, n^2), n, n) 
 for (i in 1:n) {
   fevd.matrix[i,] <- x[[i]][n.ahead,]
 }
 rownames(fevd.matrix) <- names(x)
 colnames(fevd.matrix) <- names(x)
 return(fevd.matrix)
}
#################################################################
# m2tx
#################################################################
m2tx <- function(x,Date){
  n <- length(x)
  df.out <- c()
  df.from <- c()
  df.cross <- c()
  df.net <- c()
  df.sys.a <- c()
  df.sys.b <- c()
  Name <- colnames(x[[1]])
  for (i in 1:n) {
    temp.x <- x[[i]]
    x.d0 <- temp.x #%>% as.matrix
    diag(x.d0) <- 0
    temp.out <- colSums(x.d0)
    temp.from <- rowSums(x.d0)
    temp.cross <- temp.out + temp.from
    temp.net <- temp.out - temp.from
    temp.sys.a <- sum(x.d0)
    temp.sys.b <- sum(x.d0)/ncol(x.d0)
    
    df.out <- rbind(df.out, temp.out)
    df.from <- rbind(df.from, temp.from)
    df.cross <- rbind(df.cross, temp.cross)
    df.net <- rbind(df.net, temp.net)
    df.sys.a <- rbind(df.sys.a, temp.sys.a)
    df.sys.b <- rbind(df.sys.b, temp.sys.b)
  }
  names(df.out) <- Name; df.out <- xts(df.out, as.Date(Date, format='%Y-%m-%d'))
  names(df.from) <- Name; df.from <- xts(df.from, as.Date(Date, format='%Y-%m-%d'))
  names(df.cross) <- Name; df.cross <- xts(df.cross, as.Date(Date, format='%Y-%m-%d'))
  names(df.net) <- Name; df.net <- xts(df.net, as.Date(Date, format='%Y-%m-%d'))
  names(df.sys.a) <- "sys.a"; df.sys.a <- xts(df.sys.a, as.Date(Date, format='%Y-%m-%d'))
  names(df.sys.b) <- "sys.b"; df.sys.b <- xts(df.sys.b, as.Date(Date, format='%Y-%m-%d'))
  
  list.retrun <- list(df.out,df.from,df.cross,df.net,df.sys.a,df.sys.b)
  names(list.retrun) <- c("index.out","index.from","index.cross","index.net","index.sys.a","index.sys.b")
  return(list.retrun)
}

#################################################################
# dag.SVAR.fevd
################################################################# 
dag.SVAR.fevd <- function(data, n.ahead, lag.var, amat){
  result.var <- VAR(data, p = lag.var, type = "const")
  result.svar <- SVAR(x = result.var, estmethod = "direct", Amat = amat, Bmat = NULL,hessian = TRUE, method="BFGS")
  fevd.svar <- fevd(result.svar, n.ahead = n.ahead)
  m.fevd <- fevd2matrix(fevd.svar, n.ahead = n.ahead)
  
  list.return <- list(m.fevd)
  names(list.return) <- c("m.fevd")
  return(list.return)
}

#################################################################
# dy.dag.SVAR.fevd
################################################################# 
dy.dag.SVAR.fevd <- function(data, n.ahead, lag.var, mode, span, amat){
    tic()
    n <- nrow(data)
    data <- data %>% na.omit
    
    pb <- txtProgressBar(min = 0, max = ifelse(span == "yearly", 1, n-span), 
                         style = 3)
    fevd.matrix <- list()
    
    if(class(data)[[1]] == "data.frame" & !is.na(match("Date",names(data)))){
      Date <- data$Date[-c(1:span)]
      data <- subset(data, select = -Date)
    }
    if(class(data)[[1]] == "xts"){Date <- index(data)}
    
    if(span == "yearly"){
      fevd.matrix[[1]] <- dag.SVAR.fevd(data = data, n.ahead = n.ahead, lag.var=lag.var, amat = amat)$m.fevd
      colnames(fevd.matrix[[1]]) <- rownames(fevd.matrix[[1]]) <- names(data)
      setTxtProgressBar(pb, 1)
    }else{
      for (i in 1:(n-span)){
        if (mode == "iteration") {
          fevd.matrix[[i]] <- dag.SVAR.fevd(data = data[1:(span + i-1),], n.ahead = n.ahead, lag.var=lag.var, amat = amat)$m.fevd
          colnames(fevd.matrix[[i]]) <- rownames(fevd.matrix[[i]]) <- names(data)
          }
        if (mode == "fix") {
          fevd.matrix[[i]] <- dag.SVAR.fevd(data = data[(1 + i):(span + i-1),], n.ahead = n.ahead, lag.var=lag.var, amat = amat)$m.fevd
          colnames(fevd.matrix[[i]]) <- rownames(fevd.matrix[[i]]) <- names(data)
          }
        setTxtProgressBar(pb, i)
      }
    }
    toc()  
    close(pb)
    list.result <- list(fevd.matrix)
    names(list.result) <- c("fevd.matrix")
    return(list.result)
  }
#################################################################
# growth_rate
################################################################# 
growth_rate <- function(x){
  y <- data.frame(matrix(NA, ncol = NCOL(x) , nrow = NROW(x) - 1))
  for(i in 1:NCOL(x)){
    y[,i ] <- diff(x[, i])/x[-nrow(x), i]
  }
  y[is.na(y)] <- 0
  y[y == Inf] <- 1
  return(y)
}
#################################################################
# tab.jarque.test
################################################################# 
tab.jarque.test <- function(x, digits = 0, type){
  require(moments)
  x <- x %>% na.omit %>% as.vector 
  y <- moments::jarque.test(x)
  #y <- tseries::jarque.bera.test(x)
  if(type =="latex"){
    y <- paste0(
      format(round(y$statistic, digits=digits), nsmall = digits),
      "$","^{",significance(y$p.value),"}$")
  }
  if(type =="excel"){
    y <- paste0(format(round(y$statistic, digits=digits), nsmall = digits),
                significance(y$p.value))
  }
  return(y)
  #y$statistic#y$parameter#y$p.value#y$method#y$data.name
  }
#################################################################
# tab.AR1
#################################################################
tab.AR1 <- function(x, digits = 2, type){
  y <- lm(x[-1]~x[-length(x)])
  y <- summary(y)
  p <- y$coefficients[2,4]
  coef <- y$coefficients[2,1]
  if(type =="latex"){
    y <- paste0(format(round(coef, digits = digits),nsmall = digits),"$","^{",significance(p),"}$")
    
  }
  if(type =="excel"){
    y <- paste0(format(round(coef, digits = digits), nsmall = digits),significance(p))
  }
  return(y)
}
#################################################################
# tab.Nobs
#################################################################
tab.Nobs <-function(x){
  y <- x %>% na.omit %>% length
  return(y)
}
#################################################################
# tab.mean
#################################################################
tab.mean <- function(x){
  y <- mean(x %>% na.omit)
  return(y)
}
#################################################################
# tab.sd
#################################################################
tab.sd <- function(x){
  y <- sd(x %>% na.omit)
  return(y)
}
#################################################################
# tab.skewness
#################################################################
tab.skewness <- function(x){
  y <- moments::skewness(x %>% na.omit)
  return(y)
}
#################################################################
# tab.kurtosis
#################################################################
tab.kurtosis <- function(x){
  y <- moments::kurtosis(x %>% na.omit)
  return(y)
}
#################################################################
# tab.min
#################################################################
tab.min <- function(x){
  y <-min(x %>% na.omit)
  return(y)
}
#################################################################
# tab.kurtosis
#################################################################
tab.max <- function(x){
  y <- max(x %>% na.omit)
  return(y)
}
#################################################################
# tab.ADF
#################################################################
tab.ADF.phi2 <- function(x, selectlags = "AIC", digits, type){
  x <- x %>% na.omit
  ADFtest.t <- ur.df(x, type = "trend", selectlags = "AIC") 
  if(type == "latex"){
    if(abs(ADFtest.t@teststat[2]) < abs(ADFtest.t@cval[2,3])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"")
    }
    if(abs(ADFtest.t@teststat[2]) >= abs(ADFtest.t@cval[2,3])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"$^{*}$")
    }
    if(abs(ADFtest.t@teststat[2]) > abs(ADFtest.t@cval[2,2])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"$^{**}$")
    }
    if(abs(ADFtest.t@teststat[2]) > abs(ADFtest.t@cval[2,1])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"$^{***}$")
    }
  }
  if(type == "excel"){
    if(abs(ADFtest.t@teststat[2]) < abs(ADFtest.t@cval[2,3])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"")
    }
    if(abs(ADFtest.t@teststat[2]) >= abs(ADFtest.t@cval[2,3])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"*")
    }
    if(abs(ADFtest.t@teststat[2]) > abs(ADFtest.t@cval[2,2])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"**")
    }
    if(abs(ADFtest.t@teststat[2]) > abs(ADFtest.t@cval[2,1])){
      y <- paste0(format(round(ADFtest.t@teststat[2], digits = digits), nsmall = digits),"***")
    }
  }
  return(y)
}








