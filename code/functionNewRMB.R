#################################################################
#Start stop watch timer
#################################################################
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")){
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}
#################################################################
#Read elapsed time from stopwatch
#################################################################
toc <- function(){
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}
#################################################################
# export ADF test
#################################################################
ADFtest.phi2 <- function(data, digits, selectlags){#"AIC""BIC"
  if(missing(selectlags)){selectlags <- "AIC"}
  if(missing(digits)){digits <- 2}
  result.ADFtest <- data.frame(matrix(NA, 2, ncol(data)))
  names(result.ADFtest) <- names(data)
  rownames(result.ADFtest) <- c("t-value","lags")
  for(i in 1:ncol(data)){
    ADFtest.t <- ur.df(data[,i], type = "trend", selectlags = selectlags)
    if(abs(ADFtest.t@teststat[2]) < abs(ADFtest.t@cval[2,3])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[2],digits),"")
    }
    if(abs(ADFtest.t@teststat[2]) >= abs(ADFtest.t@cval[2,3])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[2],digits),"$^{*}$")
    }
    if(abs(ADFtest.t@teststat[2]) > abs(ADFtest.t@cval[2,2])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[2],digits),"$^{**}$")
    }
    if(abs(ADFtest.t@teststat[2]) > abs(ADFtest.t@cval[2,1])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[2],digits),"$^{***}$")
    }
    result.ADFtest[2, ] <- ADFtest.t@lags
  }
  return(result.ADFtest)
}
#################################################################
# export the simpliest ADF test
#################################################################
ADFtest.tau3 <- function(data, selectlags){
  # Lag selection can be achieved according to the Akaike "AIC" or the Bayes "BIC" information criteria.
  result.ADFtest <- data.frame(matrix(NA, 2, ncol(data)))
  names(result.ADFtest) <- names(data)
  rownames(result.ADFtest) <- c("t-value","lags")
  result.ADFtest.xlsx <- data.frame(matrix(NA, 2, ncol(data)))
  names(result.ADFtest.xlsx) <- names(data)
  rownames(result.ADFtest.xlsx) <- c("t-value","lags")
  
  for(i in 1:ncol(data)){
    ADFtest.t <- ur.df(data[,i], type = "none", selectlags = selectlags)
    if(abs(ADFtest.t@teststat[1]) < abs(ADFtest.t@cval[1,3])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"")
      result.ADFtest.xlsx[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"")
    }
    if(abs(ADFtest.t@teststat[1]) >= abs(ADFtest.t@cval[1,3])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"$^{*}$")
      result.ADFtest.xlsx[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"*")
    }
    if(abs(ADFtest.t@teststat[1]) > abs(ADFtest.t@cval[1,2])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"$^{**}$")
      result.ADFtest.xlsx[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"**")
    }
    if(abs(ADFtest.t@teststat[1]) > abs(ADFtest.t@cval[1,1])){
      result.ADFtest[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"$^{***}$")
      result.ADFtest.xlsx[1, i] <- paste0(round(ADFtest.t@teststat[1],2),"***")
    }
    result.ADFtest[2, i] <- ADFtest.t@lags 
    result.ADFtest.xlsx[2, i] <- ADFtest.t@lags
  }
  return(list(result.ADFtest, result.ADFtest.xlsx))
}
#################################################################
# genFEVD for vecm using tsDyn library
#################################################################
genFEVD.VECM <- function (est, n.ahead, no.corr = F) {
  Phi <- tsDyn::irf(est, n.ahead = n.ahead, boot = F, ortho = F)
  Phi <- lapply(1:(n.ahead + 1), function(j) sapply(Phi$irf, 
                                                    function(i) i[j, ]))
  Sigma <- t(residuals(est)) %*% residuals(est)/nrow(residuals(est))
  if (no.corr) {
    Sigma <- diag(diag(Sigma))
  }
  denom <- diag(Reduce("+", lapply(Phi, function(i) i %*% 
                                     Sigma %*% t(i))))
  enum <- Reduce("+", lapply(Phi, function(i) (i %*% Sigma)^2))
  tab <- sapply(1:nrow(enum), function(j) enum[j, ]/(denom[j] * 
                                                       diag(Sigma)))
  tab <- t(apply(tab, 2, function(i) i/sum(i)))
  return(tab)
}
#################################################################
# booting genFEVD 
#################################################################
genFEVD.VECM.boot.official <- function (est, boot = TRUE, n.ahead, no.corr = F, ci , runs , seed, band){
  Phi <- c()
  #with or without tsDyn:: is the same
  Phi <- tsDyn::irf(est, n.ahead = n.ahead, boot = TRUE, ci = ci, runs = runs, seed = seed, impulse= NULL, response = NULL, ortho = F)
  if(band == "irf"){
    Phi <- lapply(1:(n.ahead + 1), function(j) sapply(Phi$irf, 
                                                      function(i) i[j, ]))
  }
  if(band == "lower"){
    Phi <- lapply(1:(n.ahead + 1), function(j) sapply(Phi$Lower, 
                                                      function(i) i[j, ]))
  }
  if(band == "upper"){
    Phi <- lapply(1:(n.ahead + 1), function(j) sapply(Phi$Upper, 
                                                      function(i) i[j, ]))
  }
  
  Sigma <- t(residuals(est)) %*% residuals(est)/nrow(residuals(est))
  if (no.corr) {
    Sigma <- diag(diag(Sigma))
  }
  denom <- diag(Reduce("+", lapply(Phi, function(i) i %*% 
                                     Sigma %*% t(i))))
  enum <- Reduce("+", lapply(Phi, function(i) (i %*% Sigma)^2))
  tab <- sapply(1:nrow(enum), function(j) enum[j, ]/(denom[j] * 
                                                       diag(Sigma)))
  tab <- t(apply(tab, 2, function(i) i/sum(i)))
  return(tab)
}
#################################################################
# process of genFEVD for vecm using tsDyn library
#################################################################
process.genFEVD.VECMorVAR <- function(data, group, start.point = "NULL", end.point = "NULL", n.ahead = 1, keep.vecm , rank){
  if(missing(keep.vecm)) {keep.vecm <- FALSE}
  if(missing(rank)){rank <- NULL}
  if(class(data)[[1]] == "data.frame"){
    if(missing(group)) {group <- names(data)[-1]}
    data <- data[, c("Date", group)] %>% na.omit
  if(start.point == "NULL") {start.point <- data$Date[1]}
  #if(start.point < data$Date[1]) {start.point <- data$Date[1]}
  if(end.point == "NULL") {end.point <- data$Date[nrow(data)]}
  data <- data[data$Date >= start.point & data$Date <= end.point, -1]
  }
  
  if(class(data)[[1]] == "xts"){
    if(missing(group)) {group <- names(data)}
    data <- data[, c(group)] %>% na.omit
    data <- data %>% na.omit
    if(start.point == "NULL") {start.point <- index(data)[1]}
    #if(start.point < data$Date[1]) {start.point <- data$Date[1]}
    if(end.point == "NULL") {end.point <- index(data)[nrow(data)]}
    data <- data[paste0(start.point,"/",end.point)]
  }
  
  n_groups <- length(group)
  #select lags
  lags <- VARselect(data, lag.max = 10, type = "const")$select
  K <- 2
  #K <- ifelse(ceiling(mean(lags)) - 1 >= 2, ceiling(mean(lags)) -1 , 2)
  #K <- ifelse(min(lags) - 1 >= 2, min(lags) - 1 , 2)
  #choose function according to number of variables to test cointegrating rank
    #test whether there is cointegration using rank.test.vecm in GVAR library
    try.error <- try(vecm.tsDyn <- VECM(data, lag=K, estim="ML"),silent = TRUE)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
    if(class(try.error)[1]=="try-error"){
      vecm.tsDyn <- VAR(data, p = K, type = "const")
      genFEVD.var <- genFEVD(vecm.tsDyn, n.ahead) %>% as.data.frame
      names(genFEVD.var) <- group
      modelclass <- "VARinL"
      v.rank <- c(0,0)
      print("Error! Run VAR instead.")
    }else{
      rank.test <- vecm.tsDyn
      # Maximum-likelihood test of the cointegrating rank.
      rank.eigen <- rank.test(vecm.tsDyn, cval = 0.01, type = "eigen")$r
      rank.trace <- rank.test(vecm.tsDyn, cval = 0.01, type = "trace")$r
      v.rank <- c(rank.eigen, rank.trace)
      names(v.rank) <- c("eigen", "trace")
      if(is.null(rank) | missing(rank)){
        rank <- floor((rank.eigen + rank.trace)/2)
      } else {
        rank <- rank
      }
      #rank <- 3
      
      # choose model according to rank of cointegration.
      if(rank > 0  & rank < n_groups) {
        vecm.tsDyn <- VECM(data, lag=K, estim="ML", r = rank)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
        #run generalized forecast error vector decomposition
        #genFEVD.var here is actually genFEVD for vecm model, don't want to change the name of variable for the purpose of convenience
        genFEVD.var <- genFEVD.VECM(vecm.tsDyn, n.ahead) %>% as.data.frame
        names(genFEVD.var) <- group
        modelclass <- "VECM"
        print("Rank is between 0 and ngroup. Run VECM.")
      } else if(rank == 0){
        #run a sastationary VAR(K−1) in differences.
        if(keep.vecm == TRUE){
          vecm.tsDyn <- VECM(data, lag=K, estim="ML", r = rank)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
          #run generalized forecast error vector decomposition
          #genFEVD.var here is actually genFEVD for vecm model, don't want to change the name of variable for the purpose of convenience
          genFEVD.var <- genFEVD.VECM(vecm.tsDyn, n.ahead) %>% as.data.frame
          names(genFEVD.var) <- group
          modelclass <- "VECM"
          print("keep using VECM")
        }else{
          g.data <- as.data.frame(data)
          vecm.tsDyn <- VAR(as.data.frame(lapply(g.data, log_GrowthRate)), p = K-1, type = "const")
          genFEVD.var <- genFEVD(vecm.tsDyn, n.ahead) %>% as.data.frame
          names(genFEVD.var) <- group
          modelclass <- "VARinD"
          print("Zero rank. There is no cointegration. Run VAR(K−1) in differences.")
        }
      } else if(rank == n_groups){
        if(keep.vecm == TRUE){
          vecm.tsDyn <- VECM(data, lag=K, estim="ML", r = rank-1)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
          #run generalized forecast error vector decomposition
          #genFEVD.var here is actually genFEVD for vecm model, don't want to change the name of variable for the purpose of convenience
          genFEVD.var <- genFEVD.VECM(vecm.tsDyn, n.ahead) %>% as.data.frame
          names(genFEVD.var) <- group
          modelclass <- "VECM"
          print("keep using VECM")
        }else{
          vecm.tsDyn <- VAR(data, p = K, type = "const")
          genFEVD.var <- genFEVD(vecm.tsDyn, n.ahead) %>% as.data.frame
          names(genFEVD.var) <- group
          modelclass <- "VARinL"
          print("Full rank. There is no cointegration. Run VAR(K) in level.")
        }
      }
    }
  return.list <- list(genFEVD.var, start.point, end.point, K, rank, modelclass, n_groups, lags, v.rank, vecm.tsDyn, data, group, rank.test)
  names(return.list) <- c("genFEVD.var", "start.point", "end.point", "K", "rank", "modelclass", "n_groups", "lags", "v.rank", "vecm.tsDyn", "data", "group", "rank.test")
  return(return.list)
}
#################################################################
# process of genFEVD for vecm using vars library (abandoned)
#################################################################
process.genFEVD.VECMorVAR.vars <- function(data, group, start.point = "NULL", end.point = "NULL", n.ahead){
  data <- data[, c("Date", group)] %>% na.omit
  if(start.point == "NULL") {start.point <- data$Date[1]}
  #if(start.point < data$Date[1]) {start.point <- data$Date[1]}
  if(end.point == "NULL") {end.point <- data$Date[nrow(data)]}
  data <- data[data$Date >= start.point & data$Date <= end.point, -1]
  
  n_groups <- length(group)
  #select lags
  lags <- VARselect(data, lag.max = 10, type = "const")$select
  K <- 2
  #K <- ifelse(ceiling(mean(lags)) - 1 >= 2, ceiling(mean(lags)) -1 , 2)
  #K <- ifelse(min(lags) - 1 >= 2, min(lags) - 1 , 2)
  #choose function according to number of variables to test cointegrating rank
  if(n_groups < 0){
    #test whether there is cointegration using ca.jo in urca library
    COINtest <- ca.jo(data, K, type = "eigen", ecdet = "const", spec = "longrun") # set ecdet as ‘const’ for constant term in cointegration 
    result.COINtest <- c()
    
    for(i in n_groups:1){
      result.COINtest[i] <- ifelse(abs(COINtest@teststat[i]) > abs(COINtest@cval[i,2]), "reject", "accept")#  5% significance level
      #print(result.COINtest[i])
    } 
    rank <- match(result.COINtest, "reject") %>% na.omit %>% sum
  }else{
    #test whether there is cointegration using rank.test.vecm in GVAR library
    vecm.tsDyn <- VECM(data, lag=K, estim="ML")#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
    
    # Maximum-likelihood test of the cointegrating rank.
    rank.eigen <- rank.test(vecm.tsDyn, cval = 0.05, type = "eigen")$r
    rank.trace <- rank.test(vecm.tsDyn, cval = 0.05, type = "trace")$r
    v.rank <- c(rank.eigen, rank.trace)
    names(v.rank) <- c("eigen", "trace")
    rank <- floor((rank.eigen + rank.trace)/2)
    
    # ca.jo can not caculate p value for more than 11 variables but still work
    COINtest <- ca.jo(data, K, type = "eigen", ecdet = "const", spec = "longrun") # set ecdet as ‘const’ for constant term in cointegration 
  }
  
  # choose model according to rank of cointegration.
  if(rank > 0  & rank < n_groups) {
    #run vec model
    vecm <- cajorls(COINtest, r = rank)
    #how to test whether the vecm model converge
    #sign(coefA(vecm))
    #sign(coefB(vecm))
    #convert vec model to var level model
    var.vecm <- vec2var(COINtest, r = rank)
    #run generalized forecast error vector decomposition
    genFEVD.var <- genFEVD(var.vecm, n.ahead) %>% as.data.frame
    names(genFEVD.var) <- group
    modelclass <- "VECM"
  } else if(rank == 0){
    #run a sastationary VAR(K−1) in differences.
    var <- VAR(as.data.frame(lapply(data, log_GrowthRate)), p = K-1, type = "const")
    genFEVD.var <- genFEVD(var, n.ahead) %>% as.data.frame
    names(genFEVD.var) <- group
    modelclass <- "VARinD"
    print("Zero rank. There is no cointegration. Run VAR(K−1) in differences.")
  } else if(rank == n_groups){
    var <- VAR(data, p = K, type = "const")
    genFEVD.var <- genFEVD(var, n.ahead) %>% as.data.frame
    names(genFEVD.var) <- group
    modelclass <- "VARinL"
    print("Full rank. There is no cointegration. Run VAR(K) in level.")
  }
  return.list <- list(genFEVD.var, start.point, end.point, K, rank, modelclass, n_groups, lags, v.rank, vecm)
  names(return.list) <- c("genFEVD.var", "start.point", "end.point", "K", "rank", "modelclass", "n_groups", "lags", "v.rank", "vecm")
  return(return.list)
}
#################################################################
# export result of process.genFEVD.VECMorVAR into table
#################################################################
result.export <- function(data, group.list, start.point = "NULL", end.point = "NULL", n.ahead){
  # define rsult matrix
  result.matrix <- data.frame(matrix(0, length(group.list), length(unique(unlist(group.list))) + 11))
  names(result.matrix) <- c(unlist(group.list) %>% unique, "CNH_DF", "CNY_NDF", "CNY_DF", "start.point", "end.point", "n.ahead", "lags", "rank", "n_groups", "class", "group.member")
  result.matrix$n.ahead <- n.ahead
  
  # run genFEVDvecm on group.list
  for(i in 1:length(group.list)){
    print(i)
    # define group.member
    group.member <- c("")
    for(g in 1:length(group.list[[i]])){
      group.member <- paste0(group.member, group.list[[i]][g], sep = " ")
    }
    result.matrix$group.member[i] <- group.member
    
    # run process.genFEVD.VECMorVAR
    result.genFEVD.vecm <- process.genFEVD.VECMorVAR(data, group = group.list[[i]], start.point, end.point, n.ahead)
    genFEVD.var.cny <- result.genFEVD.vecm$genFEVD.var[1,]
    ## record parameter
    result.matrix$lags[i] <- result.genFEVD.vecm$K
    result.matrix$rank[i] <- result.genFEVD.vecm$rank
    result.matrix$n_groups[i] <- result.genFEVD.vecm$n_groups
    result.matrix$class[i] <- result.genFEVD.vecm$modelclass
    result.matrix$start.point[i] <- result.genFEVD.vecm$start.point %>% as.character
    result.matrix$end.point[i] <- result.genFEVD.vecm$end.point %>% as.character
    ## caculate sum of same product
    locate.CNH_DF <- grep("CNH_DF",  colnames(result.genFEVD.vecm$genFEVD.var), value=F)
    locate.CNY_NDF <- grep("CNY_NDF", colnames(result.genFEVD.vecm$genFEVD.var), value = F)
    locate.CNY_DF <- grep("CNY_DF", colnames(result.genFEVD.vecm$genFEVD.var), value=F)
    
    result.matrix$CNH_DF[i] <- ifelse(length(locate.CNH_DF) == 0, 0, (genFEVD.var.cny[,locate.CNH_DF]) %>% sum)
    result.matrix$CNY_NDF[i] <- ifelse(length(locate.CNY_NDF) == 0, 0, genFEVD.var.cny[,locate.CNY_NDF] %>% sum)
    result.matrix$CNY_DF[i] <- ifelse(length(locate.CNY_DF) == 0, 0, genFEVD.var.cny[,locate.CNY_DF] %>% sum)
    
    ## place the result of each variable into the right location
    
    for(g in 1:length(group.list[[i]])){
      locate <- grep(group.list[[i]][g], colnames(result.matrix), value=F)
      result.matrix[i,locate] <- genFEVD.var.cny[g] 
    }
  }
  return(result.matrix)
}
#################################################################
# run process.genFEVD.VECMorVAR in interation or fix version
#################################################################
dyFEVD.cny <- function(data, group, n.ahead, mode, span){
  tic()
  n <- nrow(data) - span
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  locate.cny <- grep("CNYS",  group, value=F)
  AllInf <- list()
  row.cny <- as.data.frame(matrix(NA, dim(data)[1]-span, length(group)))
  
  if(class(data)[[1]] == "data.frame"){Date <- data$Date[-c(1:span)]}
  if(class(data)[[1]] == "xts"){Date <- index(data)[-c(1:span)]}
  start.point = "NULL"
  end.point = "NULL"
  if(mode == "fix"){
    for (i in 1:n) {
      AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(i:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead)
      row.cny[i,] <- AllInf[[i]]$genFEVD.var[locate.cny,]
      print(i)
      setTxtProgressBar(pb, i)
    }
    toc()  
    close(pb)
  }
  if(mode == "iteration"){
    for (i in 1:n) {
      AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(1:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead)
      row.cny[i,] <- AllInf[[i]]$genFEVD.var[locate.cny,]
      print(i)
      setTxtProgressBar(pb, i)
    }
    toc()  
    close(pb)
  }
  row.cny$"Date" <- Date 
  names(row.cny) <- c(group, "Date")
  return.list <- list(row.cny, AllInf, n.ahead, mode, span)
  names(return.list) <- c("row.cny","AllInf","n.ahead", "mode", "span")
  toc()   
  return(return.list)# AllInf is all information.
}
#################################################################
# run process.genFEVD.VECMorVAR in interation or fix version for boot
#################################################################
dyFEVD.cny.boot.official <- function(data, group, ci = 0.05, seed, runs, mode, span, n.ahead){
  tic()
  if(missing(span)){span <- 0}
  if(missing(group)){group <- names(data)}
  #there should be no NAs in data;should contain a Date column
  n <- nrow(data)
  pb <- txtProgressBar(min = 0, max = n-span, style = 3)
  locate.cny <- grep("CNYS",  group, value=F)
  AllInf <- list()
  row.cny.irf <- as.data.frame(matrix(NA, dim(data)[1]-span, length(group)))
  row.cny.lower <- as.data.frame(matrix(NA, dim(data)[1]-span, length(group)))
  row.cny.upper <- as.data.frame(matrix(NA, dim(data)[1]-span, length(group)))
  
  if(class(data)[1]=="data.frame"){Date <- data$Date[-c(1:span)]}
  if(class(data)[1]=="xts"){Date <- index(data)[-c(1:span)]}
  start.point = "NULL"
  end.point = "NULL"
  if(mode == "yearly"){
    row.cny.irf <- as.data.frame(matrix(NA, 1, length(group)))
    row.cny.lower <- as.data.frame(matrix(NA, 1, length(group)))
    row.cny.upper <- as.data.frame(matrix(NA, 1, length(group)))
    i <- 1
    AllInf[[i]] <- process.genFEVD.VECMorVAR(data, group, start.point = "NULL", end.point = "NULL", n.ahead)
    est <- AllInf[[i]]$vecm.tsDyn
    
    tab.irf <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs =runs, seed = seed, band = "irf")
    row.cny.irf[i,] <- tab.irf[locate.cny,]
    
    tab.lower <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs =runs, seed = seed, band = "lower")
    row.cny.lower[i,] <- tab.lower[locate.cny,]
    
    tab.upper <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs =runs, seed = seed, band = "upper")
    row.cny.upper[i,] <- tab.upper[locate.cny,]
    #print(i)
    setTxtProgressBar(pb, i)
    return.list <- rbind(row.cny.irf[1,],row.cny.lower[1,],row.cny.upper[1,])
    colnames(return.list) <- group
    rownames(return.list) <- c("irf","lower","upper")
    return(return.list)
  }
  if(mode == "fix"){
    for (i in 1:(n-span)) {
      AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(i:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead)
      est <- AllInf[[i]]$vecm.tsDyn
      
      tab.irf <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs =runs, seed = seed, band = "irf")
      row.cny.irf[i,] <- tab.irf[locate.cny,]
      
      tab.lower <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs =runs, seed = seed, band = "lower")
      row.cny.lower[i,] <- tab.lower[locate.cny,]
      
      tab.upper <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs =runs, seed = seed, band = "upper")
      row.cny.upper[i,] <- tab.upper[locate.cny,]
      #print(i)
      setTxtProgressBar(pb, i)
    }
  }
  if(mode == "iteration"){
    for (i in 1:n) {
      AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(1:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead)
      est <- AllInf[[i]]$vecm.tsDyn
      
      tab.irf <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs = runs, seed = seed, band = "irf")
      row.cny.irf[i,] <- tab.irf[locate.cny,]
      
      tab.lower <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs = runs, seed = seed, band = "lower")
      row.cny.lower[i,] <- tab.lower[locate.cny,]
      
      tab.upper <- genFEVD.VECM.boot.official(est, boot = TRUE, n.ahead = n.ahead, no.corr = F, ci = ci, runs = runs, seed = seed, band = "upper")
      row.cny.upper[i,] <- tab.upper[locate.cny,]
      #print(i)
      setTxtProgressBar(pb, i)
    }
  }
  
  row.cny.irf$"Date" <- Date 
  names(row.cny.irf) <- c(group, "Date")
  row.cny.lower$"Date" <- Date 
  names(row.cny.lower) <- c(group, "Date")
  row.cny.upper$"Date" <- Date 
  names(row.cny.upper) <- c(group, "Date")
  
  return.list <- list(row.cny.irf, row.cny.lower, row.cny.upper, AllInf, n.ahead, mode, span)
  names(return.list) <- c("row.cny.irf","row.cny.lower","row.cny.upper","AllInf","n.ahead", "mode", "span")
  toc()   
  return(return.list)# AllInf is all information.
  close(pb)
}
#################################################################
# dy.VECM.FEVD
#################################################################
dy.VECM.FEVD <- function(data, group, n.ahead, mode, span, keep.vecm, rank){
  tic()
  n <- nrow(data)
  pb <- txtProgressBar(min = 0, max = ifelse(span == "yearly", 1, n-span), 
                       style = 3)
  #there should be no NAs in data;should contain a Date column
  AllInf <- list()
  fevd.matrix <- list()
  if(missing(group)){group <- names(data)}
  if(missing(keep.vecm)){keep.vecm = FALSE}
  if(class(data)[[1]] == "data.frame"){Date <- data$Date[-c(1:span)]}
  if(class(data)[[1]] == "xts"){Date <- index(data)}
  start.point = "NULL"
  end.point = "NULL"
  
  if(span == "yearly"){
    AllInf[[1]] <- process.genFEVD.VECMorVAR(data, group, start.point = "NULL", end.point = "NULL", n.ahead = n.ahead, keep.vecm = keep.vecm,rank)
    fevd.matrix[[1]] <- AllInf[[1]]$genFEVD.var
    setTxtProgressBar(pb, 1)
  }else{
  if(mode == "fix"){
    for (i in 1:(n - span)) {
      AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(i:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead, keep.vecm = keep.vecm,rank)
      fevd.matrix[[i]] <- AllInf[[i]]$genFEVD.var
      setTxtProgressBar(pb, i)
    }
  }
  if(mode == "iteration"){
    for (i in 1:(n - span)) {
      AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(1:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead, keep.vecm = keep.vecm,rank)
      fevd.matrix[[i]] <- AllInf[[i]]$genFEVD.var
      setTxtProgressBar(pb, i)
    }
    
  }
  }
  return.list <- list(fevd.matrix, AllInf, n.ahead, mode, span, Date)
  names(return.list) <- c("fevd.matrix","AllInf","n.ahead", "mode", "span", "Date")
  toc()  
  close(pb)   
  return(return.list)# AllInf is all information.
}
#################################################################
# dy.VECM.GIR
#################################################################
dy.VECM.GIR <- function(data, group, n.ahead, mode, span, keep.vecm =FALSE,
                        rank = NULL#NULL or numberic
                        ){
  n.ahead <- n.ahead + 1
  tic()
  n <- nrow(data)
  pb <- txtProgressBar(min = 0, max = ifelse(span == "yearly", 1, n-span), 
                       style = 3)
  #there should be no NAs in data;should contain a Date column
  AllInf <- list()
  gir.matrix <- list()
  if(missing(group)){group <- names(data)}
  if(missing(keep.vecm)){keep.vecm == FALSE}
  if(class(data)[[1]] == "data.frame"){Date <- data$Date[-c(1:span)]}
  if(class(data)[[1]] == "xts"){Date <- index(data)}
  if(missing(rank)){rank <- NULL}
  if(missing(mode)){mode <- NULL}
  start.point = "NULL"
  end.point = "NULL"
  
  if(span == "yearly"){
    AllInf[[1]] <- process.genFEVD.VECMorVAR(data, group, start.point = "NULL", end.point = "NULL", n.ahead, keep.vecm = keep.vecm, rank = rank)
    if(class(AllInf[[1]]$vecm.tsDyn)[[1]]=="varest"){
      temp.var <- AllInf[[1]]$vecm.tsDyn
    }else{
      temp.var <- vec2var.tsDyn(AllInf[[1]]$vecm.tsDyn)
    }
    
    gir.matrix.temp <- irf_generalised(temp.var, n.ahead = n.ahead)
    gir.matrix[[1]] <- gir.matrix.temp[,,n.ahead]
    setTxtProgressBar(pb, 1)
  }else{
    if(mode == "fix"){
      for (i in 1:(n - span)) {
        AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(i:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead, keep.vecm = keep.vecm, rank = rank)
        if(class(AllInf[[i]]$vecm.tsDyn)[[1]]=="varest"){
          temp.var <- AllInf[[i]]$vecm.tsDyn
        }else{
          temp.var <- vec2var.tsDyn(AllInf[[i]]$vecm.tsDyn)
        }
        
        gir.matrix.temp <- irf_generalised(temp.var, n.ahead = n.ahead)
        gir.matrix[[i]] <- gir.matrix.temp[,,n.ahead]
        setTxtProgressBar(pb, i)
      }
    }
    if(mode == "iteration"){
      for (i in 1:(n - span)) {
        AllInf[[i]] <- process.genFEVD.VECMorVAR(data[c(1:(i+span-1)),], group, start.point = "NULL", end.point = "NULL", n.ahead, keep.vecm = keep.vecm, rank = rank)
        if(class(AllInf[[i]]$vecm.tsDyn)[[1]]=="varest"){
          temp.var <- AllInf[[i]]$vecm.tsDyn
        }else{
          temp.var <- vec2var.tsDyn(AllInf[[i]]$vecm.tsDyn)
        }
        gir.matrix.temp <- irf_generalised(temp.var, n.ahead = n.ahead)
        gir.matrix[[i]] <- gir.matrix.temp[,,n.ahead]      
        setTxtProgressBar(pb, i)
      }
    }
  }
  return.list <- list(gir.matrix, AllInf, n.ahead, mode, span, Date)
  names(return.list) <- c("gir.matrix","AllInf","n.ahead", "mode", "span", "Date")
  toc()  
  close(pb)   
  return(return.list)# AllInf is all information.
}
#################################################################
# dygraph
#################################################################
dygraph.NewRMB <- function(dy.data,dy.main,color,label.dyAxis,width.dyLegend){
  if(missing(label.dyAxis)){label.dyAxis <- " "}
  if(missing(width.dyLegend)){width.dyLegend <- 1500}
  
  #color <- c("black","gray",colorRampPalette(c("red", "white"))(5)[-5], colorRampPalette(c("blue", "white"))(5)[-5], colorRampPalette(c("green", "white"))(5)[-5])
  #color <- c(RColorBrewer::brewer.pal(9, "Set1"),RColorBrewer::brewer.pal(8, "Set2"),RColorBrewer::brewer.pal(12, "Set3"))
  dygraph(data = dy.data, main = dy.main) %>%
    dyRangeSelector(dateWindow = c("2011-10-20", "2017-12-31"), height = 20)  %>%
    
    dyAxis("y", label = label.dyAxis) %>%#valueRange = 
    dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", colors = color, strokeWidth = 2) %>%
    dyLegend(show = "auto", hideOnMouseOut = TRUE, width = width.dyLegend, labelsSeparateLines = FALSE) %>%
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.7,
                hideOnMouseOut = TRUE,
                highlightSeriesOpts = list(strokeWidth = 2.5)) %>%
    
    dyEvent("2008-9-14", "2008.9.14:雷曼兄弟破产    ", labelLoc = "top") %>%
    dyEvent("2008-11-25", "2008.11.25:美国量化宽松Q1    ", labelLoc = "top") %>%
    dyEvent("2010-6-19", "2010.6.19:增强人民币汇率弹性    ", labelLoc = "top") %>%
    dyEvent("2010-11-4", "2010.11.4:美国量化宽松Q2    ", labelLoc = "top") %>%
    dyEvent("2012-4-16", "2012.4.16:人民币汇率浮动区间从0.5%扩大到1%    ", labelLoc = "top") %>%
    dyEvent("2012-5-29", "2012.5.29:发展人民币对日元直接交易    ", labelLoc = "top") %>%
    dyEvent("2012-9-14", "2012.9.14:美国量化宽松Q3    ", labelLoc = "top") %>%
    #dyEvent("2012-12-7", "2012.12.7:《合格境外机构投资者境内证券投资外汇管理规定》    ", labelLoc = "top") %>%
    dyEvent("2012-12-13", "2012.12.13:美国量化宽松Q4    ", labelLoc = "top") %>%
    #dyEvent("2014-1-21", "2014.1.21:《外债转贷款外汇管理规定》    ", labelLoc = "top") %>%
    #dyEvent("2014-2-18", "2014.2.18:上海自贸区启动人民币跨境支付业务    ", labelLoc = "top") %>%
    dyEvent("2014-3-17", "2014.3.17:人民币汇率浮动区间由1%扩大到2%    ", labelLoc = "top") %>%
    #dyEvent("2014-5-19", "2014.5.19:《跨境担保外汇管理规定》    ", labelLoc = "top") %>%
    #dyEvent("2015-1-26", "2015.1.26:进一步推进行政审批改革 完善保险业务外汇管理    ", labelLoc = "top") %>%
    dyEvent("2015-8-11", "2015.8.11:811汇改    ", labelLoc = "top") %>%
    dyEvent("2015-12-11", "2015.12.11:发布CFETS人民币汇率指数    ", labelLoc = "top") %>%
    #dyEvent("2015-12-16", "2015.12.16:美联储加息    ", labelLoc = "top") %>%
    #dyEvent("2016-2-3", "2016.2.3:《合格境外机构投资者境内证券投资外汇管理规定》    ", labelLoc = "top") %>%
    #dyEvent("2016-8-29", "2016.8.29: 关于人民币合格境外机构投资者境内证券投资管理有关问题的通知》    ", labelLoc = "top") %>%
    #dyEvent("2016-10-1", "2016.10.1:RMB加入SDR    ", labelLoc = "top") %>%
    dyEvent("2016-12-29", "2016.12.9:调整CFETS指数货币篮子    ", labelLoc = "top") %>%
    #dyEvent("2017-1-24", "2017.1.24:中国央行时隔六年首次提升MLF政策利率10个基点    ", labelLoc = "top") %>%
    #dyEvent("2017-3-15", "2017.3.15:美联储宣布加息25个基点", labelLoc = "top") %>%
    dyEvent("2017-3-20", "2017.3.20:香港交易所推出人民币货币期权    ", labelLoc = "top") %>%
    dyEvent("2017-5-26", "2017.5.26:逆周期因子    ", labelLoc = "top") %>%
    dyEvent("2017-9-11", "2017.9.11:外汇准备金率从20%调整为0    ", labelLoc = "top")
  }
#################################################################
# dygraph
#################################################################
dygraph.horizon <- function(dy.data,dy.main,color, ylabel){
  #color <- c("black","gray",colorRampPalette(c("red", "white"))(5)[-5], colorRampPalette(c("blue", "white"))(5)[-5], colorRampPalette(c("green", "white"))(5)[-5])
  #color <- c(RColorBrewer::brewer.pal(9, "Set1"),RColorBrewer::brewer.pal(8, "Set2"),RColorBrewer::brewer.pal(12, "Set3"))
  dygraph(data = dy.data, main = dy.main) %>%
    dyRangeSelector(height = 20)  %>%
    
    dyAxis("y", label = ylabel) %>%#valueRange = 
    dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", colors = color, strokeWidth = 2) %>%
    dyLegend(show = "auto", hideOnMouseOut = TRUE, width = 800, labelsSeparateLines = FALSE) %>%
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.7,
                hideOnMouseOut = TRUE,
                highlightSeriesOpts = list(strokeWidth = 2.5))
    
    #dyEvent("2008-9-14", "2008.9.14:雷曼兄弟破产", labelLoc = "top") %>%
}
#################################################################
# dygraph horizon in solid color
#################################################################
dygraph.horizon.solid <- function(dy.data, dy.main, color, ylabel, group, need.nrow = TRUE, fillAlpha = 1){
  #order is actually the name sequence of the dy.data
  #color <- c("black","gray",colorRampPalette(c("red", "white"))(5)[-5], colorRampPalette(c("blue", "white"))(5)[-5], colorRampPalette(c("green", "white"))(5)[-5])
  #color <- c(RColorBrewer::brewer.pal(9, "Set1"),RColorBrewer::brewer.pal(8, "Set2"),RColorBrewer::brewer.pal(12, "Set3"))
  if(missing(group)){group = names(dy.data)[-1]}
  if(missing(need.nrow)){need.nrow == TRUE}
  dy.data <- dy.data[, group]
  for (i in 2:ncol(dy.data)) {
    dy.data[,i] <- dy.data[, i] + dy.data[,i-1]
  }
  if(need.nrow == TRUE){dy.data <- cbind(c(1:nrow(dy.data)), dy.data)}
  if(missing(fillAlpha)){fillAlpha <- 1}
  dygraph(data = dy.data, main = dy.main) %>%
    dyRangeSelector(height = 20)  %>%
    dyAxis("y", label = ylabel) %>%#valueRange = 
    dyOptions(axisLineWidth = 1.5, axisLineColor = "black", gridLineColor = "lightblue", colors = color, strokeWidth = 0.5) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = fillAlpha) %>%
    dyLegend(show = "auto", hideOnMouseOut = TRUE, width = 1200, labelsSeparateLines = FALSE) %>%
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.7,
                hideOnMouseOut = TRUE,
                highlightSeriesOpts = list(strokeWidth = 2.5))
  #dyEvent("2008-9-14", "2008.9.14:雷曼兄弟破产", labelLoc = "top") %>%
}
#################################################################
# Function to cut your data, and assign colour to each range
#################################################################
ColorRange <-  function(x, color){
  cut(x, seq(from = 0, to = 100, by = 10),
      labels <- paste0(color,"!", seq(from = 10, to = 100, by = 10), "!white"),
      include.lowest = FALSE, right = TRUE)
} 
#################################################################
# tab.fevd.cny
#################################################################
tab.fevd.cny <- function(tab, file.path, tab.caption, tab.label){
  table.drops <- c("start.point", "end.point", "n.ahead", "lags", "n_groups", "class", "group.member")
  group.red <- c("CNYS", "CNHS", "CNH_DF", "CNY_NDF", "CNY_DF")
  group.blue <- c("CNH_DF_1M", "CNH_DF_3M", "CNH_DF_6M", "CNH_DF_1Y", "CNY_NDF_1M", "CNY_NDF_3M", "CNY_NDF_6M", "CNY_NDF_1Y", "CNY_DF_1M", "CNY_DF_3M", "CNY_DF_6M", "CNY_DF_1Y")
  

  tab <- tab[, !(names(tab) %in% table.drops)]
  tab[tab == 0] <- NA
  tab[,-ncol(tab)] <- tab[,-ncol(tab)] * 100
  
  tab <- round(tab, 0)
  #tab[,ncol(tab)] <- round(tab[,ncol(tab)], 0)
  tab[,ncol(tab)] <- as.character(tab[,ncol(tab)])
  tab.excel <- tab
  tab.excel[is.na(tab)]=""
  write.xlsx(tab.excel, file = paste0("latex/excel/",gsub("tab:", "", tab.label), ".xlsx"), row.names = FALSE)
 
  tab[group.red] <- lapply(tab[group.red], function(x)
    paste0("\\cellcolor{", ColorRange(x, color = "red"), "}", x))
  tab[group.blue] <- lapply(tab[group.blue], function(x)
    paste0("\\cellcolor{", ColorRange(x, color = "blue"), "}", x))
  tab[tab == "\\cellcolor{NA}NA"] <- "\\cellcolor{white}"
  tab[tab == "\\cellcolor{NA}0"] <- "\\cellcolor{white}0.00"
  
  tab <- xtable(tab,
                caption = tab.caption,
                label = tab.label
  )
  names(tab) <- paste0("$",names(tab),"$")
  align(tab) <- "lllllllllllllllllll"
  addtorow <- list(pos = list(0,0),
                   command = c(" &   &\\multicolumn{4}{c}{$CNH\\_DF$} & \\multicolumn{4}{c}{$CNY\\_NDF$}& \\multicolumn{4}{c}{$CNY\\_DF$} & & & &\\\\\n",
                               "$CNYS$ &  $CNHS$ & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y & CNH\\_DF & CNY\\_NDF & CNY\\_DF & Rank\\\\\n"
                   )
  ) 
  print(tab, 
        file = file.path,
        sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
        caption.placement = "top",
        comment = "TRUE",
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        floating = TRUE, 
        floating.environment = "sidewaystable"
  )
}
#################################################################
# FEVDhorizon
#################################################################
FEVDhorizon <- function(data, group, start.point = "NULL", end.point = "NULL", v.n.ahead){
  tic()
  horizon <- c()
  for (i in v.n.ahead) {
    temp <- process.genFEVD.VECMorVAR(data, group, start.point, end.point, n.ahead = v.n.ahead[i])
    fevd.cny <- temp$genFEVD.var[1,]
    horizon <- rbind(horizon, fevd.cny)
  }
  names(horizon) <- group
  horizon <- cbind(v.n.ahead, horizon)
  toc()
  return(horizon)
}
#################################################################
# is.significant
#################################################################
significance <- function(x){
  if(x >= 0.1){
    sgnf <- ""
  }
  if(x < 0.1){
    sgnf <- "*"
  }
  if(x < 0.05){
    sgnf <- "**"
  }
  if(x < 0.01){
    sgnf <- "***"
  }
  return(sgnf)
}
#################################################################
# Parameter Estimation Results for the Vector Error Correction Model
#################################################################
table.parameter <- function(model, tab.label, call.euqation,digits,type.export){
  if(missing(call.euqation)){call.euqation <- FALSE}
  if(missing(digits)){digits <- 2}
  if(missing(type.export)){type.export <- F}
  
  process.vecm <- model
  rank <- process.vecm$rank
  vecm <- process.vecm$vecm.tsDyn
  loglikelihood.vecm <- logLik(vecm)
  coef.vecm <- vecm$coefficients %>% t
  
  summary.vecm <- summary(vecm)
  pvalue.vecm <- summary.vecm$Pvalues
  sgnf <- apply(pvalue.vecm,1:2,significance)
  coef_sgnf <- data.frame(matrix(NA, (rank+1), length(group)))
  for (i in 1:(rank+1)){
    for (j in 1:length(group)) {
      temp <- format(round(coef.vecm[i,j], digits=digits), nsmall = digits)
      coef_sgnf[i,j] <- paste0(temp, sgnf[i,j])
    }
  }
  colnames(coef_sgnf) <- group
  rownames(coef_sgnf) <- c(paste0("alpha",c(1:(rank))),"constant") 
  coef_sgnf <- rbind(coef_sgnf[(rank+1),], coef_sgnf)[-(rank+2),]
  
  if(call.euqation == F){
    beta <- coefB(vecm, normalize = TRUE) %>% t
    #beta[abs(beta)<0.000001] <- ""
    beta <- format(round(beta, digits=digits), nsmall = digits)
    
    colnames(beta) <- group
    rownames(beta) <- paste0("CI",c(1:(rank)))
    tab <- rbind(coef_sgnf, beta)
    tab <- xtable(tab,
                  caption = paste("Parameter Estimation Results for the Vector Error Correction Model", tab.label, sep = " "),
                  label = paste0("tab:parameter.", tab.label)
    )
    #require(knitr)
    align(tab) <- paste0(rep("l",length(group)+1), collapse = "")
    addtorow <- list(pos = list(0,0),
                     command = c("& &   &\\multicolumn{4}{c}{$CNH\\_DF$} & \\multicolumn{4}{c}{$CNY\\_NDF$}& \\multicolumn{4}{c}{$CNY\\_DF$}\\\\\n",
                                 "& $CNYS$ &  $CNHS$ & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y\\\\\n")) 
    print(tab, 
          file = paste0("latex/table/parameter.",tab.label,".tex"),
          sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
          caption.placement = "top",
          comment = "TRUE",
          include.rownames = TRUE,
          include.colnames = FALSE,
          add.to.row = addtorow,
          floating = TRUE, 
          floating.environment = "sidewaystable",
          type = "latex"
    )
  }
  if(call.euqation == T){
    ci.vector <- data.frame(matrix(NA, rank, length(group)))
    beta <- coefB(vecm, normalize = TRUE) %>% t
    beta[beta<0.00001] <-0
    temp.euqation <- c()
    c.beta <- format(round(beta, digits=digits), nsmall = digits)
    for (i in 1:(rank)){
      temp.euqation <- c()
      for (j in 1:length(group)) {
        if(beta[i,j]!=0){
          if(j == i){
            temp.euqation <- paste0(temp.euqation, group[j])    
          }
          if(j > 1){
            if(beta[i,j]==1){
              temp.euqation <- paste0(temp.euqation, "+", group[j])  
            }else{
              temp.euqation <- paste0(temp.euqation, "+",c.beta[i,j], "*", group[j])  
            }
          }
        }
      }
      ci.vector[i,1] <- temp.euqation
    }
    colnames(ci.vector) <- group
    rownames(ci.vector) <- paste0("CI",c(1:(rank)))
    
    if(type.export == "xlsx"){
      tab <- rbind(coef_sgnf, ci.vector)
      write.xlsx(tab, file = paste0("latex/table/parameter.",tab.label,".xlsx"), row.names = TRUE)}
    if(0){#type.export == "latex"
      tab <- coef_sgnf
      tab <- xtable(tab,
                    caption = paste("Parameter Estimation Results for the Vector Error Correction Model", tab.label, sep = " "),
                    label = paste0("tab:parameter.", tab.label)
      )
      #require(knitr)
      align(tab) <- paste0(rep("l",length(group)+1), collapse = "")
      addtorow <- list()
      addtorow$pos <- list(0,0,5,5,5,5)
      addtorow$command  <-c("& &   &\\multicolumn{4}{c}{$CNH\\_DF$} & \\multicolumn{4}{c}{$CNY\\_NDF$}& \\multicolumn{4}{c}{$CNY\\_DF$}\\\\\n",
                            "& $CNYS$ &  $CNHS$ & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y\\\\\n")
for (i in 1:rank) {
  addtorow$command <- c(addtorow$command,paste0("&&\\multicolumn{14}{l}{",ci.vector[i,1],"}\\\\\n"))
}
      print(tab, 
            file = paste0("latex/table/parameter.",tab.label,".tex"),
            sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
            caption.placement = "top",
            comment = "TRUE",
            include.rownames = TRUE,
            include.colnames = FALSE,
            add.to.row = addtorow,
            floating = TRUE, 
            floating.environment = "sidewaystable",
            type = "latex"
      )
    }
  }


  return(tab)
}
#################################################################
# cointegration test in latex
#################################################################
table.CItest <- function(model, tab.label){
data <- model$data
vecm.tsDyn <- model$rank.test
#vecm.tsDyn <- VECM(data, lag = K, estim="ML",include = c("const"))
COIN.rank.test <- rank.test(vecm.tsDyn)
COIN.res <- COIN.rank.test$res_df #COIN.eigen$r #COIN.trace$r #COIN.eigen$pval #COIN.trace$pval
table.n <- nrow(COIN.res)
K <- model$K
COIN.AIC <- rank.select(data, include = c("none"))$AICs[1:table.n, K] %>% round(0)
COIN.BIC <- rank.select(data, include = c("none"))$BICs[1:table.n, K] %>% round(0)

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

table.COINtest <- xtable(table.COINtest, caption = paste("The Determination of Cointegration Ranks", tab.label,sep = " "),
                         label = paste0("tab:COINtest", tab.label)) 

align(table.COINtest) <- paste0(rep("l",9), collapse = "")
print(table.COINtest, 
      file=paste0("latex/table/COINtest", tab.label, ".tex"), 
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
write.xlsx(table.COINtest.xlsx, file = paste0("latex/excel/COINtest", tab.label,".xlsx"), row.names = TRUE)
}
#################################################################
#convert vecm to var of in library tsDyn
#################################################################
vec2var.tsDyn <- function(x){
  
  model <- if(inherits(x,"VECM")) "VECM" else "VAR"
  co <- coef(x)
  lag <- ifelse(model=="VECM",x$lag+1, x$lag)
  K <- x$k
  include <- x$include
  
  ## VECM case: 
  if(model=="VECM"){
    LRinclude <- x$model.specific$LRinclude
    if(LRinclude!="none"){
      if(LRinclude=="const"){
        include <- "const"
      } else if(LRinclude=="trend"){
        include <- if(include=="const") "both" else "trend"
      } else if(LRinclude=="both"){
        include <- "both"
      }
    }
  }
  
  ## Take vec2var representation for VECMs
  if(model=="VECM") {
    co <- VARrep(x)
  }
  rownames(co) <- gsub("Equation ", "", rownames(co))
  colnames(co) <- gsub(" -([0-9]+)","\\.l\\1", colnames(co))
  colnames(co) <- gsub("Intercept","constant", colnames(co))
  
  ## detcoeffs
  detcoeffs <- co[,grep("constant|Trend", colnames(co)), drop=FALSE]
  
  ## A
  A <- list()
  for(i in 1:lag) A[[i]] <- co[,grep(paste("\\.l", i, sep=""), colnames(co)), drop=FALSE]
  names(A) <- paste("A", 1:lag, sep="")
  
  ## Rank
  rank <- if(model=="VECM") x$model.specific$r else K
  
  ## vecm
  ecdet <- if(model=="VECM") x$model.specific$LRinclude else "none"
  aChar <- "fakeChar"
  vecm<- new("ca.jo", season = NULL, dumvar=NULL, ecdet=ecdet,lag=as.integer(lag),spec="transitory", lambda=aChar)
  
  ## datamat
  if(model=="VAR"){
    datamat <- as.matrix(tail(as.data.frame(x$model),-lag))
  } else {
    newx <- lineVar(x$model[,1:K], lag=lag, include=include)
    datamat <- as.matrix(tail(as.data.frame(newx$model),-lag))
  }
  colnames(datamat) <- gsub(" -([0-9]+)","\\.l\\1", colnames(datamat))
  colnames(datamat) <- gsub("Intercept","constant", colnames(datamat))
  
  ## residuals
  resids <- residuals(x)
  colnames(resids) <- paste("resids of", colnames(resids))
  ## Return:
  result <- list(deterministic = detcoeffs, A = A, p = lag, K = K, y = as.matrix(x$model[,1:x$k]), obs = x$t, totobs = 
                   x$T, call = match.call(), vecm = vecm, datamat = datamat, resid = resids, r = rank)
  
  class(result) <- "vec2var"
  return(result)   
  
}
#################################################################
#convert vecm to var of in library tsDyn
#################################################################
tab.boot.fevd <- function(tab,digits=1){
  tab <- tab * 100
  tab[c("lower","upper"),] <- apply(tab[c("lower","upper"),], MARGIN = 2, sort)
  tab <- format(round(tab, digits=digits), nsmall = digits)
  tab <- rbind(tab,paste0("(",tab["lower",],",",tab["upper",],")"))
  rownames(tab)[4]<-"ci"
  return.list <- tab[c(1,4),]
  return(return.list )
}
#################################################################
# export.boot.tab
#################################################################
export.boot.tab <- function(data, v.horizon, tab.label, title, rans = 100, digits =2){
  l.boot.fevd <- list()
  l.ci.fevd <- list()
  Horizon <- c()
  for (i in 1:length(v.horizon)) {
    n.ahead <- v.horizon[i]
    l.boot.fevd[[i]] <- dyFEVD.cny.boot.official(data, group, ci = 0.01, seed = 99, runs=runs, mode = "yearly", n.ahead=n.ahead)
    l.ci.fevd[[i]] <- tab.boot.fevd(l.boot.fevd[[i]], digits = digits)
    Horizon <- c(Horizon,v.horizon[i],"")
    print(i)
  }
  require(abind)
  tab.ci.fevd <- abind(l.ci.fevd,along=1)
  tab.ci.fevd <- cbind(Horizon, tab.ci.fevd)
  rownames(tab.ci.fevd) <- NULL
  
  #excel
  write.xlsx(tab.ci.fevd, file = paste0("latex/excel/boot.",tab.label,".xlsx"), row.names = F)
  title <- "test"
  tab.ci.fevd <- xtable(tab.ci.fevd, caption = title,
                        label = paste0("tab:",tab.label))
  align(tab.ci.fevd) <- paste0(rep("c",dim(tab.ci.fevd)[2]+1), collapse = "")
  addtorow <- list(pos = list(0,0),
                   command = c("Horizon& &   &\\multicolumn{4}{c}{$CNH\\_DF$} & \\multicolumn{4}{c}{$CNY\\_NDF$}& \\multicolumn{4}{c}{$CNY\\_DF$}\\\\\n",
                               "& $CNYS$ &  $CNHS$ & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y & 1M & 3M & 6M & 1Y\\\\\n")) 
  print(tab.ci.fevd, 
        file = paste0("latex/table/boot.",tab.label,".tex"), 
        sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
        caption.placement = "top",
        comment = "TRUE",
        floating = TRUE, 
        floating.environment = "sidewaystable",
        include.rownames=FALSE,
        include.colnames=FALSE,
        add.to.row = addtorow
  )
}
#################################################################
# export.boot.tab
#################################################################
vec2var.tsDyn <- function(x){
  
  model <- if(inherits(x,"VECM")) "VECM" else "VAR"
  co <- coef(x)
  lag <- ifelse(model=="VECM",x$lag+1, x$lag)
  K <- x$k
  include <- x$include
  
  ## VECM case: 
  if(model=="VECM"){
    LRinclude <- x$model.specific$LRinclude
    if(LRinclude!="none"){
      if(LRinclude=="const"){
        include <- "const"
      } else if(LRinclude=="trend"){
        include <- if(include=="const") "both" else "trend"
      } else if(LRinclude=="both"){
        include <- "both"
      }
    }
  }
  
  ## Take vec2var representation for VECMs
  if(model=="VECM") {
    co <- VARrep(x)
  }
  rownames(co) <- gsub("Equation ", "", rownames(co))
  colnames(co) <- gsub(" -([0-9]+)","\\.l\\1", colnames(co))
  colnames(co) <- gsub("Intercept","constant", colnames(co))
  
  ## detcoeffs
  detcoeffs <- co[,grep("constant|Trend", colnames(co)), drop=FALSE]
  
  ## A
  A <- list()
  for(i in 1:lag) A[[i]] <- co[,grep(paste("\\.l", i, sep=""), colnames(co)), drop=FALSE]
  names(A) <- paste("A", 1:lag, sep="")
  
  ## Rank
  rank <- if(model=="VECM") x$model.specific$r else K
  
  ## vecm
  ecdet <- if(model=="VECM") x$model.specific$LRinclude else "none"
  aChar <- "fakeChar"
  vecm<- new("ca.jo", season = NULL, dumvar=NULL, ecdet=ecdet,lag=as.integer(lag),spec="transitory", lambda=aChar)
  
  ## datamat
  if(model=="VAR"){
    datamat <- as.matrix(tail(as.data.frame(x$model),-lag))
  } else {
    newx <- lineVar(x$model[,1:K], lag=lag, include=include)
    datamat <- as.matrix(tail(as.data.frame(newx$model),-lag))
  }
  colnames(datamat) <- gsub(" -([0-9]+)","\\.l\\1", colnames(datamat))
  colnames(datamat) <- gsub("Intercept","constant", colnames(datamat))
  
  ## residuals
  resids <- residuals(x)
  colnames(resids) <- paste("resids of", colnames(resids))
  ## Return:
  result <- list(deterministic = detcoeffs, A = A, p = lag, K = K, y = as.matrix(x$model[,1:x$k]), obs = x$t, totobs = 
                   x$T, call = match.call(), vecm = vecm, datamat = datamat, resid = resids, r = rank)
  
  class(result) <- "vec2var"
  return(result)   
  
}

