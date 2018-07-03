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
  
  file.name <- paste0(set, "_", inclusion.edgecov)
  
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
      if(!is.na(match("rolling",inclusion))){
        temp.rolling <- rolling[[i]][1:nnode,1:nnode] * (10^scale)  ;#temp.ON <- apply(temp.ON, c(1,2), round);
        diag(temp.rolling) <- 0;temp.rolling[is.na(temp.rolling)] <- 0
        if(!is.na(match("rolling",trans))){temp.rolling <- t(temp.rolling)}
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
        file.path<-paste0("data/Rdata/", file.name,".Rdata")
        save(coef.ergm.l,p.ergm.l,std.ergm.l,file = file.path)
      }
    }
  }
  close(pb)
  
  if(csv){
    file.path<-paste0("data/Rdata/", file.name,".Rdata")
    save(coef.ergm.l,p.ergm.l,std.ergm.l,file = file.path)
  }
  
  #dygraph###########################################################################################
  # only one varialbe
  if(fig){
    if(n.inclusion.edgecov == 1){
      dy.coef <- cbind(unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(coef.ergm.l),unlist(p.ergm.l)) %>% as.data.frame
      names(dy.coef) <- c("coef0.01","coef0.05","coef0.1","p-value")
      
      dy.coef$coef0.01[dy.coef$"p-value" > 0.01 |is.na(dy.coef$"p-value")] <- NA
      dy.coef$coef0.05[dy.coef$"p-value" > 0.05 | dy.coef$"p-value" < 0.01|is.na(dy.coef$"p-value")] <- NA
      dy.coef$coef0.1[dy.coef$"p-value" > 0.1 | dy.coef$"p-value" < 0.05|is.na(dy.coef$"p-value")] <- NA
      dy.coef <- xts(dy.coef, as.Date(Date, format='%Y-%m-%d'))
      dy.coef <- merge(dy.coef, is.higher$crisis.sma20)
      dy.coef <- apply(X = dy.coef,MARGIN = 2,FUN = replace.na.near)
      dy.coef <- xts(dy.coef, as.Date(index(is.higher), format='%Y-%m-%d'))
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
        dy.coef <- merge(dy.coef, is.higher$crisis.sma20)
        dy.coef <- apply(X = dy.coef,MARGIN = 2,FUN = replace.na.near)
        dy.coef <- xts(dy.coef, as.Date(Date, format='%Y-%m-%d'))
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