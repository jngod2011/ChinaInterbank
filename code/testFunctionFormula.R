# test formula part
#####################################################
test.formula <- function(
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
  inclusion.nodesqrtcovar = NULL#NULL or TRUE
){
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
      inclusion <- c(inclusion, paste0("I.",inclusion.absdiff))
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
  print(ergm.formula)
  print(n.inclusion)
  #print(!missing(inclusion.edgecov) & inclusion.edgecov != "NULL")
  #print(!missing(inclusion.edgecov))
}


inclusion.edgecov <- c("ON", "W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.nodecov <- NULL
inclusion.nodeocov<- NULL
inclusion.nodeicov<- NULL
inclusion.absdiff<- NULL
inclusion.cyclicalweights<- T
inclusion.mutual<- T
inclusion.nodecovar<- T
inclusion.nodecovar<- T
inclusion.nodeicovar<- T
inclusion.nodeocovar<- T
inclusion.nodeisqrtcovar<- T
inclusion.nodeosqrtcovar <- T
test.formula(inclusion.edgecov = inclusion.edgecov,
     inclusion.nodecov = inclusion.nodecov,
     inclusion.nodeocov = inclusion.nodeocov,
     inclusion.nodeicov = inclusion.nodeicov,
     inclusion.absdiff = inclusion.absdiff,
     inclusion.cyclicalweights = inclusion.cyclicalweights,
     inclusion.mutual = inclusion.mutual,
     inclusion.nodecovar = inclusion.nodecovar,
     inclusion.nodeicovar = inclusion.nodeicovar,
     inclusion.nodeocovar = inclusion.nodeocovar,
     inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar,
     inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar,
     inclusion.nodesqrtcovar = inclusion.nodesqrtcovar
     )

test.formula(inclusion.nodecov = inclusion.nodecov,
          
             inclusion.cyclicalweights = inclusion.cyclicalweights,
             inclusion.mutual = inclusion.mutual,
             inclusion.nodecovar = inclusion.nodecovar
            
)

# test ergm function
# transpose the matrix doesn't make much difference
#####################################################
inclusion.edgecov <- c("ON", "W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.nodecov <- c("NULL")
inclusion.nodeocov <- c("NULL")
inclusion.nodeicov <- c("ass")
trans <- NULL
test.ergm1 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:2], set = set, 
                                      inclusion.edgecov = inclusion.edgecov,
                                      inclusion.nodecov = inclusion.nodecov,
                                      inclusion.nodeocov = inclusion.nodeocov,
                                      inclusion.nodeicov = inclusion.nodeicov,
                                      Date = y.period[2:3], is.higher = is.higher,
                                      wind = raw.wind[3:4],
                                      csv = FALSE, fig = FALSE, tab = TRUE, trans = trans)
dyErgm.result <- test.ergm1;dyErgm.result$scale;
test1<- dyErgm.result$tab.ergm$coef
test1[dyErgm.result$tab.ergm$pvalue > 0.1] <- NA;
View(test1)


inclusion.edgecov <- c("ON", "W1","W2","M1","M3","M6","M9","Y1","loan")
inclusion.nodecov <- c("NULL")
inclusion.nodeocov <- c("NULL")
inclusion.nodeicov <- c("ass")
trans <- c("y","ON", "W1","W2","M1","M3","M6","M9","Y1","loan")
test.ergm2 <- dyCoefErgm.yearly(data = vecm.myl.gir[1:2], set = set, 
                                inclusion.edgecov = inclusion.edgecov,
                                inclusion.nodecov = inclusion.nodecov,
                                inclusion.nodeocov = inclusion.nodeocov,
                                inclusion.nodeicov = inclusion.nodeicov,
                                Date = y.period[2:3], is.higher = is.higher,
                                wind = raw.wind[3:4],
                                csv = FALSE, fig = FALSE, tab = TRUE, trans = trans)
dyErgm.result <- test.ergm2;dyErgm.result$scale;
test2<- dyErgm.result$tab.ergm$coef
test2[dyErgm.result$tab.ergm$pvalue > 0.1] <- NA;
View(test2)






