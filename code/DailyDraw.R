###############################################################
# load
###############################################################
set <- "VECMyx(t-1)1"
load(file = paste0("data/Rdata/Daily/", set, ".Rdata"))
range <- 10
###############################################################
# load
###############################################################
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
data <- read.csv(file = "data/bank10/ForestData.csv")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
Date <- index(data) %>% as.character;Date <- Date[-c(1:251)]
is.higher <- data[,n.is.higher][-c(1:251),]
n.inclusion.edgecov <- length(coef.ergm.l)
fig <- TRUE

###############################################################
# draw
###############################################################
if(fig){
  if(n.inclusion.edgecov == 1){
    temp.coef <- unlist(coef.ergm.l)
    dy.p <- unlist(p.ergm.l)
    dy.p[is.na(dy.p)] <- 1
    dy.p <- cut(dy.p,breaks = range, include.lowest=F, labels = c(1:range))
    #table(dy.p)
    dy.coef <- c()
    for (i in range:1) {
      coef <- temp.coef; coef[as.numeric(dy.p) > i] <- NA
      dy.coef <- cbind(dy.coef,coef)
    }

    dy.coef <- xts(dy.coef, as.Date(Date, format='%Y-%m-%d'))
    dy.coef <- merge(dy.coef, is.higher$crisis.sma20)
    #dy.coef <- apply(X = dy.coef,MARGIN = 2,FUN = replace.na.near)
    #dy.coef <- xts(dy.coef, as.Date(index(is.higher), format='%Y-%m-%d'))
    names(dy.coef)[length(names(dy.coef))] <- "Crisis"
    
    if(set =="VARyx(t)2" | set =="VECMyx(t)2" | set =="VECMyx(t-1)2"| set =="VECMyx(t-1)2"){
      color <- colorRampPalette(c("white", "red"))(range+3)[-c(1:3)]
    }
    if(set =="VARyx(t)3" | set =="VECMyx(t)3" | set =="VECMyx(t-1)3"| set =="VECMyx(t-1)3"){
      color <- colorRampPalette(c("white", "blue"))(range+3)[-c(1:3)]
    }
    if(set =="VARyx(t)4" | set =="VECMyx(t)4" | set =="VECMyx(t-1)4"| set =="VECMyx(t-1)4"){
      color <- c("#F3E7FC","#DDB3F9","#BD88F4","#9B63F6","#8241F1",
                 "#651CE4","#591BDE","#3912AB","#320F95","#240965")
    }
      #colorRampPalette(c("white","#240965"))(range+1)[-1]
    color <- c(color, "black")
    dy.main <- set
    saveWidget.dygraph <- 
      dygraph.interbank(dy.data=dy.coef, dy.main, color, begin.dateWindow = index(dy.coef)[1]) %>%
      dyAxis("y", label = "Coefficients", independentTicks = T) %>%
      dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
      dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2") %>%
      dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 900, labelsSeparateLines = FALSE)
    
    filename <- set
    file.path<-paste0("html/", filename,".html")
    saveWidget(saveWidget.dygraph, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = F, libdir = NULL,background = "white", title = set, knitrOptions = list())
  }
  
  # more than one variable
  
  if(n.inclusion.edgecov != 1){
    temp.coef <- coef.ergm.l
    dy.coef <- c()
    for (i in 1:dim(temp.coef)[2]) {
      temp.coef <- coef.ergm.l[,i]
      dy.p <- p.ergm.l[,i]
      dy.p[is.na(dy.p)] <- 1
      dy.p <- cut(dy.p,breaks = range, include.lowest=F, labels = c(1:range))
      #table(dy.p)
      for (i in range:1) {
        coef <- temp.coef; coef[as.numeric(dy.p) > i] <- NA
        dy.coef <- cbind(dy.coef,coef)
      }
    }
    
    dy.coef <- xts(dy.coef, as.Date(Date, format='%Y-%m-%d'))
    dy.coef <- merge(dy.coef, is.higher$crisis.sma20)
    #dy.coef <- apply(X = dy.coef,MARGIN = 2,FUN = replace.na.near)
    #dy.coef <- xts(dy.coef, as.Date(index(is.higher), format='%Y-%m-%d'))
    names(dy.coef)[length(names(dy.coef))] <- "Crisis"
    
    color <- c(colorRampPalette(c("white", "red"))(range+3)[-c(1:3)],
               colorRampPalette(c("white", "blue"))(range+3)[-c(1:3)])
    dy.main <- set
    
    saveWidget.dygraph <- 
      dygraph.interbank(dy.data=dy.coef, dy.main, color, begin.dateWindow = index(dy.coef)[1]) %>%
      dyAxis("y", label = "Coefficients", independentTicks = T) %>%
      dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
      dySeries("Crisis", label = "Crisis", color = "black", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2") %>%
      dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 900, labelsSeparateLines = FALSE)
    
    filename <- set
    file.path<-paste0("html/", filename,".html")
    saveWidget(saveWidget.dygraph, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = F, libdir = NULL,background = "white", title = set, knitrOptions = list())
  }
}
