## shibor rate
fnlist.shibor <- dir("data/shibor")
raw.shibor <- list()
length.fnlist <- length(fnlist.shibor)
shibor <- c()
for(i in 1:length.fnlist){
  raw.shibor[[i]] <- read_excel(path = paste0("data/shibor/",fnlist.shibor[i]),
                                sheet = "Sheet",
                                skip = 0,
                                col_names = T,
                                col_types = c("date", rep("numeric", 8))) %>% as.data.frame
  shibor <- rbind(shibor,raw.shibor[[i]])
}
names(shibor)[1] <- "Date"
shibor$Date<- substr(shibor$Date, 1, 10)

dy.data <- xts(shibor[,-1], as.Date(shibor$Date, format='%Y-%m-%d'))
dy.main <- "Shanghai Interbank Offered Rate from 2006 to 2018"
color <- c(colorRampPalette(c("#880E4F", "white"))(5)[-5],colorRampPalette(c("black", "white"))(5)[-5])
dygraph.interbank(dy.data, dy.main, color)

## shibor: is shorter rae higher than the longer one?
is.higher <- shibor
is.higher[,c(2:dim(shibor)[2])] <- 0
names(is.higher)[ncol(is.higher)] <- "sum"
for (i in 1:nrow(shibor)) {
  for (j in 2:(ncol(shibor)-1)) {
    for (f in 1:(9-j)) {
      if(shibor[i,j]>shibor[i,j+f]){is.higher[i,j] <- is.higher[i,j]+1
      }
    }
  }
}
is.higher$sum <- rowSums(is.higher[,-c(1,ncol(is.higher))])
max(is.higher$sum)#24 vs 28
max(is.higher$`O/N`)
max(is.higher$`1W`)
max(is.higher$`2W`)
max(is.higher$`1M`)
max(is.higher$`3M`)
max(is.higher$`6M`)
max(is.higher$`9M`)
is.higher$crisis.sma20 <- TTR::SMA(is.higher$sum,20)
is.higher$crisis <- cut(is.higher$sum,breaks = 5, include.lowest=F, labels = c(1:5))
is.higher <- xts(is.higher, as.Date(is.higher$Date, format='%Y-%m-%d'))
#y <- table(x) View(y)

data <- cbind(shibor[,-1], as.data.frame(is.higher$crisis.sma20))
names(data)[ncol(data)] <- "Crisis"
dy.data <- xts(data, as.Date(shibor$Date, format='%Y-%m-%d'))
dy.main <- "Shanghai Interbank Offered Rate from 2006 to 2018"
color <- c(colorRampPalette(c("#880E4F", "white"))(5)[-5],colorRampPalette(c("black", "white"))(5)[-5], "black")
dygraph.shibor <- dygraph.interbank(dy.data, dy.main, color) %>% 
  dyAxis("y", label = "Shanghai Interbank Offered Rate", independentTicks = T) %>%
  dyAxis("y2", label = "Crisis" ,independentTicks = TRUE, drawGrid = F) %>%
  dySeries("Crisis", label = "Crisis", color = "#FFC107", strokeWidth = 0.2, fillGraph = 0.5,axis = "y2")


file.path<-"html/test.html"
saveWidget(dygraph.shibor, file = file.path(normalizePath(dirname(file.path)),basename(file.path)), selfcontained = F, libdir = NULL,background = "white", title = "shibor", knitrOptions = list())
#is.higher$sum <- rowSums(is.higher[,-c(1,ncol(is.higher))])
#zeros <- rep(0, length(is.higher$sum))
#temp <- cbind(zeros, is.higher$sum,is.higher$sum)
#colnames(temp) <- c("lower", "crisis", "upper")
#data <- cbind(shibor[,-1], temp)
#dygraph.interbank(dy.data, dy.main, color) %>% dySeries(c("lower", "crisis", "upper"), label = "crisis", color = "#FFC107", strokeWidth = 0.2)





