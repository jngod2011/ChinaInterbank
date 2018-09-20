#################################################################
# load shibor bid
#################################################################
bond <- read_excel(path = "data/bond/Chinabond.xlsx",
                   sheet = "yield", 
                   skip = 22, 
                   col_names = F, 
                   col_types = c("date", rep("numeric", 15))) %>% as.data.frame
names(bond) <- c("Date","TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
bond$Date <- substr(bond$Date, 1, 10)
bond <- xts(bond[,-1], as.Date(bond$Date, format='%Y-%m-%d'));
save(bond,file = "data/Rdata/latex_bond.Rdata")
#################################################################
# load shibor bid
#################################################################
fnlist.shibor.bid <- dir("data/bid")
raw.shibor.bid <- list()
shibor.bid <- c()
all.bid.banks <- c()
all.shibor <- c()
locate <- c()
for(i in 1:length(fnlist.shibor.bid)){#2008
  if(as.numeric(substr(fnlist.shibor.bid[i],4,7)) <= 2015){
    raw.shibor.bid[[i]] <- read_excel(path = paste0("data/bid/",fnlist.shibor.bid[i]),
                                      sheet = "Sheet",
                                      skip = 0,
                                      col_names = T,
                                      col_types = c("date", "text", rep("numeric", 16)))[-c(1,2),-c(seq(4,18,2))] %>% as.data.frame
    names(raw.shibor.bid[[i]])[c(1,2)] <- c("Date","bider")
    raw.shibor.bid[[i]]$Date<- substr(raw.shibor.bid[[i]]$Date, 1, 10)
    shibor.bid <- rbind(shibor.bid,raw.shibor.bid[[i]])
  }else{
    raw.shibor.bid[[i]] <- read_excel(path = paste0("data/bid/",fnlist.shibor.bid[i]),
                                      sheet = "Sheet",
                                      skip = 0,
                                      col_names = T,
                                      col_types = c("date", "text", rep("numeric", 8))) %>% as.data.frame
    names(raw.shibor.bid[[i]])[c(1,2)] <- c("Date","bider")
    raw.shibor.bid[[i]]$Date<- substr(raw.shibor.bid[[i]]$Date, 1, 10)
    shibor.bid <- rbind(shibor.bid,raw.shibor.bid[[i]])
  }
  names(shibor.bid)[c(1,2)] <- c("Date","bider")
  #print(substr(fnlist.shibor.bid[i],4,7))
  #print(dim(raw.shibor.bid[[i]]))
  #print(unique(raw.shibor.bid[[i]]$bider))
  #print(length(unique(raw.shibor.bid[[i]]$bider)))
  #temp <- match(unique(raw.shibor.bid[[i]]$bider), all.bid.banks)
  #locate <- c(temp,locate)
  #print("===========================================")
  all.bid.banks <- c(all.bid.banks,unique(raw.shibor.bid[[i]]$bider)) %>% unique
}


for(i in 1:length(fnlist.shibor.bid)){
  print(substr(fnlist.shibor.bid[i],4,7))
  print(dim(raw.shibor.bid[[i]]))
  print(unique(raw.shibor.bid[[i]]$bider))
  print(length(unique(raw.shibor.bid[[i]]$bider)))
  temp <- match(unique(raw.shibor.bid[[i]]$bider), all.bid.banks)
  locate <- c(temp,locate)
  print("===========================================")
}

raw.shibor.bid.ON <- list()
raw.shibor.bid.W1 <- list()
raw.shibor.bid.W2 <- list()
raw.shibor.bid.M1 <- list()
raw.shibor.bid.M3 <- list()
raw.shibor.bid.M6 <- list()
raw.shibor.bid.M9 <- list()
raw.shibor.bid.Y1 <- list()
for (i in 1:length(raw.shibor.bid)) {
  temp <- raw.shibor.bid[[i]]
  names(temp) <- c("Date", "Bank", "ON", "W1", "W2", "M1", "M3", "M6", "M9", "Y1")
  temp <- melt(temp, id = c("Date", "Bank"), variable.name = "Term", value.name = "Bid", na.rm=T)
  temp <- dcast(temp, Date + Term ~ Bank , fun.aggregate = mean) %>% as.data.frame
  raw.shibor.bid[[i]] <- temp
}


na.list <- list(c("Term", "渣打上海", "汇丰上海","德意志上海"),
                c("Term", "南京商行", "汇丰上海", "渣打上海","汇丰中国","渣打银行","南京银行","德意志上海"),
                c("Term", "南京银行","德银中国","华夏银行","广发银行","汇丰中国","渣打银行"),
                c("Term", "广发银行","工商银行","邮储银行"),
                c("Term", "邮储银行", "广发银行"),
                c("Term", "浦发银行","国开行"),
                c("Term", "浦发银行","邮储银行","国开行"),
                c("Term","国开行"),
                c("Term","国开行"),
                c("Term","国开行"),
                c("Term","国开行"),
                c("Term","国开行"),
                c("Term","国开行")
                )
na.list %>% unlist %>% unique
names(na.list) <- c(2006:2018)

for (i in 1:length(raw.shibor.bid)) {
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="ON",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.ON[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"ON:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="W1",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.W1[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"W1:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="W2",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.W2[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"W2:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="M1",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.M1[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"M1:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="M3",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.M3[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"M3:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="M6",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.M6[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"M6:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="M9",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.M9[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"M9:",is.na(temp) %>% sum)),print("#"))
  
  temp <- raw.shibor.bid[[i]];temp <- temp[temp$Term=="Y1",];
  temp <- temp[,-match(na.list[[i]],names(temp))];
  temp.date <- temp$Date;
  temp <- temp[,-1] %>% as.data.frame;
  temp <- missForest(temp, variablewise = T, ntree = 2000);
  temp <- xts(temp$ximp, as.Date(temp.date, format='%Y-%m-%d'));
  raw.shibor.bid.Y1[[i]] <- temp;
  ifelse(sum(is.na(temp)) > 0, print(paste0(i,"Y1:",is.na(temp) %>% sum)),print("#"))
}

all.name.shiborbid <- c()
list.bank <- list()
for (i in 1:length(raw.shibor.bid.all)) {
  list.bank[[i]] <- names(raw.shibor.bid.ON[[i]])
  print(list.bank[[i]])
  print("######")
  all.name.shiborbid <- c(all.name.shiborbid,list.bank[[i]])
}
all.name.shiborbid <- all.name.shiborbid %>% unique
names(list.bank) <- c(2006:2018)
#library("VIM")
#aggr(temp,prop=FALSE,numbers=TRUE)
#View(temp)



raw.shibor.bid.short <- list()
raw.shibor.bid.long <- list()
raw.shibor.bid.all <- list()
for (t in 1:13) {
  temp.data <- raw.shibor.bid.ON[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.ON[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.W1[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.W1[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.W2[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.W2[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.M1[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.M1[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.M3[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.M3[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.M6[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.M6[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.M9[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.M9[[t]] <- temp.data
  
  temp.data <- raw.shibor.bid.Y1[[t]]
  id <- match(names(temp.data),group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  names(temp.data) <- temp.group.bid$Abbr
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  temp.data <- temp.data[,temp.group.bid$Abbr]
  raw.shibor.bid.Y1[[t]] <- temp.data
  
  raw.shibor.bid.short[[t]] <- (raw.shibor.bid.ON[[t]] + raw.shibor.bid.W1[[t]] + raw.shibor.bid.W2[[t]] + raw.shibor.bid.M1[[t]])/4
  raw.shibor.bid.long[[t]] <- (raw.shibor.bid.M3[[t]] + raw.shibor.bid.M6[[t]] + raw.shibor.bid.M9[[t]] + raw.shibor.bid.Y1[[t]])/4
  raw.shibor.bid.all[[t]] <- (raw.shibor.bid.short[[t]] + raw.shibor.bid.long[[t]])/2
  }
    
save(raw.shibor.bid.ON,
  raw.shibor.bid.W1,
  raw.shibor.bid.W2,
  raw.shibor.bid.M1,
  raw.shibor.bid.M3,
  raw.shibor.bid.M6,
  raw.shibor.bid.M9,
  raw.shibor.bid.Y1,
  raw.shibor.bid.short,
  raw.shibor.bid.long,
  raw.shibor.bid.all,
  file = "data/Rdata/latex_ALLshiborbid_RawShiborBid.Rdata")
#################################################################
# network.y
#################################################################
load(file = "data/Rdata/latex_ALLshiborbid_RawShiborBid.Rdata")
aenet.myl.ON <- list()
aenet.myl.W1 <- list()
aenet.myl.W2 <- list()
aenet.myl.M1 <- list()
aenet.myl.M3 <- list()
aenet.myl.M6 <- list()
aenet.myl.M9 <- list()
aenet.myl.Y1 <- list()
aenet.myl.short <- list()
aenet.myl.long <- list()
aenet.myl.all <- list()

for (i in 1:length(raw.shibor.bid.all)) {
  data <- merge(raw.shibor.bid.ON[[i]],bond) %>% na.omit
  n.bank <- dim(raw.shibor.bid.ON[[i]])[2]
  aenet.myl.ON[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL", end.point = "NULL")$Coef
  aenet.myl.ON[[i]] <- replaceNA0(aenet.myl.ON[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.W1[[i]],bond) %>% na.omit
  aenet.myl.W1[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.W1[[i]] <- replaceNA0(aenet.myl.W1[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.W2[[i]],bond) %>% na.omit
  aenet.myl.W2[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.W2[[i]] <- replaceNA0(aenet.myl.W2[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.M1[[i]],bond) %>% na.omit
  aenet.myl.M1[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.M1[[i]] <- replaceNA0(aenet.myl.M1[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.M3[[i]],bond) %>% na.omit
  aenet.myl.M3[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.M3[[i]] <- replaceNA0(aenet.myl.M3[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.M6[[i]],bond) %>% na.omit
  aenet.myl.M6[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.M6[[i]] <- replaceNA0(aenet.myl.M6[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.M9[[i]],bond) %>% na.omit
  aenet.myl.M9[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.M9[[i]] <- replaceNA0(aenet.myl.M9[[i]][[1]])[1:n.bank,1:n.bank]
  
  data <- merge(raw.shibor.bid.Y1[[i]],bond) %>% na.omit
  aenet.myl.Y1[[i]] <- aenet.fix(data = data, fix = "yearly", start.point = "NULL",end.point = "NULL")$Coef
  aenet.myl.Y1[[i]] <- replaceNA0(aenet.myl.Y1[[i]][[1]])[1:n.bank,1:n.bank]
  }

for (i in 1:length(raw.shibor.bid.all)) {
  aenet.myl.short[[i]] <- (aenet.myl.ON[[i]]+aenet.myl.W1[[i]]+aenet.myl.W2[[i]]+aenet.myl.M1[[i]])/4
  aenet.myl.long[[i]] <- (aenet.myl.M3[[i]]+aenet.myl.M6[[i]]+aenet.myl.M9[[i]]+aenet.myl.Y1[[i]])/4
  aenet.myl.all[[i]] <- (aenet.myl.short[[i]]+aenet.myl.long[[i]])/2
  }
save(list.bank,
  aenet.myl.ON, 
     aenet.myl.W1, 
     aenet.myl.W2, 
     aenet.myl.M1, 
     aenet.myl.M3, 
     aenet.myl.M6, 
     aenet.myl.M9, 
     aenet.myl.Y1, 
     aenet.myl.short,
     aenet.myl.long, 
     aenet.myl.all, 
     file = "data/Rdata/latex_ALLshiborbid_aenet.Rdata")
#################################################################
# wind: estimate loan and deposit network
#################################################################
group.bid <- read_excel(path = "data/GroupBidFull.xlsx",
                        sheet = "group",
                        skip = 0,
                        col_names = T,
                        col_types = c(rep("text", 7))) %>% as.data.frame
group.bid$Eclass <- factor(group.bid$Eclass, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks","Foreign Capital Bank"))
group.bid <- group.bid[order(group.bid$Eclass,decreasing = F),] 
rownames(group.bid) <- group.bid$Abbr
group.bid$color <- c(colorRampPalette(c("#B71C1C", "white"))(7)[-7], colorRampPalette(c("#FF9800", "white"))(10)[-10], colorRampPalette(c("#33691E", "white"))(3)[-3],colorRampPalette(c("#1A237E", "white"))(3)[-3])
save(group.bid, file = "data/Rdata/latex_group.bid.full.Rdata")
#################################################################
# wind: estimate loan and deposit network
#################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")
load(file = "data/Rdata/latex_ALLshiborbid_aenet.Rdata")
load(file = "data/Rdata/latex_group.bid.full.Rdata")
list.bank <- list.bank[-1]
loan <- list()
deposit <- list()
for (i in 1:(length(list.bank)-2)) {
  id <- match(list.bank[[i]],group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  
  l.id <- match(temp.group.bid$Cwind,colnames(l.matrix[[i]]))
  print(sum(is.na(l.id)))
  print(l.id)
  print(temp.group.bid$Cname)
  print("###########")
  l.temp <- l.matrix[[i]][l.id, l.id]
  l.temp[is.na(l.temp)] <- 0
  colnames(l.temp) <- rownames(l.temp) <- temp.group.bid$Abbr
  loan[[i]] <- l.temp
  
  d.id <- match(temp.group.bid$Cwind,colnames(d.matrix[[i]]))
  d.temp <- d.matrix[[i]][d.id, d.id]
  d.temp[is.na(d.temp)] <- 0
  colnames(d.temp) <- rownames(d.temp) <- temp.group.bid$Abbr
  deposit[[i]] <- d.temp
}
save(loan,deposit,file = "data/Rdata/latex_shiborbid_LoanDeposit.Rdata")
#################################################################
# wind scale for 10 banks
#################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")
load(file = "data/Rdata/latex_ALLshiborbid_aenet.Rdata")
load(file = "data/Rdata/latex_group.bid.full.Rdata")
list.bank <- list.bank[-1]

wind.name <- read_excel(path = "data/windname.xlsx",sheet = "name",
                        skip = 0,
                        col_names = F,
                        col_types = c("text", "text" , "text")) %>% as.data.frame

fnlist.wind <- dir("data/wind")
raw.wind.full <- list()
length.fnlist <- length(fnlist.wind)
for(i in 1:length.fnlist){
  id <- match(list.bank[[i]],group.bid$Cname)
  temp.group.bid <- group.bid[id,]
  o <- order(temp.group.bid[,"Eclass"], temp.group.bid[,"Abbr"])
  temp.group.bid <- temp.group.bid[o,]
  group.wind <- temp.group.bid$Cwind
  
  temp <- read_excel(path = paste0("data/wind/",fnlist.wind[i]),
                     sheet = "万得",
                     skip = 0,
                     col_names = T,
                     col_types = c("text", "text", rep("numeric", 55), "date")) %>% as.data.frame
  temp <- temp[temp$数据来源=="合并报表",]
  #print(paste("lending",length(temp$`拆出资金(万元)` %>% na.omit), "in",i))
  #print(paste("borrowing",length(temp$`拆入资金(万元)` %>% na.omit), "in",i))
  
  temp$`向中央银行借款(万元)`[is.na(temp$`向中央银行借款(万元)`)] <- 0
  temp$`向中央银行借款净增加额(万元)`[is.na(temp$`向中央银行借款净增加额(万元)`)] <- 0
  temp$`向其他金融机构拆入资金净增加额(万元)`[is.na(temp$`向其他金融机构拆入资金净增加额(万元)`)] <- 0
  temp$`客户存款和同业存放款项净增加额(万元)`[is.na(temp$`客户存款和同业存放款项净增加额(万元)`)] <- 0
  temp$`客户贷款及垫款净增加额(万元)`[is.na(temp$`客户贷款及垫款净增加额(万元)`)] <- 0
  temp <- subset(temp, select =  -`应付利息(万元)`)
  temp <- subset(temp, select =  -`应收利息(万元)`)
  #temp <- subset(temp, select =  -`基本每股收益(万/股)`)
  #temp <- subset(temp, select =  -`稀释每股收益(元/股)`)
  temp$`基本每股收益(万/股)` <- temp$`基本每股收益(万/股)` * (10^4)/10000*(12^10)#scale#
  temp$`稀释每股收益(元/股)` <- temp$`稀释每股收益(元/股)` * 10000*(12^10)#scale#
  #temp$`应付利息(万元)` <- temp$`应付利息(万元)` /(10^6)/10000*(12^10)#scale#
  #temp$`应收利息(万元)` <- temp$`应收利息(万元)` /(10^6)/10000*(12^10)#scale#
  temp.title <- temp[,c("公司名称","数据来源","报告期")]
  temp <- subset(temp, select =  -`报告期`)
  temp <- subset(temp, select =  -`公司名称`)
  temp <- subset(temp, select =  -`数据来源`)
  
  #print(temp %>% is.na %>% sum)
  onames <- names(temp)
  names(temp) <- wind.name[,2][match(onames,wind.name[,1])]
  temp <- temp*10000/(12^10)#scale#
  #log(temp[1,] %>% abs,10) %>% ceiling
  #log(temp$asst %>% abs,10) %>% ceiling
  rownames(temp) <- temp.title$公司名称
  raw.wind.full[[i]] <- temp
  raw.wind.full[[i]] <- cbind(raw.wind.full[[i]],temp.title)
 
  locate <- match(group.wind, temp.title$公司名称)
  temp <- temp[locate,]
  rownames(temp) <- temp.group.bid$Abbr
  print(sum(is.na(temp)))
  print(i)
  print("########")
  temp <- missForest(temp, variablewise = T, ntree = 2000)
  raw.wind.full[[i]] <- temp$ximp
}

raw.wind.full %>% unlist %>%is.na %>% sum

for (i in 1:length(raw.wind.full)) {
  raw.wind.full[[i]]$clnd <- raw.wind.full[[i]]$lnd + raw.wind.full[[i]]$dlnd
  raw.wind.full[[i]]$cbrr <- raw.wind.full[[i]]$brr + raw.wind.full[[i]]$dbrr
}

save(raw.wind.full, file = "data/Rdata/latex_shiborbid_raw.wind.Rdata")
load(file = "data/Rdata/latex_shiborbid_raw.wind.Rdata")
#################################################################
# YearlyAll_fd_gir.R
#################################################################
load(file = "data/Rdata/latex_ALLshiborbid_RawShiborBid.Rdata")
load(file = "data/Rdata/latex_group.bid.full.Rdata")
var.myl.gir <- list()
vecm.myl.gir <- list()
network.y <- list()

raw.shibor.bid <- list(
  raw.shibor.bid.ON,
  raw.shibor.bid.W1,
  raw.shibor.bid.W2,
  raw.shibor.bid.M1,
  raw.shibor.bid.M3,
  raw.shibor.bid.M6,
  raw.shibor.bid.M9,
  raw.shibor.bid.Y1
)

type.list <- c("ON","W1","W2","M1","M3","M6","M9","Y1")
for (type in 1:length(type.list)) {
  for (t in 1:length(raw.shibor.bid.Y1)) {
    temp <- raw.shibor.bid[[type]][[t]]
    n.bank <- dim(temp)[2]
    try.error <- try(vecm.tsDyn <- VECM(data = temp, lag=2, estim="ML"),silent = TRUE)#Type of estimator: 2OLS for the two-step approach or ML for Johansen MLE
    rank.test <- vecm.tsDyn
    rank.eigen <- rank.test(vecm.tsDyn, cval = 0.01, type = "eigen")$r
    rank.trace <- rank.test(vecm.tsDyn, cval = 0.01, type = "trace")$r
    rank <- floor((rank.eigen + rank.trace)/2)#4
    if(rank >= n.bank){
      print(paste0("rank is ", rank," for ",type.list[type]," with nbank = ",n.bank))  
    }
  }
}


for (type in 1:length(type.list)) {
  for (t in 2:length(raw.shibor.bid.Y1)) {
    temp.data <- raw.shibor.bid[[type]][[t]]
    n.bank <- dim(temp.data)[2]
    temp.abbr <- names(temp.data)
    if(0){
      vecm.gir0 <- dy.VECM.GIR(data = temp.data, n.ahead = 0, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir0 <- matrix(data = unlist(vecm.gir0),nrow = n.bank, ncol = n.bank)
      vecm.gir1 <- dy.VECM.GIR(data = temp.data, n.ahead = 1, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir1 <- matrix(data = unlist(vecm.gir1),nrow = n.bank, ncol = n.bank)
      vecm.gir2 <- dy.VECM.GIR(data = temp.data, n.ahead = 2, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir2 <- matrix(data = unlist(vecm.gir2),nrow = n.bank, ncol = n.bank)
      vecm.gir3 <- dy.VECM.GIR(data = temp.data, n.ahead = 3, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir3 <- matrix(data = unlist(vecm.gir3),nrow = n.bank, ncol = n.bank)
      vecm.gir4 <- dy.VECM.GIR(data = temp.data, n.ahead = 4, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir4 <- matrix(data = unlist(vecm.gir4),nrow = n.bank, ncol = n.bank)
      vecm.gir5 <- dy.VECM.GIR(data = temp.data, n.ahead = 5, span = "yearly", keep.vecm = T, rank = rank)[[1]];vecm.gir5 <- matrix(data = unlist(vecm.gir5),nrow = n.bank, ncol = n.bank)
      GIR <- array(c(vecm.gir0,
                     vecm.gir1,
                     vecm.gir2,
                     vecm.gir3,
                     vecm.gir4,
                     vecm.gir5), dim = c(n.bank,n.bank,6))
      temp <- weighted_gir(GIR, divided=1)$weighted.matrix
      temp <- matrix(temp %>% unlist,n.bank,n.bank) #%>% t
      vecm.myl.gir[[t]] <- temp
      colnames(vecm.myl.gir[[t]]) <- temp.abbr;rownames(vecm.myl.gir[[t]]) <- temp.abbr
    }
    
    temp.date <- index(temp.data)
    temp.date <-  diff(log(temp.data), lag=1)[-1,]
    
    var.gir0 <- dy.VAR.GIR(data = temp.data, n.ahead = 0, span = "yearly")[[1]];var.gir0 <- matrix(data = unlist(var.gir0),nrow = n.bank,ncol = n.bank)
    var.gir1 <- dy.VAR.GIR(data = temp.data, n.ahead = 1, span = "yearly")[[1]];var.gir1 <- matrix(data = unlist(var.gir1),nrow = n.bank,ncol = n.bank)
    var.gir2 <- dy.VAR.GIR(data = temp.data, n.ahead = 2, span = "yearly")[[1]];var.gir2 <- matrix(data = unlist(var.gir2),nrow = n.bank,ncol = n.bank)
    var.gir3 <- dy.VAR.GIR(data = temp.data, n.ahead = 3, span = "yearly")[[1]];var.gir3 <- matrix(data = unlist(var.gir3),nrow = n.bank,ncol = n.bank)
    var.gir4 <- dy.VAR.GIR(data = temp.data, n.ahead = 4, span = "yearly")[[1]];var.gir4 <- matrix(data = unlist(var.gir4),nrow = n.bank,ncol = n.bank)
    var.gir5 <- dy.VAR.GIR(data = temp.data, n.ahead = 5, span = "yearly")[[1]];var.gir5 <- matrix(data = unlist(var.gir5),nrow = n.bank,ncol = n.bank)
    GIR <- array(c(var.gir0,
                   var.gir1,
                   var.gir2,
                   var.gir3,
                   var.gir4,
                   var.gir5), dim = c(n.bank,n.bank,6))
    temp <- weighted_gir(GIR, divided=1)$weighted.matrix
    temp <- matrix(temp %>% unlist,n.bank,n.bank) #%>% t
    var.myl.gir[[t]] <- temp
    colnames(var.myl.gir[[t]]) <- temp.abbr;rownames(var.myl.gir[[t]] ) <- temp.abbr
  }
  save(var.myl.gir,
       file = paste0("data/Rdata/latex_yearly_networky_Allshiborbid_gir_",type.list[type],".Rdata")
  )
}

load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_ON.Rdata")
var.myl.gir.ON <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_W1.Rdata")
var.myl.gir.W1 <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_W2.Rdata")
var.myl.gir.W2 <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_M1.Rdata")
var.myl.gir.M1 <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_M3.Rdata")
var.myl.gir.M3 <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_M6.Rdata")
var.myl.gir.M6 <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_M9.Rdata")
var.myl.gir.M9 <- var.myl.gir[-1]
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir_Y1.Rdata")
var.myl.gir.Y1 <- var.myl.gir[-1]

var.myl.gir.short <- list()
for(i in 1:length(var.myl.gir.ON)){
  var.myl.gir.short[[i]] <- (var.myl.gir.ON[[i]] + 
                               var.myl.gir.W1[[i]] + 
                               var.myl.gir.W2[[i]] + 
                               var.myl.gir.M1[[i]])/4
}

var.myl.gir.long <- list()
for(i in 1:length(var.myl.gir.ON)){
  var.myl.gir.long[[i]] <- (var.myl.gir.M3[[i]] + 
                              var.myl.gir.M6[[i]] + 
                              var.myl.gir.M9[[i]] + 
                              var.myl.gir.Y1[[i]])/4
}

var.myl.gir.all <- list()
for(i in 1:length(var.myl.gir.ON)){
  var.myl.gir.all[[i]] <- (var.myl.gir.short[[i]]+var.myl.gir.long[[i]])/2
}

y.period <- c(#"2006-12-31",
  "2007-12-31",
  "2008-12-31",
  "2009-12-31",
  "2010-12-31",
  "2011-12-31",
  "2012-12-31",
  "2013-12-31",
  "2014-12-31",
  "2015-12-31",
  "2016-12-31",
  "2017-12-31",
  "2018-12-31"
)

save(list.bank,
  y.period,
     var.myl.gir.ON,
     var.myl.gir.W1,
     var.myl.gir.W2,
     var.myl.gir.M1,
     var.myl.gir.M3,
     var.myl.gir.M6,
     var.myl.gir.M9,
     var.myl.gir.Y1,
     var.myl.gir.short,
     var.myl.gir.all,
     var.myl.gir.long,
     file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir.Rdata"
)



