#import wind data
group.wind <- c("交通银行股份有限公司", 
                "中国工商银行股份有限公司", 
                "中国建设银行股份有限公司", 
                "中国银行股份有限公司", 
                "平安银行股份有限公司", 
                "上海浦东发展银行股份有限公司", 
                "华夏银行股份有限公司", 
                "中国民生银行股份有限公司", 
                "招商银行股份有限公司", 
                "兴业银行股份有限公司", 
                "中信银行股份有限公司", 
                "宁波银行股份有限公司", 
                "南京银行股份有限公司", 
                "北京银行股份有限公司")

fnlist.wind <- dir("data/wind")
raw.wind <- list()
length.fnlist <- length(fnlist.wind)
raw.wind <- c()
for(i in 1:length.fnlist){
  temp <- read_excel(path = paste0("data/wind/",fnlist.wind[i]),
                     sheet = "万得",
                     skip = 0,
                     col_names = T,
                     col_types = c("text", "text", rep("numeric", 55), "date")) %>% as.data.frame
  temp <- temp[temp$数据来源=="合并报表",]
  
  locate <- sapply(group.wind, grepl, temp$公司名称)
  locate <- apply(locate, 2, which)
  locate <- locate %>% unlist
  temp <- temp[locate,]
  #temp$公司名称 <- group[1,]
  raw.wind[[i]] <- temp
  #cat(dim(raw.wind[[i]])[1]," ")
}

sum(is.na(raw.wind %>% unlist))
raw.wind <- F.fill.up.NAs(raw.wind)#sum(is.na(raw.wind[[3]] %>% unlist))
sum(is.na(raw.wind[[3]]$`拆出资金(万元)` %>% unlist))
sum(is.na(raw.wind[[3]]$`拆入资金(万元)` %>% unlist))


raw.wind[[i]]$`拆出资金(万元)`#the data is exactly the same with the data in combas

资产总计(万元)负债合计(万元)手续费及佣金收入(万元)/营业收入(万元)

