Sys.setlocale("LC_TIME", "C")
library(readxl)
library(dplyr)
library(reshape2)
library(lubridate)

# load raw data
raw.data <- read_excel(path = "data/wind/2016年报.xlsx",
                     sheet = 1,
                     skip = 0,
                     col_names = T,
                     col_types = c("numeric", "text", "numeric", "date", "text", rep("numeric", 67))) %>% as.data.frame
names(raw.data) <- c("银行代码", "银行中文简称", "股票代码", "会计期间", "报表类型编码", "现金及存放中央银行款项", "贵金属", "存放同业款项", "拆出资金净额", "交易性金融资产", "衍生金融资产", "买入返售金融资产净额", "应收款项类投资", "其他应收款净额", "应收利息净额", "应收股利净额", "发放贷款及垫款净额", "定期存款", "可供出售金融资产净额", "持有至到期投资净额", "长期股权投资净额", "投资性房地产净额", "固定资产净额", "在建工程净额", "固定资产清理", "无形资产净额", "商誉净额", "长期待摊费用", "递延所得税资产", "其他资产", "资产总计", "向中央银行借款", "拆入资金", "吸收存款及同业存放", "其中：同业及其他金融机构存放款项", "其中：吸收存款", "短期借款", "交易性金融负债", "衍生金融负债", "卖出回购金融资产款", "应付职工薪酬", "应交税费", "应付利息", "应付股利", "递延收益-流动负债", "其他应付款", "长期借款", "应付债券", "预计负债", "递延收益-非流动负债", "递延所得税负债", "其他负债", "负债合计", "实收资本(或股本)", "其他权益工具", "其中：优先股", "其中：永续债", "其中：其他", "资本公积", "减：库存股", "专项储备", "其他综合收益", "盈余公积", "未分配利润", "外币报表折算差额", "未确认的投资损失", "一般风险准备", "交易风险准备", "归属于母公司所有者权益合计", "少数股东权益", "所有者权益合计", "负债与所有者权益总计")

data <- filter(raw.data,  报表类型编码 == "A" & 会计期间 == "2016-12-31" ) 
data[is.na(data)] <- 0

#maximum entropy
library(NetworkRiskMeasures)
EstimatedMatrix.me <- matrix_estimation(data$拆出资金净额, data$拆入资金, method = "me", max.it = 10000, abs.tol = 0.001, verbose = TRUE)
st.EstimatedMatrix.me <- EstimatedMatrix.me/max(EstimatedMatrix.me)

#network drawing
igraph.matrix <- graph_from_adjacency_matrix(st.EstimatedMatrix.me, mode = c("directed"), weighted = TRUE, diag = FALSE)
networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
#networkD3.matrix[[2]]$size <- rowSums(sd.matrix) - colSums(sd.matrix)
#networkD3.matrix[[2]]$group <- c("亚洲发展中国家", "其它发展中国家", "其它发达国家", "其它发展中国家", "其它发达国家", "亚洲发展中国家", "亚洲发展中国家", "亚洲发达国家", "亚洲发达国家", "其它发展中国家", "亚洲发展中国家", "亚洲发展中国家", "其它发展中国家", "亚洲发展中国家", "欧洲发达国家或地区", "欧洲发达国家或地区", "其它发达国家")#c("中国", "发展中国家", "发达国家", "发展中国家", "发达国家", "发展中国家", "发展中国家", "发达国家", "发达国家", "发展中国家", "发展中国家", "发展中国家", "发展中国家", "发展中国家", "发达国家", "发达国家", "发达国家")
#networkD3.matrix[[2]]$name <- c("人民币", "阿根廷比索", "澳大利亚元", "巴西雷亚尔", "加拿大元", "印度卢比", "印尼盾", "日元", "韩元", "墨西哥比索", "俄罗斯卢布", "沙特里亚尔", "南非兰特", "土耳其里拉", "英镑", "欧元", "美元")

temp <- networkD3.matrix
exchange.temp <- temp[[1]]
locate.minus <- which(temp[[1]]$value < 0)
temp[[1]]$source[locate.minus] <- exchange.temp$target[locate.minus]
temp[[1]]$target[locate.minus] <- exchange.temp$source[locate.minus]
temp[[1]]$value <- abs(temp[[1]]$value)
temp[[1]] <- temp[[1]][-which(temp[[1]]$value < 0.002),]

temp[[2]]$size <- temp[[2]]$size*100
temp[[1]]$value <- temp[[1]]$valu*100
temp[[1]]$value[temp[[1]]$value > 10] <- 10
temp[[2]]$size[temp[[2]]$size < 3] <- 3

temp[[2]]$name <- paste0(rep("_", NROW(temp[[2]]$name)), temp[[2]]$name)
matrix.date <- data.frame(" ",0, as.character(rbokeh.date[i,]))
names(matrix.date) <- names(temp[[2]])
temp[[2]] <- rbind(temp[[2]], matrix.date)

rbokeh.g20 <- forceNetwork(Links = temp[[1]], Nodes = temp[[2]],
                           Source = "source", Target = "target", Value = "value", 
                           arrows = T, linkDistance = JS("function(d){return 1 / d.value * 20 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "#C3C1C6",
                           NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                           Group = "group", legend = T, fontSize = 18, opacity = 0.8, opacityNoHover = 1,
                           height = 900 , width = 900, bounded = F, zoom = T, charge = -1500)
widget2png(rbokeh.g20, file = paste0("G", i, ".png"), timeout = 1)


