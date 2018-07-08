data_ori <- "Grp_1;Grp_2;Grp_3;Grp_4;Grp_5
a;2.6;2.9;2.1;2.0;2.2
b;20.8;9.8;7.0;3.7;19.2
c;10.0;11.0;9.2;12.4;9.6
d;9;3.3;10.3;11.1;10"

data <- read.table(text=data_ori, header=T, row.names=1, sep=";", quote="")
data
library(ggplot2)
library(reshape2)
library(dplyr)
data_rownames <- rownames(data)
data_colnames <- colnames(data)
data$gene <- data_rownames
data_m <- melt(data, id.vars=c("gene"))
data_m
# 给定数据，和x轴、y轴所在列名字
# 直接使用geom_bar就可以绘制柱状图
# position: dodge: 柱子并排放置
p <- ggplot(data_m, aes(x=gene, y=value))
p + geom_bar(stat="identity", position="dodge", aes(fill=variable))

# 如果没有图形界面，运行下面的语句把图存在工作目录下的Rplots.pdf文件中
#dev.off()

# 获取平均值和标准差
data_m_sd_mean <- data_m %>% group_by(gene) %>% dplyr::summarise(sd=sd(value), value=mean(value))
data_m_sd_mean <- as.data.frame(data_m_sd_mean)
data_m_sd_mean

p <- ggplot(data_m_sd_mean, aes(x=gene, y=value)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd))
p

# position="fill" 展示的是堆积柱状图各部分的相对比例
# position="stack" 展示的是堆积柱状图的原始值
data_m$value[1] <- 5
p <- ggplot(data_m, aes(x=variable, y=value)) +
  geom_bar(stat="identity", position="stack", aes(fill=gene)) +
  geom_text(aes(label=value), position=position_stack(vjust=0.5))
p