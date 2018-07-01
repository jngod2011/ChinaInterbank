#################################################################
# data summary
#################################################################
library(e1071)
library(xtable)
names(g.sp) <- group[2,]
summary.g.sp <- sapply(g.sp, each(min, max, median, mean, sd, skewness, kurtosis)) * c(100,100,10000,10000,100,1,1)
summary.g.sp <- summary.g.sp %>% round(2) %>% t
names(summary.g.sp) <- c("min", "max", "median", "mean", "sd", "skewness", "kurtosis")
write.xlsx(summary.g.sp, file = "latex/report/excel/summary_g_sp.xlsx", row.names = TRUE)

summary.g.sp <- xtable(summary.g.sp, caption = "Data Description of  Log Difference",
                            label = "tab:summary_g_sp"
)
align(summary.g.sp) <- "llllllll"
print(summary.g.sp, 
      file="latex/report/table/summary_g_sp.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
#################################################################
# ADF test
#################################################################
names(sp)[-1] <- group[2,]
d.sp <- sp[-1,-1]-sp[-ncol(sp),-1]
table.ADFtest <- rbind(
  ADFtest.tau3(sp[,-1], selectlags = "AIC")[[1]],
  ADFtest.tau3(d.sp, selectlags = "AIC")[[1]],
  ADFtest.tau3(sp[,-1], selectlags = "BIC")[[1]],
  ADFtest.tau3(d.sp, selectlags = "BIC")[[1]]
) %>% t


table.ADFtest <- xtable(table.ADFtest, caption = "Results of Unit Root Tests",
                        label = "tab:table_ADFtest")

addtorow <- list(pos = list(0, 0,0),
                 command = c("& \\multicolumn{4}{c}{AIC} & \\multicolumn{4}{c}{BIC}\\\\\n",
                             "  & \\multicolumn{2}{c}{I(0)} & \\multicolumn{2}{c}{I(1)} & \\multicolumn{2}{c}{I(0)} & \\multicolumn{2}{c}{I(1)}\\\\\n",
                             "  & T & lags & T & lags & T & lags & T & lags \\\\\n"
                 ) )
align(table.ADFtest) <- "lllllllll"
print(table.ADFtest, 
      file="latex/report/table/table_ADFtest.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      hline.after = c(-1,0, nrow(table.ADFtest)),
      add.to.row = addtorow,
      include.colnames = FALSE
)


table.ADFtest.xlsx <- rbind(
  ADFtest.tau3(sp[,-1], selectlags = "AIC")[[2]],
  ADFtest.tau3(d.sp, selectlags = "AIC")[[2]],
  ADFtest.tau3(sp[,-1], selectlags = "BIC")[[2]],
  ADFtest.tau3(d.sp, selectlags = "BIC")[[2]]
) %>% t
write.xlsx(table.ADFtest.xlsx, file = "latex/report/excel/table.ADFtest.xlsx", row.names = TRUE)

#################################################################
#cointegration test in latex
#################################################################
data <- sp
group.cointest <- group[1,]
tab.label <- "cointest"
model <- process.genFEVD.VECMorVAR(data = sp, group=group.cointest, start.point = "2007-09-25", end.point = "2018-1-19", n.ahead=1)
data <- model$data
vecm.tsDyn <- model$rank.test
#vecm.tsDyn <- VECM(data, lag = K, estim="ML",include = c("const"))
COIN.rank.test <- rank.test(vecm.tsDyn)
COIN.res <- COIN.rank.test$res_df #COIN.eigen$r #COIN.trace$r #COIN.eigen$pval #COIN.trace$pval
table.n <- nrow(COIN.res)
K <- model$K
COIN.AIC <- rank.select(data, include = c("none"))$AICs[1:table.n, K] 
COIN.BIC <- rank.select(data, include = c("none"))$BICs[1:table.n, K] 

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
      file=paste0("latex/report/table/COINtest", tab.label, ".tex"), 
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
write.xlsx(table.COINtest.xlsx, file = paste0("latex/report/excel/COINtest", tab.label,".xlsx"), row.names = TRUE)

#################################################################
#appendix
#################################################################
appendix.group <- group[c(2,3,1,5),]
rownames(appendix.group) <- c("Abbr","Name","中文名","Classification")
appendix.group <- appendix.group %>% t
appendix.group <- xtable(appendix.group, caption = "Comparison Table",
                         label = "tab:Comparison") 

align(appendix.group) <- paste0(rep("l",ncol(appendix.group)+1), collapse = "")
print(appendix.group, 
      file=paste0("latex/report/table/ComparisonTable.tex"), 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE",
      include.rownames = FALSE,
      include.colnames = TRUE)
