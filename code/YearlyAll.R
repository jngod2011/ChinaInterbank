#YearlyAll.R
#time###########################################################################################
timestart <- Sys.time()
#####################################################################
ID <- c("1")
network.y.type <- "VECM.GIR"#"VECM.GIR"#VAR.GIR#
interbank.type <- "r"#r#pl#pb#ab#al
inclusion.edgecov <- c("loan","deposit")#loan#deposit#compound
inclusion.nodecov <- NULL
inclusion.both <- c("asst","cbrr")#NULL#c("asst")#,"ad"
library(knitr)
network.x <- NULL#combine_words(inclusion.edgecov, and = "",sep = "-",before ="",after = "")
inclusion.out <- NULL##NULL#"dlnd"#,"dfa""dlnd",
inclusion.in <-  NULL#c("brr","dbrr")#"dbrr"#c("grp")#"dbrr",
inclusion.wind <- NULL
inclusion.nodeocov <- c(inclusion.both, inclusion.out, inclusion.wind)
inclusion.nodeicov <- c(inclusion.both, inclusion.in, inclusion.wind)
inclusion.absdiff <- NULL
#setting###########################################################################################
set <- ""
triangle <- "all"
directed <- T
BidType <- "aenet"
filename <- paste0(network.y.type,"_",interbank.type,"_",ID)#"_",network.x,
filename
MCMLE.maxit <- 100
trans <- c("y","ON","W1","W2","M1","M3","M6","M9","Y1","short","long","all")
inclusion.cyclicalweights <- F
inclusion.mutual <- F
inclusion.nodecovar <- F
inclusion.nodeicovar <-F
inclusion.nodeocovar <- F
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- F
#library###########################################################################################
# load libraries
library(knitr)
library(devtools)
library(dplyr)
library(dygraphs)
library(ergm.count)
library(ergm)
library(frequencyConnectedness)
library(gcdnet)
library(ggplot2)
library(glmnet)
library(htmlwidgets)
#library(igraph)
library(latentnet)
library(lubridate)
library(magrittr)
library(miscTools)
library(msaenet)
#library(networkD3)
library(NetworkRiskMeasures)
library(plyr)
library(rbokeh)
library(readxl)
library(reshape2)
library(tcltk)
#library(ts)
library(tsDyn)
library(TTR)
library(vars)
library(VIM) 
library(xlsx)
library(xtable)
library(xts)
library(GERGM)
library(stargazer)
source("code/function.R")
source("code/functionInterbank.R")
source("code/functionNewRMB.R")
#load data###########################################################################################
load(file = "data/Rdata/latex_sp.all.Rdata")#l.is.listed;l.sp.all
load(file = "data/Rdata/latex_group.stockprice.Rdata")
l.is.listed <- l.is.listed[-length(l.is.listed)]
l.is.listed <- l.is.listed[-c(1:length(1991:2006))]
#interbank network###########################################################################################
load(file = "data/Rdata/latex_raw.wind_all.Rdata")
#interbank network###########################################################################################
load(file = "data/Rdata/latex_interbank_wind.Rdata")
load(file = "data/Rdata/latex_group.stockprice.Rdata")

l.matrix.all <- list()
for (i in 1:length(2007:2016)) {
  temp.Cwind <- group.stockprice[l.is.listed[[i]],"Cwind"]
  temp.locate <- match(temp.Cwind,colnames(l.matrix[[i]]))
  temp.matrix <- l.matrix[[i]][temp.locate,temp.locate]
  temp.matrix[is.na(temp.matrix)] <- 0
  colnames(temp.matrix) <- l.is.listed[[i]];rownames(temp.matrix) <- l.is.listed[[i]]
  l.matrix.all[[i]] <- temp.matrix
}

d.matrix.all <- list()
for (i in 1:length(2007:2016)) {
  temp.Cwind <- group.stockprice[l.is.listed[[i]],"Cwind"]
  temp.locate <- match(temp.Cwind,colnames(d.matrix[[i]]))
  temp.matrix <- d.matrix[[i]][temp.locate,temp.locate]
  temp.matrix[is.na(temp.matrix)] <- 0
  colnames(temp.matrix) <- l.is.listed[[i]];rownames(temp.matrix) <- l.is.listed[[i]]
  d.matrix.all[[i]] <- temp.matrix
}

loan.network <- l.matrix.all
deposit.network <- d.matrix.all
#network###########################################################################################
load(file = "data/Rdata/latex_yearly_networky_all.Rdata")
if(network.y.type=="VECM.GIR"){
  network.y <- vecm.myl.gir
}
if(network.y.type=="VAR.GIR"){
  network.y <- var.myl.gir
}
#################################################################
# basic setting
dyErgm.result <- dyCoefErgm.yearly(data = network.y, set = set, 
                                   inclusion.edgecov = inclusion.edgecov,
                                   inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov,
                                   inclusion.absdiff = inclusion.absdiff,
                                   inclusion.cyclicalweights = inclusion.cyclicalweights,
                                   inclusion.mutual = inclusion.mutual,
                                   inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar,
                                   inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar,
                                   Date = network.y$Date, 
                                   trans = trans, 
                                   BidType = BidType,
                                   MCMLE.maxit = MCMLE.maxit,
                                   triangle = triangle,
                                   directed = directed,
                                   tab = T, csv = F,
                                   wind = raw.wind
)

dyErgm.result$scale;
result <- dyErgm.result$tab.ergm$coef
result[dyErgm.result$tab.ergm$pvalue > 0.1 | is.na(dyErgm.result$tab.ergm$pvalue)] <- NA
xlsx::write.xlsx(result, file=paste0(filename,".xlsx"), sheetName="nodecov",  col.names = T, showNA = F)

stargazer(dyErgm.result$result.vergm.l, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), 
          column.labels = c(2007:2016) %>% as.character, 
          covariate.labels = c("cns",sub(pattern = "coef.", replacement = "",  names(result))), 
          notes = "", 
          digits = 2, 
          label = paste0("tab:",filename), 
          out = paste0(filename,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

#Number & 7 & 14 & 14 & 14 & 16 & 16 & 16 & 16 & 16 & 16 \\ 

for (i in 1:length(2007:2016)) {
  l.is.listed[[i]] %>% print
  colnames(vecm.myl.gir[[i]]) %>% print
  colnames(l.matrix.all[[i]]) %>% print
  colnames(d.matrix.all[[i]]) %>% print
  rownames(raw.wind[[i]])  %>% print
  print("#######################")
}
