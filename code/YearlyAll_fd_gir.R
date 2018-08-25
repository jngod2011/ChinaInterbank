#YearlyAll_fd_gir.R
#time###########################################################################################
timestart <- Sys.time()
#####################################################################
ID <- c("1")
network.y.type <- "all"#"VECM.GIR"#VAR.GIR#
interbank.type <- "r"#r#pl#pb#ab#al
inclusion.edgecov <- c("loan.so","loan.je","loan.ot")#loan#deposit#compound
inclusion.nodecov <- NULL
inclusion.both <- c("asst")#NULL#c("asst")#,"ad"
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
#wind###########################################################################################
load(file = "data/Rdata/latex_shiborbid_raw.wind.Rdata")
raw.wind <- raw.wind.full
#network###########################################################################################
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
  "2016-12-31"
  #"2017-12-31",
  #"2018-12-31"
)
# the sample in 2018 is too small
load(file = "data/Rdata/latex_yearly_networky_Allshiborbid_gir.Rdata")
list.bank <- list.bank[c(-11,-12)]
if(network.y.type=="short"){
  network.y <- var.myl.gir.short[c(-11,-12)]
}
if(network.y.type=="long"){
  network.y <- var.myl.gir.long[c(-11,-12)]
}
if(network.y.type=="all"){
  network.y <- var.myl.gir.all[c(-11,-12)]
}
if(network.y.type=="ON"){
  network.y <- var.myl.gir.ON[c(-11,-12)]
}
if(network.y.type=="W1"){
  network.y <- var.myl.gir.W1[c(-11,-12)]
}
if(network.y.type=="W2"){
  network.y <- var.myl.gir.W2[c(-11,-12)]
}
if(network.y.type=="M1"){
  network.y <- var.myl.gir.M1[c(-11,-12)]
}
if(network.y.type=="M3"){
  network.y <- var.myl.gir.M3[c(-11,-12)]
}
if(network.y.type=="M6"){
  network.y <- var.myl.gir.M6[c(-11,-12)]
}
if(network.y.type=="M9"){
  network.y <- var.myl.gir.M9[c(-11,-12)]
}
if(network.y.type=="Y1"){
  network.y <- var.myl.gir.Y1[c(-11,-12)]
}
#loan###########################################################################################
load(file = "data/Rdata/latex_shiborbid_LoanDeposit.Rdata")
load(file = "data/Rdata/latex_group.bid.full.Rdata")
loan.network <- list()
loan.network.so <- list()
loan.network.je <- list()
loan.network.ot <- list()
if(interbank.type == "r"){
  loan.network <- loan
  for (i in 1:length(loan)) {
    temp.id <- match(colnames(loan[[i]]),group.bid$Abbr)
    
    locate <- which(group.bid$Eclass[temp.id]!="State-Owned Banks")
    temp <- loan[[i]]
    temp[locate,] <- 0
    temp[,locate] <- 0
    loan.network.so[[i]] <- temp
    
    locate <- which(group.bid$Eclass[temp.id]!="Joint-Equity Commercial Banks")
    temp <- loan[[i]]
    temp[locate,] <- 0
    temp[,locate] <- 0
    loan.network.je[[i]] <- temp
    
    locate <- which(group.bid$Eclass[temp.id]=="State-Owned Banks" |
                      group.bid$Eclass[temp.id]=="Joint-Equity Commercial Banks")
    temp <- loan[[i]]
    temp[locate,] <- 0
    temp[,locate] <- 0
    loan.network.ot[[i]] <- temp
  }
  deposit.network <- deposit
  compound.network <- list()
  for (i in 1:length(loan)) {
    compound.network[[i]] <- loan[[i]] + deposit[[i]]
  }
}


#################################################################
# basic setting
dyErgm.result <- dyCoefErgm.yearly(data = network.y[1:10], set = set, 
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
                                   wind = raw.wind[1:10]
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

for (i in 1:10) {
  print(dim(network.y[[i]])[1])
}
#NO.  & 12 & 12 & 14 & 15 & 15 & 15 & 17 & 17 & 17& 17 \\ 