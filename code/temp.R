#time###########################################################################################
timestart <- Sys.time()
#####################################################################
ID <- c("SL5")
inclusion.edgecov <- c("rolling")
inclusion.nodecov <- NULL
inclusion.nodeocov <- NULL
inclusion.nodeicov <- NULL
inclusion.absdiff <- NULL
#setting###########################################################################################
set <- " "
network.y.type <- "VAR.GIR"#"VECM.GIR"#VAR.GIR#VECM.GIR.OR
triangle <- "all"
directed <- T
BidType <- "aenet"
filename <- paste0(network.y.type,"_",as.character(directed),"_",triangle,"_",BidType, ID)
MCMLE.maxit <- 100
trans <- c("y","rolling")
inclusion.cyclicalweights <- F
inclusion.mutual <- F
inclusion.nodecovar <- F
inclusion.nodeicovar <-F
inclusion.nodeocovar <- F
inclusion.nodeisqrtcovar <- F
inclusion.nodeosqrtcovar <- F
inclusion.nodesqrtcovar <- F
#shibor###########################################################################################
n.is.higher <- c("O.N","X1W","X2W","X1M","X3M","X6M","X9M","sum","crisis.sma20","crisis")
data <- read.csv(file = "data/bank10/ForestData.csv")
data <- xts(data[,-1], as.Date(data[,1], format='%Y-%m-%d'))
Date <- index(data) %>% as.character
is.higher <- data[,n.is.higher]

network.y <- aenet.myl.M1[-1]
rolling <- aenet.myl.M1[-length(aenet.myl.M1)]
dyErgm.result <- dyCoefErgm.yearly(data =  network.y[1:2], set = set, 
                                   inclusion.edgecov = inclusion.edgecov,
                                   inclusion.nodecov = inclusion.nodecov, inclusion.nodeocov = inclusion.nodeocov, inclusion.nodeicov = inclusion.nodeicov,
                                   inclusion.absdiff = inclusion.absdiff,
                                   inclusion.cyclicalweights = inclusion.cyclicalweights,
                                   inclusion.mutual = inclusion.mutual,
                                   inclusion.nodecovar = inclusion.nodecovar, inclusion.nodeicovar = inclusion.nodeicovar, inclusion.nodeocovar = inclusion.nodeocovar,
                                   inclusion.nodeisqrtcovar = inclusion.nodeisqrtcovar, inclusion.nodeosqrtcovar = inclusion.nodeosqrtcovar, inclusion.nodesqrtcovar = inclusion.nodesqrtcovar,
                                   Date = c("2009-12-31","2010-12-31"),#,"2011-12-31","2012-12-31","2013-12-31","2014-12-31","2015-12-31","2016-12-31"), 
                                   trans = trans, 
                                   BidType = BidType,
                                   MCMLE.maxit = 20,
                                   triangle = triangle,
                                   directed = directed,
                                   tab = F,fig = T,
                                   is.higher = is.higher
)


