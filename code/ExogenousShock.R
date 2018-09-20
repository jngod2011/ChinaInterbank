load(file = "data/Rdata/latex_ALLshiborbid_RawShiborBid.Rdata")
load(file = "data/Rdata/latex_bond.Rdata")
i=8
data <- merge(raw.shibor.bid.Y1[[i]],bond) %>% na.omit

data.before <- data["2013-01-01/2013-06-06"]
data.before <- data.before[,1:17]
data.after <- data["2013-06-07/2013-12-30"]
data.after <- data.after[,1:17]
data.between <- data["2013-01-01/2013-07-20"]
data.between <- data.between[,1:17]
n.bank <- dim(raw.shibor.bid.ON[[i]])[2]

#data <- merge(data.before,bond) %>% na.omit
data <- data.before
m.before <- aenet.fix(data = data, fix = "yearly", start.point = "NULL", end.point = "NULL")$Coef[[1]]
m.before <- replaceNA0(m.before)[1:n.bank,1:n.bank]
View(m.before)


#data <- merge(data.after,bond) %>% na.omit
data <- data.between
m.between <- aenet.fix(data = data, fix = "yearly", start.point = "NULL", end.point = "NULL")$Coef[[1]]
m.between <- replaceNA0(m.between)[1:n.bank,1:n.bank]
View(m.between)



data <- data.before
m.before <- dy.VAR.FEVD(data, n.ahead=1, mode=NULL,span = "yearly")$fevd.matrix[[1]]
data <- data.after
m.after <- dy.VAR.FEVD(data, n.ahead=1, mode=NULL,span = "yearly")$fevd.matrix[[1]]
data <- data.before
m.before <- dy.VAR.GIR(data, n.ahead=1, mode=NULL,span = "yearly")$fevd.matrix[[1]]

data <- data.after
m.after <- dy.VAR.GIR(data, n.ahead=1, mode=NULL,span = "yearly")$gir.matrix[[1]]



temp <- temp
bank.abbr <- bank10.abbr
temp <- matrix(temp %>% unlist,10,10) #%>% t
colnames(temp) <- bank.abbr;rownames(temp) <- bank.abbr
temp <- temp %>% t
temp <- temp/max(temp)
#if(max(temp)>0.02){temp[temp < 0.02] <- 0}
igraph.matrix <- graph_from_adjacency_matrix(temp, mode = c("directed"), weighted = TRUE, diag = FALSE)
networkD3.matrix <- igraph_to_networkD3(igraph.matrix)
networkD3.matrix[[2]]$group <- group.stockprice[bank.abbr,c("Eclass")] %>% as.character

temp.size <- rowSums(temp)#-colSums(temp)
temp.size[temp.size < 0] <- min(temp.size[temp.size > 0])/2
networkD3.matrix[[2]]$size <- temp.size
#networkD3.matrix[[2]]$group <- factor(networkD3.matrix[[2]]$group, order = TRUE, levels = c("State-Owned Banks","Joint-Equity Commercial Banks","Urban Commercial Banks"))
networkD3.matrix[[2]]$size <- networkD3.matrix[[2]]$size*1
networkD3.matrix[[1]]$value <- networkD3.matrix[[1]]$valu*3
#matrix.date <- data.frame(name="",size=0, group=as.character(substr(combas.date[i],1,4)))
#networkD3.matrix[[2]] <- rbind(networkD3.matrix[[2]], matrix.date)

ColourScale <- 'd3.scaleOrdinal() .range(["#B71C1C","#FF9800","#33691E","white"]);'
#rbokeh.interbank <- 
forceNetwork(Links = networkD3.matrix[[1]], Nodes = networkD3.matrix[[2]],
                       Source = "source", Target = "target", Value = "value", 
                       arrows = T, linkDistance = JS("function(d){return 1 / d.value * 100 }"), linkWidth = JS("function(d) { return d.value; }"), linkColour = "gray",
                       NodeID = "name", Nodesize = 'size', radiusCalculation = "d.nodesize", colourScale = JS(ColourScale),
                       Group = "group", legend = T, fontSize = 15, opacity = 0.8, opacityNoHover = 1,
                       height = 700 , width = 700, bounded = F, zoom = T, charge = -100)

#x2 <- dy.VAR.GIR(data, n.ahead=1, mode=NULL, span = "yearly")$gir.matrix[[1]]
