install.packages("igraph")
install.packages("networkD3")
install.packages("qgraph")
install.packages("data.tree")
install.packages("rattle")
install.packages("GGally")
################## start

library(igraph)

setwd("C:/Users/Pushpita/Documents/R")

####

dat=read.csv("DSM11.csv",header=TRUE,row.names=1,check.names=FALSE)
# choose an adjacency matrix from a .csv file
m=as.matrix(dat) # coerces the data set as a matrix
g=graph.adjacency(m,mode="undirected",weighted=NULL)
g
# this will create an 'igraph object'
par(mar=c(0,0,0,0) -0.0,mgp=c(0,0,0), bg="White")
#V(g)$size=degree(g)*1.5
degree(g)
V(g)$color <-  ifelse(V(g)$name %in% c("1","2"), "blue", "#FBB4AE")
V(g)$size <-  ifelse(V(g)$name %in% c("1","2","24","8"), 35,20)

par(mar=c(0,0,0,0) -0.0,mgp=c(0,0,0), bg="White")
plot(g,layout=layout_with_mds)
#Sugiyama 
#plot(g,layout=layout_with_sugiyama)
#par(mar=c(0,0,0,0) -0.0,mgp=c(0,0,0), bg="White")
#plot.igraph(g,vertex.label.font = 3, vertex.label.cex = 0.7, vertex.color = '#FBB4AE', vertex.label.color = '#101010',layout=layout.auto)
#layout.davidson.harel
#layout_on_grid

tkplot (g, layout=layout_with_mds)

############### edge color 
#g <- simplify(g, remove.multiple = F, remove.loops = T)
par(mar=c(0,0,0,0) -0.0,mgp=c(0,0,0), bg="White")
#plot(g,layout=layout.auto)
#degree(g)

edge.start <- ends(g, es=E(g), names=F)[,1]
edge.start
edge.col <- V(g)$color[edge.start]
plot(g, edge.color=edge.col,layout=layout.circle)
tkplot (g, layout=layout_with_mds, edge.color=edge.col)


###############################

dat1=read.csv("P2P.csv",header=TRUE,row.names=1,check.names=FALSE)
# choose an adjacency matrix from a .csv file
m1=as.matrix(dat1) # coerces the data set as a matrix
m1
g1=graph.adjacency(m1,mode="undirected",weighted=NULL)
degree(g1) 
par(mar=c(0,0,0,0) -0.0,mgp=c(0,0,0), bg="White")
#V(g1)$size=degree(g1)*5
plot.igraph(g1,layout=layout.circle, vertex.label.cex = 0.9)


####################

dat2=read.csv("P2P.csv",header=TRUE,row.names=1,check.names=FALSE)
# choose an adjacency matrix from a .csv file

gb=graph.adjacency(m1,mode="undirected",weighted=NULL)
gb
#V(gb)$size=degree(gb)*3

V(gb)$color <-  ifelse(V(gb)$name %in% c("eng106","eng013","eng087","eng244"), "blue", "#FBB4AE")
V(gb)$size <-  ifelse(V(gb)$name %in% c("eng106","eng013","eng087","eng244"),32,25)
#gb <- simplify(gb, remove.multiple = F, remove.loops = T)


## Edges are a bit trickier
#el1 <- apply(get.edgelist(g1), 1, paste, collapse="-")
  
#E(gb)$color<-ifelse(E(gb)$name %in% c("41"),"red","black")
#E(g)$color[E(gb)$weight == 12] <- 'green'

plot(gb,layout=layout.circle,edge.color="black")

tkplot (gb,layout=layout.circle,edge.color="black")

#Sugiyama


dat3=read.csv("DSMFlyingcar.csv",header=TRUE,check.names=FALSE)
# choose an adjacency matrix from a .csv file
m2=as.matrix(dat3) # coerces the data set as a matrix
m2
g22=graph.adjacency(m2,mode="undirected",weighted=NULL)
degree(g22) 
par(mar=c(0,0,0,0) -0.0,mgp=c(0,0,0), bg="White")
#V(g1)$size=degree(g1)*5
plot.igraph(g22,layout=layout.circle, vertex.label.cex = 0.9)
V(g22)$color <-  ifelse(V(g22)$name %in% c("Batteries","Sensors","Harnesses"), "red", "#FBB4AE")
V(g22)$size <-  ifelse(V(g22)$name %in% c("Batteries","Sensors","Harnesses"),35,30)

edge.start1 <- ends(g22, es=E(g22), names=F)[,1]
edge.col1 <- V(g22)$color[edge.start]
edge.col1
tkplot(g22, vertex.label.cex = 0.9, edge.width=1)

dev.off()
