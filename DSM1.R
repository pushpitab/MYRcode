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
# this will create an 'igraph object'
plot(g)
g <- simplify(g, remove.multiple = F, remove.loops = T)
plot(g,layout=layout.auto)
###############################
tkid <- tkplot(g) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=l)
##################################


g=graph.adjacency(m,mode="undirected")
plot(g)
g <- simplify(g, remove.multiple = F, remove.loops = T)
plot(g,layout=layout.lgl)

##################
.libPaths(c( "C:/Users/Pushpita/Documents/R/win-library/3.4", "C:/Program Files/R/R-3.4.2/library"))
library(igraph)

Relationships=read.csv("RelationTree.csv",header=TRUE)

names(Relationships) <- c( "id","children","siblings")

g1 <- graph.data.frame(d = Relationships,directed = TRUE)

g1

plot(g1,layout=layout.kamada.kawai);
#png( "mygraph2.png", width = 2000, height = 2000 )
g1

g1 <- simplify(g1, remove.loops = T)

plot(g1,
     vertex.label.font = 3, vertex.label.cex = 0.7, 
     edge.width = 1,layout = layout.auto,
     edge.arrow.size = 0.3, vertex.color = '#FBB4AE', vertex.label.color = '#101010'
     )


dev.off()
plot(g1, vertex.size=12, vertex.color = rainbow(10, .8, .8, alpha= .8),
     vertex.label.color = "black", vertex.label.cex = 0.4, vertex.label.degree = -pi/2,
     edge.arrow.size = 0.3, edge.arrow.width = 0.4, edge.color = "black", 
     layout = layout_as_tree)



library(igraph)

id = 1:5
parent = c(1,1,2,3,3)
#name = c("A", "B", "C", "D", "E")
data = data.frame(id, parent)
data
g = graph.data.frame(data)
plot <- plot.igraph(g,vertex.label = name)



Relationship1=read.csv("Tree.csv",header=TRUE)

id1=Relationship1$id
parent1=Relationship1$parents
data1 = data.frame(id1, parent1)
data1
g1 = graph.data.frame(data1)
g1 <- simplify(g1, remove.loops = T)

plot <- plot.igraph(g1,layout =layout_with_fr,
                    vertex.size=10,edge.arrow.width = 0.4,
                    edge.arrow.size = 0.3,vertex.label.cex = 0.7)
