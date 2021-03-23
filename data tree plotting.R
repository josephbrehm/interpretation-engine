### Load

library(data.tree)
library(ggplot2)

setwd("E:/NMSU")

load("Engine Guts/datatree-svi.rdata")

### simple plots

plot(tr.svi)

plot(as.dendrogram(tr.svi))

library(igraph)
plot(as.igraph(tr.svi)) # non-unique nodes, this wont work

## wrong for this. 
library(networkD3)
sviNetwork <- ToDataFrameNetwork(tr.svi)
simpleNetwork(sviNetwork[-3], fontSize = 12)

#### fancier basic plot()s 


# from vignette
plot(tr.svi)
SetGraphStyle(tr.svi, rankdir = "TB")
SetEdgeStyle(tr.svi, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(tr.svi, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow",
             fontname = "helvetica", tooltip = GetDefaultTooltip)
#SetNodeStyle(tr.svi$levelName, fillcolor = "LightBlue", penwidth = "5px")
plot(tr.svi)

