# Introduction -----

# The following notes summarize (mainly) lectures by Prof. Aaron Clauset, University of
# Colorado, Boulder and the Santa-Fe Institute. Full graduate level course lectures notes
# (including further readings) are available online at
# https://aaronclauset.github.io/courses/5352/
  
# The recent rise of the internet and digital social networks has led to a surge in interest
# and research in network analysis. The mathematical basis for analyzing networks is somewhat
# older: Euler's solution to the Seven Bridges of Königsberg riddle, proposed in 1735, is
# considered to be the cornerstone for a branch of mathematics known as **Graph Theory**.

# The city of Königsberg, Prussia (now Kaliningrad, Russia) spanned over two banks of the
# river Pregel, with two islands and seven bridges over the river. Figure 1 shows a
# contemporary map of Kaliningrad and the approximate locations of the original seven bridges
# in red circles.

par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("figure1-600w.png"), axes = FALSE)

# The challenge was to find a path that visits all the four land masses of the city and
# crosses each of the seven bridges only once. Euler abstracted the problem by treating
# only the land masses and the bridges (ignoring, e.g., distances or any other physical
# characteristics). In Figure 2, taken from Euler's original solution, the four land masses
# are denoted by the uppercase letters A-D and the bridges are by the lowercase letters a-f.

plot(imager::load.image("figure2-600w.png"), axes = FALSE)

# A contemporary graphical representation of the same abstraction is suggested in Figure 3.

plot(imager::load.image("figure3-600w.png"), axes = FALSE)

# We will not discuss here the solution to the riddle. Our interests here lie elsewhere:
# in the **abstraction** of a city map – a highly detailed and complex object – to a graph
# comprised of only two types of components, and in the analytical treatment of graphs.

# Some terminology: **graphs** are mathematical objects which have **vertices** (or nodes,
# actors) and **edges** (or links, ties). In our first example the vertices are the land
# masses (A-D in Figures 2 and 3) and the edges are the bridges (a-f in Figures 2 and 3).
# Vertices must be distinct objects, while edges are pairwise relations between these objects.

# The **degree** of a vertex is the number of edges that are connected to it. The term
# network, in our context, will be used interchangeably with the term graph. Thus, for
# example, the term "social network" will remain meaningless lest we define what the
# vertices are, what the edges are, and which vertices are connected by edges. 

# A possible network representation of a social domain may include persons as vertices
# and friendships as edges. However, one may suggest other network representations of the
# same thing.

# Some examples of popular network representations of various phenomena are given in Table 1:

#   Network                  |  Vertex                         |  Edge
# ------------------------------------------------------------------------------------------
#  Internet                  |  Computer                       |  IP network adjacency
#  World Wide Web            |  Web page                       |  Hyperlink
#  Documents                 |  Article, patent or legal case  |  Citation
#  Power grid transmission   |  Generating or relay station    |  Transmission line
#  Rail system               |  Rail station                   |  Railroad tracks
#  Neuronal network          |  Neuron                         |  Synapse
#  Food web                  |  Species                        |  Predation
#  Banking networks          |  Bank                           |  Lending between banks
#  Bitcoin                   |  Bitcoin wallet                 |  Transaction


# Some images that hopefully provide some sense of what abstraction is here:
plot(imager::load.image("neuron-synapse-pic.jpg"), axes = FALSE)
# https://www.psypost.org/wp-content/uploads/2017/03/Neurons-by-Penn-State.jpg
plot(imager::load.image("neuron-synapse-draw.jpg"), axes = FALSE)
# https://pittmedneuro.com/synaptic.html
plot(imager::load.image("neuron-synapse-draw-illustrated.png"), axes = FALSE)
plot(imager::load.image("neuron-synapse-draw-graph.png"), axes = FALSE)

plot(imager::load.image("predator-prey.jpg"), axes = FALSE)
# https://www.ampersand.fr/images/photos_169/2295.JPG


# * Types of networks -----
# There are several types of networks: 
# **Simple Network**: undirected, unweighted and has no self-loops.
# Euler's graph of the Seven Bridges of Königsberg is indeed such a simple network.

# Question: which other networks from Table 1 are simple?
# Answer:


# **Directed Network**: when edges are directed – point from one vertex to 
# another – the network is directed.

# Question: which other networks from Table 1 are directed?
# Answer:


# **Weighted Network**: when different edges are assigned with numbers the network is weighted. 

# Question: which other networks from Table 1 are weighted?
# Answer:


# **Multipartite Networks**: have several types of vertices in a single network. 

# Question: which other networks from Table 1 are multipartite?
# Answer:


# **Multiplex networks**: also have several types of edges in a single network.

# Question: which other networks from Table 1 are multiplex?
# Answer:

# Network Measures -----
# What kind of questions can we ask about networks? What kind of answers does network science
# provide? We will survey here some quick examples of answers to the above questions.

# * Centrality -----
# How important is a vertex? 

# Can we quantitatively discern more important than less important vertices? 
# The concept of centrality is supposed to provide some insight into this question. 
# There are many measures of centrality of vertices. These measures receive network data
# as input, and assign each vertex with a number representing its centrality (different
# measures may provide differing quantifications). 

# Figure 5 shows for example the Eigenvector Centrality measure of a given network 
# structure, which assigns a higher centrality measure to vertices that are connected to
# vertices with a high degree. Google's search engine famous page ranking algorithm is a 
# variant of the Eigenvector centrality measure.

plot(imager::load.image("figure5-600w.png"), axes = FALSE)
# Source: adapted from wikipedia.org

# * Community Structure -----
# Community, of course, is a rich notion that cannot be fully covered by a network 
# representation or any all-encompassing quantitative measure. In the context of network 
# analysis, community structure is a group of vertices that connects with other groups of 
# vertices in similar ways. We'll provide two examples of community structures:

# Suppose our vertices are divided into subgroups. To what extent are vertices more highly 
# connected to vertices of the same kind? The **assortativity** coefficient quantifies exactly 
# this. High assortativity indicates that vertices of the same kind have many edges between 
# each other. The next figure demonstrates how a highly assortative network may look like:

plot(imager::load.image("figure6-600w.png"), axes = FALSE)

# The presence or absence of an assortative community structures is of high importance in, 
# e.g., the study of epidemics. Define a network such that persons are vertices and edges are 
# interactions amongst persons that allow infection with a disease. If an assortative community 
# structure exists and vaccination is at hand, then the spread of infectious diseases may be 
# more easily controlled by vaccinating the persons that correspond to the vertices that 
# connect the subgroups of vertices, thus containing the disease in a smaller group.

# Another widely discussed community structure is the **core-periphery** community structure. 
# A core-periphery structure exists when a group of vertices that are densely connected to one 
# another (the core) is connected to a sparser group (the periphery). 
# The next figure demonstrates how a network with a clear core-periphery community structure 
# may look like.

plot(imager::load.image("figure7-600w.png"), axes = FALSE)

#  Della Rossa, Dercole & Piccardi have demonstrated how network core-periphery characterization
#  when applied to world trade data, generates the following core-periphery world map.

plot(imager::load.image("figure8-600w.png"), axes = FALSE)

# Other major topics in networks science involve, for exmaple: 
# * The evolution of networks over time
# * Building statistical generative models that may shed light on the processes 
#   that underlie the development of different kinds of networks.
# * Developing algorithms for predicting missing links
# * Detecting network change points over time ("phase transitions").

# There are many additional concepts and research interests in network science that we will 
# not mention here. 

# In a nutshell, a successful network research depends on a good definition of the network,
# one that will allow interesting insights to arise from the various established network
# analyses.

# Visualizing networks in R -----

# Adapted from: www.kateto.net/sunbelt2021

# * Data types for network structures -----

# ** Edge/node list -----

# For a simple network, all we need is the following:
library(tidyverse)
# Read in the data:
nodes_min <- read_csv("Dataset1-Media-Example-NODES_min.csv")
links_min <- read_csv("Dataset1-Media-Example-EDGES_min.csv")

# Examine the data:
nodes_min
# Each row represents a node.

links_min
# Each row represents an edge.

# Question: Why do we need the two files? (i.e. why not only take the nodes that 
# appear in the links dataframe and use it as our set of nodes?)

# Answer:

# Question: How can we make this into a directed network?
# Answer: 





# ** Adjacency matrix -----

# A different data structure that represents networks is the **adjacency matrix**.
# It is a square matrix in which the labels for each row denote a node, as well as the
# labels for each column. The values in the cells denote links, where 0 indicates that
# there is no link.

# Example:
plot(imager::load.image("adjacency.png"), axes = FALSE)

A <- matrix(
  c(
    0, 0, 0, 0, 1,
    1, 0, 0, 0, 1,
    0, 0, 1, 1, 1,
    1, 1, 0, 0, 0,
    0, 1, 0, 0, 0
  ),
  byrow = TRUE,
  nrow = 5
)
node_names <- c("A", "B", "C", "D", "E")
rownames(A) <- node_names
colnames(A) <- node_names
A

library(plotly)
plot_ly() %>% add_heatmap(x = rownames(A), y = colnames(A), z = A, showscale = FALSE)


# Question: what are the properties of the adjacency matrix of simple network?
# Answer: 


# Adjacency matrices can convey valuable information visually too!

# Example:
library(igraphdata)
data("UKfaculty")
# The personal friendship network of a faculty of a UK university, consisting of 
# 81 vertices (individuals) and 817 directed and weighted connections. The school 
# affiliation of each individual is stored as a vertex attribute. This dataset can 
# serve as a testbed for community detection algorithms.

library(igraph)
(sparse_matrix_UKfaculty <- as_adj(UKfaculty))
(matrix_UKfaculty <- as.matrix(sparse_matrix_UKfaculty))


plot_ly() %>% 
  add_heatmap(
    x = rownames(matrix_UKfaculty), 
    y = colnames(matrix_UKfaculty), 
    z = matrix_UKfaculty
    ) %>%
  hide_colorbar()
# Not too informative. 

# However, when we use some algorithm for ordering the rows/columns, we can get:
library(dendextend)
row_dend  <- 
  matrix_UKfaculty %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() %>%
  ladderize()

col_dend  <- 
  matrix_UKfaculty %>% 
  t() %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() %>%
  ladderize()

library(heatmaply)
heatmaply(
  matrix_UKfaculty,
  Rowv = row_dend,
  Colv = col_dend   
)

# For more on dendrograms and heatmaps in plotly:
# https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html

# The igraph library -----

library(igraph)
# igraph (https://igraph.org/) is a popular C library for network analysis. It has interfaces
# for R, python and Mathematica.

# * igraph data -----
# Network structures are stored internally as igraph objects.

# Let's read a fuller version of the lists of nodes and links:
library(tidyverse)
(nodes <- read_csv("Dataset1-Media-Example-NODES.csv"))
(links <- read_csv("Dataset1-Media-Example-EDGES.csv"))

# Convert the data to an igraph object:

# The graph_from_data_frame() function takes two data frames: 'd' and 'vertices'.
# 'd' describes the edges of the network - it should start with two columns 
# containing the source and target node IDs for each network tie.

# 'vertices' should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.

net <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE) 

# Examine the resulting object:
class(net)
net 

# "IGRAPH" denotes that this is an igraph graph. 
# Then come four bits that denote the kind of the graph: 
# the first is "U" for undirected and "D" for directed graphs. 
# The second is "N" for named graph (i.e. if the graph has the 
# "name" vertex attribute set). 
# The third is "W" for weighted graphs (i.e. if the "weight" edge 
# attribute is set). 
# The fourth is "B" for bipartite graphs (i.e. if the ‘type’ vertex 
# attribute is set).

# Then come two numbers, the number of vertices and the number of edges 
# in the graph, and after a double dash, the name of the graph (the "name" 
# graph attribute) is printed if present. 

# The second line is optional and it contains all the attributes of the graph. 

# We can access the nodes, edges, and their attributes:
E(net)
V(net)
E(net)$type
V(net)$media

# Or find specific nodes and edges by attribute:
# (that returns objects of type vertex sequence / edge sequence)
V(net)[media == "BBC"]
E(net)[type == "mention"]


# If you need them, you can extract an edge list 
# or a matrix back from the igraph networks.
as_edgelist(net, names = TRUE)
as_adjacency_matrix(net, attr = "weight")

# Or data frames describing nodes and edges:
igraph::as_data_frame(net, what = "edges")
igraph::as_data_frame(net, what = "vertices")

# You can also look at the network matrix directly:
net[1, ]
net[5, 7]

# First attempt to plot the graph:
plot(net) # not pretty!

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size = 0.4, vertex.label = NA)

# For some networks, self loops are of no interest.
# To remove loops from the graph:
net_no_loops <- igraph::simplify(net, remove.multiple = FALSE, remove.loops = TRUE)
plot(net_no_loops, edge.arrow.size = 0.4, vertex.label = NA)

# * igraph plots -----

?igraph.plotting

# We can set the node & edge options in two ways - one is to specify
# them in the plot() function, as we are doing below.

# Plot with curved edges and reduce arrow size:
plot(net, edge.arrow.size = 0.4, edge.curved = 0.1)

# Set node color to orange and the border color to hex #555555
# Replace the vertex label with the node names stored in "media"
plot(
  net, 
  edge.arrow.size = 0.2, 
  edge.curved = 0,
  vertex.color = "orange", 
  vertex.frame.color = "#555555",
  vertex.label = V(net)$media, 
  vertex.label.color = "black",
  vertex.label.cex = 0.7
  ) 

# The second way to set attributes is to add them to the igraph object.

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode = "all")
V(net)$size <- deg * 3
# Alternatively, we can set node size based on audience size:
V(net)$size <- V(net)$audience.size * 0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight / 6

# change arrow size and edge color:
E(net)$arrow.size <- 0.2
E(net)$edge.color <- "gray80"

# Set the network layout:
graph_attr(net, "layout") <- layout_with_lgl
plot(net) 

# We can also override the attributes explicitly in the plot:
plot(net, edge.color = "orange", vertex.color = "gray50") 


# Sometimes, especially with semantic networks, we may be interested in 
# plotting only the labels of the nodes:

plot(net, vertex.shape = "none", vertex.label = V(net)$media, 
     vertex.label.font = 2, vertex.label.color = "gray40",
     vertex.label.cex = 0.7, edge.color = "gray85")


# Let's color the edges of the graph based on their source node color.
# We'll get the starting node for each edge with "ends()".
edge.start <- ends(net, es = E(net), names = FALSE)[ , 1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color = edge.col, edge.curved = 0.1)



# * igraph layouts ----

# Network layouts are algorithms that return coordinates for each
# node in a network.

# Let's generate a slightly larger 100-node graph using 
# a preferential attachment model (Barabasi-Albert).

set.seed(1)
net.bg <- sample_pa(100, 1.2) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

# Now let's plot this network using the layouts available in igraph.

# You can set the layout in the plot function:
plot(net.bg, layout = layout_randomly)

# Or calculate the vertex coordinates in advance:
(l <- layout_in_circle(net.bg))
plot(net.bg, layout = l)

# l is simply a matrix of x,y coordinates (N x 2) for the N nodes in the graph.
# For 3D layouts, it is matrix of x, y, and z coordinates (N x 3)

# You can generate your own:
# For instance, to highlight the links of a specific node:
(l <- cbind(1:vcount(net.bg), c(1, vcount(net.bg):2)))
plot(net.bg, layout = l)

# Or use other built-in layouts:
# Randomly placed vertices:
l <- layout_randomly(net.bg)
plot(net.bg, layout = l)

# 3D sphere layout
l <- layout_on_sphere(net.bg)
plot(net.bg, layout = l)


# Fruchterman-Reingold layout
# Like other force-directed algorithms, it treats graphs as a physical system. 
# Nodes are electrically charged particles that repulse each other when they
# get too close. Edges act as springs that pull connected nodes closer.
# F-R is nice but slow, most often used in graphs smaller than ~1000 vertices.
l <- layout_with_fr(net.bg)
plot(net.bg, layout = l)


# With force-directed layouts, you can use the 'niter' parameter to control
# the number of iterations to perform. The default is 500, but you can lower
# it for large graphs to get results faster and check if they look reasonable.
l <- layout_with_fr(net.bg, niter = 50)
plot(net.bg, layout = l)

# The layout can also use edge weights. It checks for a 'weight' edge attribute
# in the network object, or you can use a 'weights' parameter in the function.
# Nodes connected by more heavily weighted edges are pulled closer together.
ws  <-  c(1, rep(100, ecount(net.bg)-1))
lw <- layout_with_fr(net.bg, weights = ws)
plot(net.bg, layout = lw) 


# You will also notice that the F-R layout is not deterministic - different 
# runs will result in slightly different configurations. Saving the layout 
# in l allows us to get the exact same result multiple times.
par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)

dev.off()

# By default, the coordinates of the plots are rescaled to the [-1,1] interval
# for both x and y. You can change that with the parameter "rescale = FALSE"
# and rescale your plot manually by multiplying the coordinates by a scalar.
# You can use norm_coords to normalize the plot with the boundaries you want.
# This way you can create more compact or spread out layout versions.

# Get the layout coordinates:
l <- layout_with_fr(net.bg)
# Normalize them so that they are in the -1, 1 interval:
l <- norm_coords(l, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

par(mfrow = c(2, 2), mar = c(0, 0, 0, 0))
plot(net.bg, rescale = FALSE, layout = l * 0.4)
plot(net.bg, rescale = FALSE, layout = l * 0.8)
plot(net.bg, rescale = FALSE, layout = l * 1.2)
plot(net.bg, rescale = FALSE, layout = l * 1.6)
dev.off()

# Some layouts have 3D versions that you can use with parameter 'dim = 3'
l <- layout_with_fr(net.bg, dim = 3)
plot(net.bg, layout = l)

# As you might expect, a 3D layout has 3 columns, for X, Y, and Z coordinates:
l

# Another popular force-directed algorithm that produces nice results for
# connected graphs is Kamada Kawai. Like Fruchterman Reingold, it attempts to 
# minimize the energy in a spring system.
l <- layout_with_kk(net.bg)
plot(net.bg, layout = l)

# Graphopt is a force-directed layout that uses layering 
# to help with visualizations of large networks. 
l <- layout_with_graphopt(net.bg)
plot(net.bg, layout = l)

# Graphopt parameters can change the mass and electric charge of nodes
# as well as the optimal spring length and the spring constant for edges.
# The parameter names are 'charge' (default 0.001),  'mass' (default 30), 
# 'spring.length' (default 0), and 'spring.constant' (default 1).
# Tweaking those can lead to considerably different graph layouts.

# For instance, the charge parameter below changes node repulsion:
l1 <- layout_with_graphopt(net.bg, charge = 0.02)
l2 <- layout_with_graphopt(net.bg, charge = 0.00000001)

par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
plot(net.bg, layout = l1)
plot(net.bg, layout = l2)

dev.off() 


# The MDS (multidimensional scaling) algorithm tries to place nodes based on some
# measure of similarity or distance between them. More similar nodes are plotted 
# closer to each other. By default, the measure used is based on the shortest 
# paths between nodes in the network. That can be changed with the 'dist' parameter.
plot(net.bg, layout = layout_with_mds)

# The LGL algorithm is for large connected graphs. Here you can specify a root - 
# the node that will be placed in the middle of the layout.
plot(net.bg, layout = layout_with_lgl)


# By default, igraph uses a layout called 'layout_nicely' which selects
# an appropriate layout algorithm based on the properties of the graph. 

# Check out all available layouts in igraph:
?igraph::layout_


# Exercise:
# Use igraph to plot several layouts for the UKfaculty network,
# set the color of a node according to its centrality.

# Comment on what each layout emphasizes.

# Answer:
data("UKfaculty")



# * Highlighting graph features -----
(nodes <- read_csv("Dataset1-Media-Example-NODES.csv"))
(links <- read_csv("Dataset1-Media-Example-EDGES.csv"))
net <- graph_from_data_frame(d = links, vertices = nodes, directed = FALSE) 

# We can also try to make the network map more useful by
# showing the communities within it.

# Community detection:
clp <- cluster_leiden(net)
class(clp)
clp$membership

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- 
  adjustcolor(c("gray50", "tomato", "gold", "yellowgreen"), alpha = 0.6)
plot(net, vertex.color = colrs[V(net)$community])

# Highlighting specific nodes or links:

# Sometimes we want to focus the visualization on a particular node
# or a group of nodes. Let's represent distance from the NYT:
dist.from.NYT <- 
  distances(
    net, 
    v = V(net)[media == "NY Times"],  
    to = V(net),
    weights = NA
    )

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT) + 1)
col <- col[dist.from.NYT + 1]

plot(
  net, 
  vertex.color = col, 
  vertex.label = dist.from.NYT, 
  edge.arrow.size = 0.6, 
  vertex.label.color = "white"
  )


# We can also highlight paths between the nodes in the network.
# Say here between MSNBC and the New York Post:
news.path <- 
  shortest_paths(
    net,
    from = V(net)[media == "MSNBC"], 
    to  = V(net)[media == "New York Post"],
    output = "both"
    ) # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(
  net,
  vertex.color = vcol, 
  edge.color = ecol, 
  edge.width = ew, 
  edge.arrow.mode = 0
  )


# Highlight the edges going into or out of a vertex, for instance the WSJ.
# For a single node, use 'incident()', for multiple nodes use 'incident_edges()'
inc.edges <- 
  incident(net, V(net)[media == "Wall Street Journal"], mode = "all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$media == "Wall Street Journal"] <- "gold"
plot(net, vertex.color = vcol, edge.color = ecol)

# Or we can highlight the immediate neighbors of a vertex, say WSJ.
# The 'neighbors' function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'.
# To find node neighborhoods going more than one step out, use function 'ego()'
# with parameter 'order' set to the number of steps out to go from the focal node(s).

neigh.nodes <- 
  neighbors(net, V(net)[media == "Wall Street Journal"], mode = "out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color = vcol)

# Another way to draw attention to a group of nodes:
plot(net, mark.groups = c(1,4,5,8), mark.col = "#C5E5E7", mark.border = NA)
# Mark multiple groups:
plot(
  net, 
  mark.groups = list(c(1,4,5,8), c(15:17)), 
  mark.col = c("#C5E5E7","#ECD89A"), 
  mark.border = NA
  )



# * tkplot -----

# R and igraph offer interactive plotting capabilities
# (mostly helpful for small networks)

tkid <- tkplot(net) # tkid is the id of the tkplot

# ggraph ----- 

# The `ggraph` package extends `ggplot` to network data. 

library(ggraph)
library(igraph)

# We can use our 'net' igraph object directly with the 'ggraph' package.
# The following code gets the data and adds layers for nodes and links.
(nodes <- read_csv("Dataset1-Media-Example-NODES.csv"))
(links <- read_csv("Dataset1-Media-Example-EDGES.csv"))
net <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE) 
V(net)$color <- colrs[V(net)$media.type]

ggraph(net) +
  geom_edge_link() +   # add edges to the plot
  geom_node_point()    # add nodes to the plot


# You will recognize some graph layouts familiar from igraph plotting:
# 'star', 'circle', 'grid', 'sphere', 'kk', 'fr', 'mds', 'lgl', etc.

ggraph(net, layout = "lgl") +
  geom_edge_link() +
  ggtitle("No nodes!")  # add title to the plot


# Use geom_edge_link() for straight edges, geom_edge_arc() for curved ones, and
# geom_edge_fan() to make sure any overlapping multiplex edges will be fanned out.
# As in other packages, here we can set visual properties for the network plot by
# using various parameters, for nodes ('color', 'fill', 'shape', 'size', 'stroke') 
# and edges ('color', 'width', 'linetype'). Here too 'alpha' controls transparency.

ggraph(net, layout = "lgl") +
  geom_edge_fan(color = "gray50", width = 0.8, alpha = 0.5) + 
  geom_node_point(color = V(net)$color, size = 8) +
  theme_minimal()

# As in ggplot2, we can add different themes to the plot. For a cleaner look,
# you can use an empty theme with theme_minimal() and theme_void() 
g <-
ggraph(net, layout = "linear") + 
  geom_edge_arc(color = "orange", width = 0.7) +
  geom_node_point(size = 5, color = "gray50") +
  theme_void()


# 'ggraph' also uses the 'ggplot2' way of mapping aesthetics: that is  
# to say specifying which elements of the data should correspond to different 
# visual properties of the graphic. This is done using the aes() function
# that matches visual parameters with attribute names from the data.

ggraph(net, layout = "lgl") +
  geom_edge_link(aes(color = type)) +           # colors by edge type 
  geom_node_point(aes(size = audience.size)) +  # size by audience size  
  theme_void()

# The edge attribute 'type' and node attribute 'audience.size' are 
# taken from our data as they are included in the igraph object 'net'

# We can add node labels with geom_node_text() or geom_node_label():
ggraph(net,  layout = "lgl") +
  geom_edge_arc(color = "gray", strength = 0.3) +            
  geom_node_point(color = "orange", aes(size = audience.size)) +
  geom_node_text(aes(label = media), color = "gray50", repel = TRUE) +
  theme_void()

# Note that 'ggraph' offers a number of other interesting ways to represent
# networks (e.g. dendrograms, treemaps, hive plots, circle plots)



# Interactive JavaScript networks -----

# There are a number of libraries like 'rcharts' and 'htmlwidgets' that can help you 
# export interactive web charts from R. We'll take a quick look at three packages that
# can export networks from R to JavaScript: : 'visNetwork' and 'threejs', and 'networkD3'

# But first, the plotly!

# * plotly -----

# We can utilize an igraph layout to create interactive network visualizations with plotly
library(plotly)
(nodes <- read_csv("Dataset1-Media-Example-NODES.csv"))
(links <- read_csv("Dataset1-Media-Example-EDGES.csv"))
net <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE) 

# First, the nodes:
l <- layout_with_fr(graph = net)
colnames(l) <- c("x", "y")
nodes_w_layout <- tibble(nodes, as_tibble(l))

(plotly_network <- 
    plot_ly() %>%
    add_markers(
      data = nodes_w_layout,
      x = ~x, 
      y = ~y, 
      text = ~media, 
      hoverinfo = "text",
      size = ~audience.size
      ))

# Question: why not use the `color` mapping?

axis <- 
  list(
    title = "", 
    showgrid = FALSE, 
    showticklabels = FALSE, 
    zeroline = FALSE
  )

plotly_network %>% 
  layout(
    xaxis = axis,
    yaxis = axis
  )

# Exercise:
# use `add_segments` to add the links.
# Hint: you'd need to use left_join twice.
# Next, add segments:

# Another option is to use ggnet2:
# library(network)
library(intergraph) # to silently convert the `igraph` to `network`
library(GGally)
ggnet_network <- 
  ggnet2(
    net
    # more options
  )

ggplotly(ggnet_network) %>%
  layout(
    xaxis = axis,
    yaxis = axis
  ) %>%
  config(scrollZoom = TRUE)


# * visNetwork -----

library(visNetwork) 

# We can visualize the network right away - visNetwork() will accept 
# our node and link data frames (it needs node data with an 'id' column,
# and edge data with 'from' and 'to' columns).

visNetwork(nodes, links)

# We can set the height and width of the visNetwork() window 
# with parameters 'height' and 'width', the back color with 'background',
# the title, subtitle, and footer with 'main', 'submain', and 'footer'

visNetwork(
  nodes, 
  links, 
  height = "300px", 
  width = "100%", 
  background = "#eeefff",
  main = "Network", 
  submain = "And what a great network it is!",
  footer= "Hyperlinks and mentions among media sources"
  )

# Like 'igraph' did, 'visNetwork' allows us to set graphic properties 
# as node or edge attributes directly in the data or through a function.

# Check out the available options with:
?visNodes
?visEdges

# We'll start by adding new node and edge attributes to our dataframes. 
vis.nodes <- nodes
vis.links <- links

# The options for node shape include 'ellipse', 'circle', 
# 'database', 'box', 'text', 'image', 'circularImage', 'diamond', 
# 'dot', 'star', 'triangle', 'triangleDown', 'square', and 'icon'

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$media # Text on click
vis.nodes$label  <- vis.nodes$type.label # Node label
vis.nodes$size   <- vis.nodes$audience.size # Node size
vis.nodes$borderWidth <- 2 # Node border width

# We can set the color for several elements of the nodes:
# "background" changes the node color, "border" changes the frame color;
# "highlight" sets the color on click, "hover" sets the color on mouseover.

vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visNetwork(vis.nodes, vis.links)

# Below we change some of the visual properties of the edges:
vis.links$width <- 1 + links$weight / 8 # line width
vis.links$color <- "gray"    # line color  
vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow

visNetwork(vis.nodes, vis.links)

# Remove the arrows and set the edge width to 1:
vis.links$arrows <- "" 
vis.links$width  <- 1
visnet <- visNetwork(vis.nodes, vis.links)
visnet

# We can also set the visualization options directly with visNodes() and visEdges()
visNetwork(nodes, links) %>%
  visNodes(
    shape = "square", 
    shadow = TRUE, 
    color = 
      list(
        background = "gray", 
        highlight = "orange", 
        border = "black"
        )
    ) %>%
  visEdges(
    color = 
      list(
        color = "black", 
        highlight = "orange"
        ),
    smooth = FALSE, 
    width = 2, 
    dashes = TRUE, 
    arrows = "middle"
    ) 


# 'visNetwork' can also work with predefined groups of nodes.
# Visual characteristics for each group can be set with visGroups().
nodes$group <- nodes$type.label 
visNetwork(nodes, links) %>%
  visGroups(groupname = "Newspaper", shape = "square",
            color = list(background = "gray", border="black")) %>%
  visGroups(groupname = "TV", shape = "dot",
            color = list(background = "tomato", border="black")) %>%
  visGroups(groupname = "Online", shape = "diamond",   
            color = list(background = "orange", border="black")) %>%
  visLegend(main="Legend", position="right", ncol=1) 


# For more information, check out:
?visOptions # available options 
# Notable ones:
# highlightNearest
# nodesIdSelection
?visLayout  # available layouts
?visIgraphLayout # make netwrok and compute igraph layouts
?`visNetwork-igraph`  # make networked with precomputed igraph layouts
?`visNetwork-collapse` # options for collapsing nodes
?`visNetwork-shiny` # integration with shiny
?visGroups  # using node groups
?visLegend  # adding a legend


# Exercise:
# Create a nice representation of the UKFaculty graph using visNetwork.
# Consider that it is a directed and weighted network. 
# Try some community detection algorithm from igraph with it.

# Some ingredients:
library(igraph)
library(igraphdata)
data("UKfaculty")

bb <- babynames::babynames
V(UKfaculty)$label <- sample(bb$name, vcount(UKfaculty))

vg <- toVisNetworkData(UKfaculty, idToLabel = FALSE)
vg$nodes
vg$edges

# igraph community detection:
# Which works for directed and/or weighted network?
?cluster_edge_betweenness()
?cluster_fast_greedy()
?cluster_label_prop()
?cluster_leading_eigen() 
?cluster_louvain() 
?cluster_leiden() 
?cluster_optimal() 
?cluster_spinglass() 
?cluster_walktrap()



# * threejs -----

# Another package exporting networks from R to a js library is 'threejs'
# The nice thing about it is that it can read igraph objects.

# If you get errors or warnings using this library and the latest R version,
# try installing the development version of the 'htmlwidgets' package
# devtools::install_github('ramnathv/htmlwidgets')


library(threejs)
library(htmlwidgets)
library(igraph)

# The main network plotting function - graphjs() will take an igraph object.
# We could use our initial 'net' object with a slight modification - we will
# delete its graph layout and let 'threejs' generate its own layout.
# (We cheated a bit by assigning a function to the layout attribute above 
# rather than giving it a table of node coordinates. This is fine by 'igraph',
# but 'threejs' will not let us do it.

net.js <- net
graph_attr(net.js, "layout") <- NULL

# Note that RStudio for Windows may not render the 'threejs' graphics properly.
# We will save the output in an HTML file and open it in a browser.
# Some of the parameters that we can add include 'main' for the plot title;
# 'curvature' for the edge curvature; 'bg' for background color; 
# 'showLabels' to set labels to visible (TRUE) or not (FALSE); 
# 'attraction' and 'repulsion' to set how much nodes attract and repulse
# each other; 'opacity' for node transparency (0 to 1); 'stroke' to indicate
# whether nodes should be framed in a black circle (TRUE) or not (FALSE), etc.
# For the full list of parameters, check out ?graphjs

gjs <- graphjs(net.js, main="Network!", bg="gray10", showLabels=F, stroke=F, 
               curvature=0.1, attraction=0.9, repulsion=0.8, opacity=0.9)
print(gjs)
saveWidget(gjs, file="Media-Network-gjs.html")
browseURL("Media-Network-gjs.html")

# Once we open the resulting visualization in the browser, we can use the mouse to
# control it: scrollwheel to zoom in and out, the left mouse button to rotate 
# the network, and the right mouse button to pan. 

# We can also create simple animations with 'threejs' by using lists of
# layouts, vertex colors, and edge colors that will switch at each step.

gjs.an <- graphjs(net.js, bg="gray10", showLabels=F, stroke=F, 
                  layout=list(layout_randomly(net.js, dim=3),
                              layout_with_fr(net.js,  dim=3),
                              layout_with_drl(net.js, dim=3),  
                              layout_on_sphere(net.js)),
                  vertex.color=list(V(net.js)$color, "gray", "orange", V(net.js)$color),
                  main=list("Random Layout", "Fruchterman-Reingold", "DrL layout", "Sphere" ) )
print(gjs.an)
saveWidget(gjs.an, file="Media-Network-gjs-an.html")
browseURL("Media-Network-gjs-an.html")

# Another example is the 'Les Miserables' network included with the package:

data(LeMis)
lemis.net <- graphjs(LeMis, main="Les Miserables", showLabels=T)
print(lemis.net)
saveWidget(lemis.net, file="LeMis-Network-gjs.html")
browseURL("LeMis-Network-gjs.html")

# https://www.rdocumentation.org/packages/threejs/versions/0.3.3/topics/globeOutput


# * networkD3 -----

# Another package using JavaScript to export networks: networkD3
# https://christophergandrud.github.io/networkD3/
# install.packages("networkD3")

library(networkD3) 

# d3ForceNetwork expects node IDs that are numeric and start from 0
# so we have to transform our character node IDs:

links.d3 <- 
  data.frame(
    from = as.numeric(factor(links$from)) - 1,
    to = as.numeric(factor(links$to)) - 1
    )

# The nodes need to be in the same order as the "source" column in links:
nodes.d3 <- 
  cbind(idn = factor(nodes$media, levels = nodes$media), nodes)

# The `Group` parameter is used to color the nodes.
# Nodesize is not (as you might think) the size of the node, but the
# number of the column in the node data that should be used for sizing.
# The `charge` parameter guides node repulsion (if negative) or 
# attraction (if positive).

forceNetwork(
  Links = links.d3, 
  Nodes = nodes.d3, 
  Source = "from", 
  Target = "to",
  NodeID = "idn", 
  Group = "type.label",
  linkWidth = 1,
  linkColour = "#afafaf", 
  fontSize = 12, 
  zoom = TRUE, 
  legend = TRUE,
  Nodesize = 6, 
  opacity = 1, 
  charge = -600, 
  width = 600, 
  height = 600
  )
