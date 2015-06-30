## ---- setup, echo = FALSE, message = FALSE-------------------------------
library(knitr)
library(igraph)
opts_chunk$set(
  prompt = FALSE,
  comment = "#>",
  tidy = FALSE)
options(width = 73)
igraph_options(graph.margin = 0, margin = 0)

## ----echo = FALSE, results = "hide", message = FALSE---------------------
set.seed(42)
library(igraph)
library(igraphdata)
data(karate)
par(mar=c(0,0,0,0))
plot(karate, margin = 0)

## ----message = FALSE-----------------------------------------------------
library(igraph)

## ------------------------------------------------------------------------
toy1 <- make_graph(~ A - B, B - C - D, D - E:F:A, A:B - G:H)
toy1

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0)); plot(toy1)

## ------------------------------------------------------------------------
toy2 <- make_graph(~ A -+ B, B -+ C -+ D +- A:B)
toy2

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0)); plot(toy2)

## ------------------------------------------------------------------------
toy2

## ------------------------------------------------------------------------
make_ring(5)

## ------------------------------------------------------------------------
make_ring(5)

## ------------------------------------------------------------------------
A <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10)
A

## ------------------------------------------------------------------------
graph_from_adjacency_matrix(A)

## ------------------------------------------------------------------------
L <- matrix(sample(1:10, 20, replace = TRUE), ncol = 2)
L

## ------------------------------------------------------------------------
graph_from_edgelist(L)

## ------------------------------------------------------------------------
edges <- data.frame(
  stringsAsFactors = FALSE,
  from = c("BOS", "JFK", "LAX"),
  to   = c("JFK", "LAX", "JFK"),
  Carrier = c("United", "Jetblue", "Virgin America"),
  Departures = c(30, 60, 121)
)
vertices <- data.frame(
  stringsAsFactors = FALSE,
  name = c("BOS", "JFK", "LAX"),
  City = c("Boston, MA", "New York City, NY",
    "Los Angeles, CA")
)

## ------------------------------------------------------------------------
edges

## ------------------------------------------------------------------------
vertices

## ------------------------------------------------------------------------
toy_air <- graph_from_data_frame(edges, vertices = vertices)
toy_air

## ------------------------------------------------------------------------
library(igraphdata)
data(USairports)
USairports

## ------------------------------------------------------------------------
as_data_frame(toy_air, what = "edges")

## ------------------------------------------------------------------------
as_data_frame(toy_air, what = "vertices")

## ------------------------------------------------------------------------
as_long_data_frame(toy_air)

## ------------------------------------------------------------------------
V(USairports)[[1:5]]

## ------------------------------------------------------------------------
E(USairports)[[1:5]]

## ------------------------------------------------------------------------
is_simple(USairports)
sum(which_multiple(USairports))
sum(which_loop(USairports))

## ------------------------------------------------------------------------
air <- simplify(USairports, edge.attr.comb =
  list(Departures = "sum", Seats = "sum", Passengers = "sum", "ignore"))
is_simple(air)
summary(air)

## ----eval = FALSE--------------------------------------------------------
## BOS: JFK, LAX, EWR, MKE, PVD
## JFK: BGR, BOS, SFO, BNA, BUF, SRQ, RIC RDU, MSP
## LAX: DTW, MSY, LAS, FLL, STL,
## . . .

## ------------------------------------------------------------------------
air["BOS", "JFK"]
air["BOS", "ANC"]

## ------------------------------------------------------------------------
air[c("BOS", "JFK", "ANC"), c("BOS", "JFK", "ANC")]

## ------------------------------------------------------------------------
E(air)$weight <- E(air)$Passengers
air["BOS", "JFK"]

## ------------------------------------------------------------------------
air[["BOS"]]

## ------------------------------------------------------------------------
air[[, "BOS"]]

## ------------------------------------------------------------------------
air["BOS", "ANC"] <- TRUE
air["BOS", "ANC"]

## ------------------------------------------------------------------------
air["BOS", "ANC"] <- FALSE
air["BOS", "ANC"]

## ------------------------------------------------------------------------
g <- make_empty_graph(10)
g[-1, 1] <- TRUE
g

## ------------------------------------------------------------------------
g <- make_ring(10) + 2
par(mar = c(0,0,0,0)); plot(g)

## ------------------------------------------------------------------------
g <- make_(ring(10), with_vertex_(color = "grey")) +
  vertices(2, color = "red")
par(mar = c(0,0,0,0)); plot(g)

## ------------------------------------------------------------------------
g <- make_(star(10), with_edge_(color = "grey")) +
  edge(5, 6, color = "red")
par(mar = c(0,0,0,0)); plot(g)

## ------------------------------------------------------------------------
g <- make_(empty_graph(5)) + path(1,2,3,4,5,1)
g2 <- make_(empty_graph(5)) + path(1:5, 1)
g
g2

## ----echo = FALSE--------------------------------------------------------
par(mar=c(0,0,0,0))
plot(make_star(11, center = 11, mode = "undirected") + path(1:10, 1))

## ------------------------------------------------------------------------
make_star(11, center = 11, mode = "undirected") + path(1:10, 1)

## ------------------------------------------------------------------------
FL <- V(air)[grepl("FL$", City)]
CA <- V(air)[grepl("CA$", City)]

V(air)$color <- "grey"
V(air)[FL]$color <- "blue"
V(air)[CA]$color <- "blue"

## ------------------------------------------------------------------------
E(air)[FL %--% CA]
E(air)$color <- "grey"
E(air)[FL %--% CA]$color <- "red"

## ------------------------------------------------------------------------
V(air)[[1:5]]

## ------------------------------------------------------------------------
E(air)[[1:5]]

## ----echo = FALSE--------------------------------------------------------
set.seed(42)
g <- sample_gnp(12, 0.25)
l <- layout_nicely(g)
par(mar=c(0,0,0,0))
plot(g, margin = 0, layout = l)

## ----echo = FALSE--------------------------------------------------------
pa <- V(g)[11, 2, 12, 8]
V(g)[pa]$color <- 'green'
E(g)$color <- 'grey'
E(g, path = pa)$color <- 'red'
E(g, path = pa)$width <- 3
par(mar=c(0,0,0,0))
plot(g, margin = 0, layout = l)

## ------------------------------------------------------------------------
set.seed(42)
g <- sample_gnp(12, 0.25)

pa <- V(g)[11, 2, 12, 8]

V(g)[pa]$color <- 'green'
E(g)$color <- 'grey'
E(g, path = pa)$color <- 'red'
E(g, path = pa)$width <- 3

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0))
plot(g, margin = 0, layout = layout_nicely)

## ----echo = FALSE--------------------------------------------------------
set.seed(42)
g <- sample_gnp(12, 0.25)
pa <- V(g)[11, 2, 12, 8]
V(g)[pa]$color <- 'green'
E(g)$color <- 'grey'
E(g, path = pa)$color <- 'red'
E(g, path = pa)$width <- 3
par(mar=c(0,0,0,0))
plot(g, margin = 0, layout = layout_nicely)

## ------------------------------------------------------------------------
air <- delete_edge_attr(air, "weight")
distances(air, 'PBI', 'ANC')

## ------------------------------------------------------------------------
sp <- shortest_paths(air, 'PBI', 'ANC', output = "both")
sp
air[[ sp$epath[[1]] ]]

## ------------------------------------------------------------------------
all_shortest_paths(air, 'PBI', 'ANC')$res

## ------------------------------------------------------------------------
wair <- simplify(USairports, edge.attr.comb = 
   list(Departures = "sum", Seats = "sum", Passangers = "sum",
        Distance = "first", "ignore"))
E(wair)$weight <- E(wair)$Distance

## ------------------------------------------------------------------------
distances(wair, c('BOS', 'JFK', 'PBI', 'AZO'), 
                    c('BOS', 'JFK', 'PBI', 'AZO'))

## ------------------------------------------------------------------------
shortest_paths(wair, from = 'BOS', to = 'AZO')$vpath
all_shortest_paths(wair, from = 'BOS', to = 'AZO')$res

## ------------------------------------------------------------------------
mean_distance(air)
air_dist_hist <- distance_table(air)
air_dist_hist

## ------------------------------------------------------------------------
barplot(air_dist_hist$res, names.arg = seq_along(air_dist_hist$res))

## ------------------------------------------------------------------------
co <- components(air, mode = "weak")
co$csize
groups(co)[[2]]

## ------------------------------------------------------------------------
co <- components(air, mode = "strong")
co$csize

## ------------------------------------------------------------------------
largest_component <- function(graph) {
  comps <- components(graph, mode = "strong")
  gr <- groups(comps)
  sizes <- vapply(gr, length, 1L)
  induced_subgraph(graph, gr[[ which.max(sizes) ]])
}
sc_air <- largest_component(air)

## ------------------------------------------------------------------------
table(distances(sc_air, "BOS"))
table(distances(sc_air, "LAX"))

## ------------------------------------------------------------------------
mean(as.vector(distances(sc_air, "BOS")))
mean(as.vector(distances(sc_air, "LAX")))

## ------------------------------------------------------------------------
D <- distances(sc_air)
sort(rowMeans(D))[1:10]

## ------------------------------------------------------------------------
sort(rowMeans(D), decreasing = TRUE)[1:10]

## ------------------------------------------------------------------------
V(sc_air)[[names(sort(rowMeans(D), decreasing = TRUE)[1:10])]]

## ----echo = FALSE--------------------------------------------------------
par(mar=c(0,0,0,0))
plot(make_star(11))

## ----echo = FALSE--------------------------------------------------------
data(kite)
par(mar=c(0,0,0,0))
plot(kite)

## ------------------------------------------------------------------------
V(kite)$label.cex <- 2
V(kite)$color <- V(kite)$frame.color <- "grey"
V(kite)$size <- 30
par(mar=c(0,0,0,0)) ; plot(kite)

## ------------------------------------------------------------------------
d <- degree(kite)
par(mar = c(0,0,0,0))
plot(kite, vertex.size = 10 * d, vertex.label =
       paste0(V(kite)$name, ":", d))

## ------------------------------------------------------------------------
cl <- closeness(kite)

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0)); plot(kite, vertex.size = 500 * cl)

## ------------------------------------------------------------------------
btw <- betweenness(kite)
btw

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0)); plot(kite, vertex.size = 3 * btw)

## ------------------------------------------------------------------------
ec <- eigen_centrality(kite)$vector
ec
cor(ec, d)

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0)); plot(kite, vertex.size = 20 * ec)

## ------------------------------------------------------------------------
page_rank(kite)$vector

## ------------------------------------------------------------------------
graph <- make_graph( ~ A-B-C-D-A, E-A:B:C:D, 
                       F-G-H-I-F, J-F:G:H:I,
                       K-L-M-N-K, O-K:L:M:N,
                       P-Q-R-S-P, T-P:Q:R:S,
                       B-F, E-J, C-I, L-T, O-T, M-S,
                       C-P, C-L, I-L, I-P)

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0)); plot(graph)

## ------------------------------------------------------------------------
flat_clustering <- make_clusters(
    graph,
    c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4))

## ------------------------------------------------------------------------
flat_clustering

## ------------------------------------------------------------------------
flat_clustering[[1]]
length(flat_clustering)
sizes(flat_clustering)

## ------------------------------------------------------------------------
induced_subgraph(graph, flat_clustering[[1]])

## ------------------------------------------------------------------------
data(karate)
karate
karate <- delete_edge_attr(karate, "weight")

## ------------------------------------------------------------------------
ground_truth <- make_clusters(karate, V(karate)$Faction)
length(ground_truth)
ground_truth

## ------------------------------------------------------------------------
cluster_naive2 <- function(graph, center1, center2) {
  # ...
}

## ------------------------------------------------------------------------
cluster_naive2 <- function(graph, center1, center2) {
  dist <- distances(graph, c(center1, center2))
  cl <- apply(dist, 2, which.min)
  make_clusters(graph, cl)
}
dist_memb <- cluster_naive2(karate, 'John A', 'Mr Hi')

## ------------------------------------------------------------------------
dist_memb

## ------------------------------------------------------------------------
rand_index <- compare(ground_truth, dist_memb, method = "rand")
rand_index

## ------------------------------------------------------------------------
random_partition <- function(n, k = 2) { sample(k, n, replace = TRUE) }
total <- numeric(100)
for (i in seq_len(100)) {
  c1 <- random_partition(100)
  c2 <- random_partition(100)
  total[i] <- compare(c1, c2, method = "rand")
}
mean(total)

## ------------------------------------------------------------------------
total <- numeric(100)
for (i in seq_len(100)) {
  c1 <- random_partition(100)
  c2 <- random_partition(100)
  total[i] <- compare(c1, c2, method = "adjusted.rand")
}
mean(total)

## ------------------------------------------------------------------------
compare(ground_truth, dist_memb, method = "adjusted.rand")

## ------------------------------------------------------------------------
edge_density(karate)
subgraph_density <- function(graph, vertices) {
  sg <- induced_subgraph(graph, vertices)
  edge_density(sg)
}

## ------------------------------------------------------------------------
subgraph_density(karate, ground_truth[[1]])
subgraph_density(karate, ground_truth[[2]])

## ------------------------------------------------------------------------
modularity(ground_truth)
modularity(karate, membership(ground_truth))

## ------------------------------------------------------------------------
modularity(karate, rep(1, gorder(karate)))
modularity(karate, seq_len(gorder(karate)))

## ------------------------------------------------------------------------
dendrogram <- cluster_edge_betweenness(karate)
dendrogram

## ------------------------------------------------------------------------
membership(dendrogram)

## ------------------------------------------------------------------------
compare_all <- function(cl1, cl2) {
  methods <- eval(as.list(args(compare))$method)
  vapply(methods, compare, 1.0, comm1 = cl1, comm2 = cl2)
}
compare_all(dendrogram, ground_truth)

## ------------------------------------------------------------------------
cluster_memb <- cut_at(dendrogram, no = 2)
compare_all(cluster_memb, ground_truth)
clustering <- make_clusters(karate, membership = cluster_memb)

## ------------------------------------------------------------------------
V(karate)[Faction == 1]$shape <- "circle"
V(karate)[Faction == 2]$shape <- "square"
par(mar=c(0,0,0,0)); plot(clustering, karate)

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0)); plot_dendrogram(dendrogram, direction = "downwards")

## ------------------------------------------------------------------------
optimal <- cluster_optimal(karate)
modularity(clustering)
modularity(optimal)
modularity(ground_truth)

## ------------------------------------------------------------------------
dend_fast <- cluster_fast_greedy(karate)
compare_all(dend_fast, ground_truth)

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0)); plot_dendrogram(dend_fast, direction = "downwards")

## ------------------------------------------------------------------------
igraph_options(edge.color = "black")
data(karate) ; par(mar=c(0,0,0,0)); plot(karate)

## ----fig.width = 6-------------------------------------------------------
V(karate)$color <- "DarkOliveGreen" ; E(karate)$color <- "grey"
par(mar=c(0,0,0,0)) ; plot(karate)

## ----fig.width = 6-------------------------------------------------------
par(mar = c(0,0,0,0))
plot(karate, edge.color = "black", vertex.color = "#00B7FF",
     vertex.label.color = "black")

## ------------------------------------------------------------------------
karate$palette <- categorical_pal(length(clustering))
par(mar = c(0,0,0,0)); plot(karate, vertex.color = membership(clustering))

## ------------------------------------------------------------------------
shapes()

## ----echo = FALSE--------------------------------------------------------
shapes <- setdiff(shapes(), "")
g <- make_ring(length(shapes))

## ----eval = FALSE--------------------------------------------------------
## plot(g, vertex.shape=shapes, vertex.label=shapes, vertex.label.dist=1,
##      vertex.size=15, vertex.size2=15,
##      vertex.pie=lapply(shapes, function(x) if (x=="pie") 2:6 else 0),
##      vertex.pie.color=list(heat.colors(5)))

## ----echo = FALSE--------------------------------------------------------
par(mar = c(0,0,0,0))
plot(g, vertex.shape=shapes, vertex.label=shapes, vertex.label.dist=1,
     vertex.size=15, vertex.size2=15,
     vertex.pie=lapply(shapes, function(x) if (x=="pie") 2:6 else 0),
     vertex.pie.color=list(heat.colors(5)))

## ----echo = FALSE--------------------------------------------------------
lat <- make_lattice(c(5,5))
layout(rbind(1:2,3:4))
par(mar=c(0,0,0,0))
set.seed(42); plot(lat, layout = layout_with_fr(lat, niter = 1))
set.seed(42); plot(lat, layout = layout_with_fr(lat, niter = 5))
set.seed(42); plot(lat, layout = layout_with_fr(lat, niter = 10))
set.seed(42); plot(lat, layout = layout_with_fr(lat, niter = 20))

## ------------------------------------------------------------------------
tree <- make_tree(20, 3)
par(mar = c(0,0,0,0)); plot(tree, layout=layout_as_tree)

## ------------------------------------------------------------------------
l <- layout_as_tree(tree, circular = TRUE)
par(mar = c(0,0,0,0)); plot(tree, layout = l)

## ----echo = FALSE--------------------------------------------------------
## Data taken from http://tehnick-8.narod.ru/dc_clients/
DC <- graph_from_literal("DC++" -+
                "LinuxDC++":"BCDC++":"EiskaltDC++":"StrongDC++":"DiCe!++",
                "LinuxDC++" -+ "FreeDC++", "BCDC++" -+ "StrongDC++",
                "FreeDC++" -+ "BMDC++":"EiskaltDC++",
                "StrongDC++" -+ "AirDC++":"zK++":"ApexDC++":"TkDC++",
                "StrongDC++" -+ "StrongDC++ SQLite":"RSX++",
                "ApexDC++" -+ "FlylinkDC++ ver <= 4xx",
                "ApexDC++" -+ "ApexDC++ Speed-Mod":"DiCe!++",
                "StrongDC++ SQLite" -+ "FlylinkDC++ ver >= 5xx",
                "ApexDC++ Speed-Mod" -+ "FlylinkDC++ ver <= 4xx",
                "ApexDC++ Speed-Mod" -+ "GreylinkDC++",
                "FlylinkDC++ ver <= 4xx" -+ "FlylinkDC++ ver >= 5xx",
                "FlylinkDC++ ver <= 4xx" -+ AvaLink,
                "GreylinkDC++" -+ AvaLink:"RayLinkDC++":"SparkDC++":PeLink)

## Use edge types
E(DC)$lty <- 1
E(DC)["BCDC++" %->% "StrongDC++"]$lty <- 2
E(DC)["FreeDC++" %->% "EiskaltDC++"]$lty <- 2
E(DC)["ApexDC++" %->% "FlylinkDC++ ver <= 4xx"]$lty <- 2
E(DC)["ApexDC++" %->% "DiCe!++"]$lty <- 2
E(DC)["StrongDC++ SQLite" %->% "FlylinkDC++ ver >= 5xx"]$lty <- 2
E(DC)["GreylinkDC++" %->% "AvaLink"]$lty <- 2

## Layers, as on the plot
layers <- list(c("DC++"),
               c("LinuxDC++", "BCDC++"),
               c("FreeDC++", "StrongDC++"),
               c("BMDC++", "EiskaltDC++", "AirDC++", "zK++", "ApexDC++",
                 "TkDC++", "RSX++"),
               c("StrongDC++ SQLite", "ApexDC++ Speed-Mod", "DiCe!++"),
               c("FlylinkDC++ ver <= 4xx", "GreylinkDC++"),
               c("FlylinkDC++ ver >= 5xx", "AvaLink", "RayLinkDC++",
                 "SparkDC++", "PeLink"))

## Check that we have all nodes
all(sort(unlist(layers)) == sort(V(DC)$name))

## Add some graphical parameters
V(DC)$color <- "white"
V(DC)$shape <- "rectangle"
V(DC)$size <- 20
V(DC)$size2 <- 10
V(DC)$label <- lapply(V(DC)$name, function(x)
                      paste(strwrap(x, 12), collapse="\n"))
E(DC)$arrow.size <- 0.5
invisible()

## ------------------------------------------------------------------------
summary(DC)
lay1 <-  layout_with_sugiyama(DC, layers=apply(sapply(layers,
                        function(x) V(DC)$name %in% x), 1, which))

## ------------------------------------------------------------------------
par(mar = rep(0, 4))
plot(DC, layout = lay1$layout, vertex.label.cex = 0.5)

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0)); plot(lay1$extd_graph, vertex.label.cex=0.5)

## ------------------------------------------------------------------------
data(UKfaculty)
UKfaculty

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0)); plot(UKfaculty, layout = layout_with_graphopt)

## ------------------------------------------------------------------------
cl_uk <- cluster_louvain(as.undirected(UKfaculty))
cl_gr <- contract(UKfaculty, mapping = cl_uk$membership)
E(cl_gr)$weight <- count_multiple(cl_gr)
cl_grs <- simplify(cl_gr)
E(cl_grs)$weight

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0)); plot(cl_grs, edge.width=E(cl_grs)$weight / 200,
            edge.curved = .2, vertex.size = sizes(cl_uk) * 2)

## ------------------------------------------------------------------------
subs <- lapply(groups(cl_uk), induced_subgraph, graph = UKfaculty)
summary(subs[[1]])

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0)); plot(subs[[1]])

## ------------------------------------------------------------------------
library(networkD3)
d3_net <- simpleNetwork(as_data_frame(karate, what = "edges")[, 1:3])
d3_net

## ------------------------------------------------------------------------
library(DiagrammeR)

## ------------------------------------------------------------------------
df_kar <- as_data_frame(karate, what = "both")
df_kar$vertices <- cbind(node = rownames(df_kar$vertices),
                         df_kar$vertices)
dg <- create_graph(
  nodes_df = df_kar$vertices,
  edges_df = df_kar$edges
)
render_graph(dg, width = 800, height = 600)

## ------------------------------------------------------------------------
library(rgexf)
df_fac <- as_data_frame(UKfaculty, what = "both")
df_fac$vertices <- cbind(seq_len(gorder(UKfaculty)), df_fac$vertices)
output <- "images/UKfaculty.gexf"
write.gexf(nodes = df_fac$vertices, edges = df_fac$edges[,1:2], 
           edgesAtt = df_fac$edges[,-(1:2), drop = FALSE],
           output = output)

