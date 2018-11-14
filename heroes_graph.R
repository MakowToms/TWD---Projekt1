#install.packages('igraph')
#install.packages('readr')
#install.packages('png')
#install.packages('jpeg')
#install.packages('data.table')
library(igraph)
library(ggplot2)
library(readr)
library(dplyr)
library(data.table)
library(patchwork)
library(png)
library(jpeg)

#wczytanie
nodes <- read.csv('dataset/nodes.csv')
edges <- read.csv('dataset/edges.csv')
hero_network  <- data.table(read.csv('dataset/hero-network.csv'))
hero1 <- hero_network[, .(.N), by = .(hero1)]
hero2 <- hero_network[, .(.N), by = .(hero2)]
merged <- merge(hero1,hero2,all=TRUE,by.x = "hero1",by.y = "hero2")
#NA-s should be replaced
merged$n <- (merged$N.x + merged$N.y)
merged <- merged[,c(1,4)]
top20 <- merged[order(-n)][1:20]
#powyżej zmienia się 10 na inną liczbę, żeby było tylu superbohaterów
heros <- hero_network[, .(.N), by = .(hero1,hero2)][order(-N)]

labels <- c("CAPTAIN AMERICA","SPIDER-MAN","IRON MAN","THOR","THING","WOLVERINE",
            "HUMAN TORCH","SCARLET WITCH","MISTER FANTASTIC","VISION","INVISIBLE WOMAN",
            "BEAST","CYCLOPS","STORM","HAWK","WASP","COLOSSUS","PROFESSOR X",
            "HULK","ANT-MAN")

if_both_of_heros_in_top_20 <- function (x) if(any(rep(x[1],20)==top20$hero1) & any(rep(x[2],20)==top20$hero1)) 1 else 0
#which_rows_of_hero_network <- apply(hero_network,1,if_both_of_heros_in_top_20)
#saveRDS(which_rows_of_hero_network,'which_rows_of_hero_network.RDS')
which_rows_of_hero_network = readRDS('which_rows_of_hero_network.RDS')
hn_edges <- hero_network[which_rows_of_hero_network==1, .(.N), by = .(hero1,hero2)][order(-N)]
#brzydkie :(
hn_edges2 <- hn_edges[1,]
for (i in 2:nrow(hn_edges)) {
  j <- nrow(hn_edges2)
  heros_the_same <- (hn_edges2[1:j]$hero1 == rep(as.character(hn_edges[i]$hero2),j)) + 
    (hn_edges2[1:j]$hero2 == rep(as.character(hn_edges[i]$hero1),j))
  if (any((heros_the_same) == 2)) {
    index <- (1:j)[heros_the_same==2]
    hn_edges2[index]$N <- hn_edges2[index]$N + hn_edges[i]$N
  }
  else {hn_edges2 <- rbind(hn_edges2,hn_edges[i])}
}
#saveRDS(hn_edges2,'hn_edges2.RDS')
#hn_edges2 = readRDS('hn_edges2.RDS')
hn_edges <- hn_edges2

edge_colors <- rep('red',nrow(hn_edges))
edge_colors[as.numeric(hn_edges$N)>380L] <- 'blue'

#hn_edges <- hn_edges[as.numeric(N)>380L]
hn_edges[] <- lapply(hn_edges, as.character)
network1 <- graph_from_data_frame(hn_edges, directed = F, top20$hero1)
img <- readJPEG('images/superhero.jpeg')
V(network1)$raster <- replicate(vcount(network1), img, simplify=FALSE)
plot.igraph(network1,vertex.shape="raster",vertex.label = labels,#[1:10], #labels[1:tylu_ilu_bohaterow]
            # vertex.size = 10, vertex.label.cex = 1,
            edge.width = as.numeric(hn_edges$N)/50,
            edge.color = edge_colors,
            main = "a Network Relationship between Heroes")
?plot.igraph




#dalej są komendy przepisane z kernela - nie ma sensu ich uruchamiać












glimpse(nodes)
glimpse(edges)
glimpse(hero_network)
nodes[] <- lapply(nodes, as.character)
edges[] <- lapply(edges, as.character)
hero_network[] <- lapply(hero_network, as.character)
graph_from_data_frame(edges, directed = F, nodes)
hero.vector <- unique(edges$hero)
comic.vector <- unique(edges$comic)
hero.vector.type <- rep("hero", times = length(hero.vector))
comic.vector.type <- rep("comic", times = length(comic.vector))
df1 <- data.frame(node = as.character(hero.vector),
                  type = as.character(hero.vector.type))
df2 <- data.frame(node = as.character(comic.vector),
                  type = as.character(comic.vector.type))
revised_nodes <- rbind(df1, df2)
revised_nodes[] <- lapply(revised_nodes, as.character)
head(revised_nodes, 6)
glimpse(revised_nodes)
revised_nodes$type <- ifelse(revised_nodes$type == "hero", TRUE, FALSE)
head(revised_nodes, 6)
glimpse(revised_nodes)
network1 <- graph_from_data_frame(edges, directed = F, revised_nodes)
# network2 <- graph_from_data_frame(edges, directed = T, revised_nodes)
network3 <- bipartite_projection(network1, which = "TRUE")
network4 <- bipartite_projection(network1, which = "FALSE")

#159
plot.igraph(network1, vertex.size = 3, vertex.label.cex = 0.5,
            main = "a Network Relationship between Heroes and Comics")
plot.igraph(network3, vertex.size = 3, vertex.label.cex = 0.5,
            main = "a Network Relationship between Heroes and Comics")


V(network1)
E(network1)
edge_density(network1, loops=F) #graph.density(network1) - to samo
transitivity(network1, type="global")
transitivity(network1, type='local') #coś z trójkątami??
farthest.nodes(network1, directed=F, weights=NA) #nqjdłuższa ścieżka

#degrees
deg <- degree(network1, mode="all") 
deg_dataframe <- as.data.frame(deg)
deg_dataframe$node <- row.names(deg_dataframe)
row.names(deg_dataframe) <- NULL
top20_nodes <- deg_dataframe %>% select(node, deg) %>%
  arrange(desc(deg)) %>%  head(20)
top20_nodes$node
table(deg_dataframe$deg)


#hero_network[hero1=='PATRIOT/JEFF MACE' & hero2=='PATRIOT/JEFF MACE']
