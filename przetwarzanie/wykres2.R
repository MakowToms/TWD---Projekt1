library(ggplot2)
library(data.table)

# po znalezieniu kto ma ile jakich mocy
library(igraph)
library(ggplot2)
library(readr)
library(dplyr)
library(data.table)
library(patchwork)
library(jpeg)
library(png)
library(igraph)

final <- data.table(read.csv("przetwarzanie/final.csv"))

final <- final[,c(2,3,53:56)]
final <- final[order(-n)]
final$add <- 1:20

names(final) <- c("hero","popularity","real","scifi","fantasy","all","number")
final$real <- final$real/final$all*100
final$scifi <- final$scifi/final$all*100
final$fantasy <- final$fantasy/final$all*100

final$hero <- c("Capitan America","Spider-Man","Iron Man","Thor","Thing","Wolverine",
                "Human Torch","Scarlet Witch","Mister Fantastic","Vision","Invisible Woman",
                "Beast","Cyclops","Storm","Hawk","Wasp","Colossus","Professor X",
                "Hulk","Ant-Man")

tmp <- final[,-6]
tmp <- melt(tmp[,-2], id=c("hero","number"))
tmp[order(tmp$number)]

w1 <- ggplot(tmp[order(tmp$number)], aes(x=hero, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = final$hero[20:1], labels=rep('',20)) +
  coord_flip()+
  scale_fill_manual(values = c("#636363","#ca0020","#0571b0")) +
  labs(y = "",x ="", title="Jaki rozklad mocy maja najważniejsze postacie?",fill='Rodzaj mocy')

library('gridGraphics')

img <- readJPEG('images/superhero.jpeg')
g <- rasterGrob(img, interpolate=FALSE)
#tmp3 <- data.frame(x=1:20,y=1,z=rep(g,20))
#w3 <- ggplot(tmp3, aes(x = x, y = y, shape=z)) +
  #annotation_custom(g, xmin=1, xmax=3, ymin=1, ymax=3) +
w3 <- qplot(1:20, rep(1,20), geom="blank") +
  annotation_custom(g, xmin=1.5, xmax=2.5, ymin=0.9, ymax=1.1) +
  annotation_custom(g, xmin=2.5, xmax=3.5, ymin=0.9, ymax=1.1) +
  geom_point() +
  theme_void() +
  coord_flip() +
  theme(legend.position = "none")

w3 + w1


