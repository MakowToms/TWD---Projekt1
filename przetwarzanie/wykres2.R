library(ggplot2)
library(data.table)

# po znalezieniu kto ma ile jakich mocy

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

ggplot(tmp[order(tmp$number)], aes(x=hero, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = final$hero[20:1], labels=rep('',20)) +
  coord_flip()+
  scale_fill_manual(values = c("#636363","#ca0020","#0571b0")) +
  labs(y = "",x ="", title="Jaki rozklad mocy maja najważniejsze postacie?",fill='Rodzaj mocy')

ggsave('wykres.png')
