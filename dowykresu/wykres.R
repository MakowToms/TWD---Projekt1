library(data.table)

dt <- data.table(read.csv2("edges.csv"))
names(dt) <- c("hero1","hero2")
ans1 <- dt[, .(.N), by = .(hero1)]
ans2 <- dt[, .(.N), by = .(hero2)]

merged <- merge(ans1,ans2,all=TRUE,by.x = "hero1",by.y = "hero2")
merged$n <- (merged$N.x + merged$N.y)
merged <- merged[,c(1,4)]
merged <- merged[order(-n)]
ans <- merged[1:20]
ans$hero1 <- c("CAPTAIN AMERICA","SPIDER-MAN","IRON MAN","THOR","THING","WOLVERINE",
               "HUMAN TORCH","SCARLET WITCH","MISTER FANTASTIC","VISION","INVISIBLE WOMAN",
               "BEAST","CYCLOPS","STORM","HAWK","WASP","COLOSSUS","PROFESSOR X",
               "HULK","ANT-MAN")

powers <- data.table(read.csv2("powers.csv"))
colnames(powers)[1] <- "heros"
powers$heros <- toupper(powers$heros)

tmp <- merge(ans,powers,by.x = "hero1",by.y = "heros", all.x = TRUE)
write.csv2(tmp,file="dowykresu.csv")

temp <- tmp[Accelerated.Healing==TRUE]
nrow(temp)

v <- colnames(tmp)
v <- v[3:length(v)]

empty <- vector()
for( i in 1:length(v)) {
  print(v[i])
  temp <- tmp[v[i]==TRUE]
  if (nrow(temp) == 0) {
    empty <- c(empty,i)
  }
}
empty

head(v)
write.csv2(v,file="namesofpowers.csv")
