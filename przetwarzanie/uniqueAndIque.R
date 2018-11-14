options(stringsAsFactors = FALSE)
library(data.table)
dane <- read.csv("../dataset/super_hero_powers.csv")[-1]
dane
t <- as.data.frame(matrix(0, nrow(dane), ncol(dane)))
for(i in 1:nrow(dane))
  for(j in 1:ncol(dane))
    if(dane[i, j]=="True"){
      t[i, j] <- 1
    }
colnames(t) <- colnames(dane)
s <- colSums(t)
s <- s[order(-s)]
unikalne <- s[s==1] 
barplot(s)
popularne <- s[1:10]
write.csv(popularne, "popularne.csv")
write.csv(unikalne, "unikalne.csv")