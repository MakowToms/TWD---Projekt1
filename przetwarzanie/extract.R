dane <- read.csv(file = "Desktop/dowykresu.csv", header = 1, row.names = 1)
ncol(dane)
head(dane)
dane
t <- apply(X = dane[-(1:2)], 2, FUN = any)
t <- c(TRUE, TRUE, unlist(t))
t
dane <- dane[,t]
dane
ncol(dane)
t
colnames(dane[-(1:2)])

write.csv(dane, file = "Desktop/dowykresupoprawione.csv")
