dane <- read.csv("Desktop/git/TWD---Projekt1/dowykresu/dowykresupoprawione.csv")[-1]
nazwy <- read.csv("Desktop/git/TWD---Projekt1/dowykresu/nazwy.csv", row.names = 2)[-1]
moce <- read.csv("Desktop/git/TWD---Projekt1/dowykresu/moceiklasy.csv")[-1]
t <- matrix(rep(0, 80), ncol = 4)
for(i in 1:20)
  for(j in 1:49)
    if(dane[i, j+2]){
      dane[i, j+2] <- moce[i, j]
      t[i, moce[i, j]] =  t[i, moce[i, j]] + 1
      t[i, 4] = t[i, 4] + 1
    } else {
      dane[i, j+2] <- NA
    }
dane
t