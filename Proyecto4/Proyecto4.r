library(ggplot2)

Corporaciones <- read.csv("C:/Users/Usuario/Desktop/ESCOM/Ciencias/Quinto/Analítica y visualización de datos/Proyecto4/Corpo.csv")
#print(Corporaciones)

dfFord = Corporaciones[Corporaciones$Compania == "Ford",]
dfExxon = Corporaciones[Corporaciones$Compania == "Exxon",]
dfDuPont = Corporaciones[Corporaciones$Compania == "DuPont",]

lista <- as.data.frame(t(c(dfFord[-1])), row.names = NULL)

df3Corpo <- rbind(dfFord, dfExxon,dfDuPont)
Corporaciones = Corporaciones[,-1]
df3Corpo <- sapply(df3Corpo, as.numeric)

df3Corpo$mahalnobis<- mahalanobis(Corporaciones, colMeans(Corporaciones), cov(Corporaciones))
print(df3Corpo)

num.outliers <- 3
mah.ordenacion <- order(mahalanobis(Corporaciones , colMeans(Corporaciones), cov(Corporaciones)), decreasing=TRUE)
outlier3 <- rep(FALSE , nrow(Corporaciones))
outlier3[mah.ordenacion[1:num.outliers]] <- TRUE
colorear.outlier <- outlier3 * 50

plot(Corporaciones, pch=0)
points(Corporaciones , pch=colorear.outlier)

print("-------------------")

dfGM = Corporaciones[Corporaciones$Compania == "General Motors",]
dfIBM = Corporaciones[Corporaciones$Compania == "IBM",]
dfChrysler = Corporaciones[Corporaciones$Compania == "Chrysler",]

lista1 <- as.data.frame(t(c(dfGM[-1])), row.names = NULL)

df3Corpo1 <- rbind(dfGM, dfIBM,dfChrysler)
Corporaciones = Corporaciones[,-1]
df3Corpo1 <- sapply(df3Corpo1, as.numeric)

df3Corpo1$mahalnobis<- mahalanobis(Corporaciones, colMeans(Corporaciones), cov(Corporaciones))
print(df3Corpo1)

num.outliers <- 3
mah.ordenacion <- order(mahalanobis(Corporaciones , colMeans(Corporaciones), cov(Corporaciones)), decreasing=TRUE)
outlier3 <- rep(FALSE , nrow(Corporaciones))
outlier3[mah.ordenacion[1:num.outliers]] <- TRUE
colorear.outlier <- outlier3 * 50

plot(Corporaciones, pch=0)
points(Corporaciones , pch=colorear.outlier)

print("************************")

dfPM = Corporaciones[Corporaciones$Compania == "Philip Morrls",]
dfTexaco = Corporaciones[Corporaciones$Compania == "Texaco",]
dfMobil = Corporaciones[Corporaciones$Compania == "Mobil",]

lista2 <- as.data.frame(t(c(dfPM[-1])), row.names = NULL)

df3Corpo2 <- rbind(dfPM, dfTexaco,dfMobil)
Corporaciones = Corporaciones[1]
df3Corpo2 <- sapply(df3Corpo2, as.numeric)

df3Corpo2$mahalnobis<- mahalanobis(Corporaciones, colMeans(Corporaciones), cov(Corporaciones))
print(df3Corpo2)

num.outliers <- 3
mah.ordenacion <- order(mahalanobis(Corporaciones , colMeans(Corporaciones), cov(Corporaciones)), decreasing=TRUE)
outlier3 <- rep(FALSE , nrow(Corporaciones))
outlier3[mah.ordenacion[1:num.outliers]] <- TRUE
colorear.outlier <- outlier3 * 50

plot(Corporaciones, pch=0)
points(Corporaciones , pch=colorear.outlier)
