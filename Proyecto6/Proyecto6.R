library(CommEcol)
library(adespatial)

main<- function(){
  df <- read.csv("D:/Documentos/Licenciatura en Ciencia de Datos/Quinto Semestre/AVD/Proyecto6/TipoDeSuelo.csv")
  
  newDf = df[df$Tipo.de.Suelo.Aluvial..abundancia.km2. >= 20,]
  newDf = df[df$Tipo.de.Suelo.Lacustre..abundancia.km2. >= 20,]
  
  print(newDf)
  
  cat("\nCalculo de los indices de disimilitud  de jaccard para nuestro df\n")
  print(dis.chao(newDf[,2:3], index='jaccard'))
  cat("\nCalculo de los indices de disimilitud  de sorensen para nuestro df\n")
  print(dis.chao(newDf[,2:3], index='sorensen'))
  
}

main()