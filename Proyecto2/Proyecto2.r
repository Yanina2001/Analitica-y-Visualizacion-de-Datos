cat("INSTITUTO POLITECNICO NACIONAL\nESCUELA SUPERIOR DE COMPUTO\n PROGRAMACION PARA LA CIENCIA DE DATOS\n 
    \nAutor: \n
    Medina Barrera Daniel Ivan
    García Rodríguez Diana Itzel
    Zamudio Zamora Roxana
    Ángeles Lomelí Felipe Alberto\n
    ")
# setwd("~/proyecto_vi")
install.packages('arules')
install.packages("geometry")
library("arules")
library("geometry")
names <- cbind("sepal_Length","sepal_Width", "petal_Length", "petal_Width", "class")

df <- read.csv("iris.csv", header = FALSE,col.names =names)

sepal_L <- df["sepal_Length"]
sepal_W <- df["sepal_Width"]
petal_L <- df["petal_Length"]
petal_W <- df["petal_Width"]

area_S <-  sepal_L * sepal_W
area_P <-  petal_L * petal_W

colnames(area_S) <- "area_Sepal"
df <- cbind(df, area_S)

colnames(area_P) <- "area_Petalo"
df <- cbind(df, area_P)
iris_setosa_Data <-df[which(df$class=="Iris-setosa"),]
iris_Versicolor_Data <- df[which(df$class=="Iris-versicolor"),]
iris_Virginica_Data <- df[which(df$class=="Iris-virginica"),]




# Cuantificar el valor de disimilitud
# con la norma Eucl?dea entre las flores:
# Setosa, Versicolor y Virginica, tomando como base la longitud y anchura del
# s?palo y p?talo de cada flor.

euclidean <- function(a, b) sqrt(sum((a - b)^2))

print_euclidean <- function(df1, df2){
  cat("\n")
  cat("\n La disimilitud con la norma Eucl?dea de sepal_Length: ")
  cat(euclidean(df1$sepal_Length, df2$sepal_Length ))
  cat("\n La disimilitud con la norma Eucl?dea de sepal_Width: ")
  cat(euclidean(df1$sepal_Width, df2$sepal_Width))
  cat("\n La disimilitud con la norma Eucl?dea de petal_Length: ")
  cat(euclidean(df1$petal_Length, df2$petal_Length))
  cat("\n La disimilitud con la norma Eucl?dea de petal_Width: ")
  cat(euclidean(df1$petal_Width, df2$petal_Width))
  cat("\n La disimilitud con la norma Eucl?dea no aplica para clase\n")
  cat("\n_______________________________________________")
}

print_euclidean_datos <- function(){
  cat("\n La disimilitud con la norma Eucl?dea para Iris Setosa, e Iris Versicolor")
  print_euclidean(iris_setosa_Data, iris_Versicolor_Data)
  cat("\n La disimilitud con la norma Eucl?dea para Iris Setosar, Iris Virginica")
  print_euclidean(iris_setosa_Data, iris_Virginica_Data)
  cat("\n La disimilitud con la norma Eucl?dea para Iris Versicolor, Iris Virginica")
  print_euclidean(iris_Versicolor_Data, iris_Virginica_Data)
}
print_euclidean_datos()



#Establecer el umbral de dimilitud entre las 3 clases de flores.
print_dimilitud_datos <- function(){
  cat("\n La disimilitud con la norma Eucl?dea para Iris Setosa, e Iris Versicolor ")
  print(euclidean(iris_setosa_Data$area_Sepal, iris_setosa_Data$area_Petalo))
  cat("\n La disimilitud con la norma Eucl?dea para Iris Setosar, Iris Virginica")
  print(euclidean(iris_Versicolor_Data$area_Sepal, iris_Versicolor_Data$area_Petalo))
  cat("\n La disimilitud con la norma Eucl?dea para Iris Versicolor, Iris Virginica")
  print(euclidean(iris_Virginica_Data$area_Sepal, iris_Virginica_Data$area_Petalo))
}

print_dimilitud_datos()

#norma
norma <- function(df) {
  df_ma = df[,-5]
  cat(norm(as.matrix(df_ma)))
}

print_norm_datos <- function(){
  cat("\n La norma de Iris Setosa: ")
  norma(iris_setosa_Data)
  cat("\n La norma de Iris Versicolor: ")
  norma(iris_Versicolor_Data)
  cat("\n La norma de Iris Virginica: ")
  norma(iris_Virginica_Data)
}
print_norm_datos()


#Calcular la distancia entre los elementos de la clase
#Setosa-Versicolor; Setosa-Virginica; Versicolor-Virginica; Versicolor-Setosa;
#Virginica-Setosa; Virginica-Versicolor

print_outer_datos <- function(){
  cat("\n Calcular la distancia entre los elementos de la clase")
  cat("\n Setosa-Versicolor")
  print(outer(as.matrix(iris_setosa_Data[,-5]),as.matrix(iris_Versicolor_Data[,-5])))
  cat("\n Setosa-Virginica")
  print(outer(as.matrix(iris_setosa_Data[,-5]),as.matrix(iris_Virginica_Data[,-5])))
  cat("\n Versicolor-Virginica")
  print(outer(as.matrix(iris_Versicolor_Data[,-5]),as.matrix(iris_Virginica_Data[,-5])))
  cat("\n Versicolor-Setosa")
  print(outer(as.matrix(iris_Versicolor_Data[,-5]),as.matrix(iris_setosa_Data[,-5])))
  cat("\n Virginica-Setosa")
  print(outer(as.matrix(iris_Virginica_Data[,-5]),as.matrix(iris_setosa_Data[,-5])))
  cat("\n Virginica-Versicolor")
  print(outer(as.matrix(iris_Virginica_Data[,-5]),as.matrix(iris_Versicolor_Data[,-5])))
}
print_outer_datos()


#producto internoz

print_dot_datos <- function(){
  cat("\n Calcular producto interno")
  cat("\n Setosa-Versicolor")
  print(dot(as.matrix(iris_setosa_Data[,-5]), as.matrix(iris_Versicolor_Data[,-5])))
  cat("\n Setosa-Virginica")
  print(dot(as.matrix(iris_setosa_Data[,-5]), as.matrix(iris_Virginica_Data[,-5])))
  cat("\n Versicolor-Virginica")
  print(dot(as.matrix(iris_Versicolor_Data[,-5]), as.matrix(iris_Virginica_Data[,-5])))
  cat("\n No se realisaron todas las conbinaciones por que da lo mismo")
}
print_dot_datos()
 
