
#Moda funciones
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

print_moda <- function(df){
  cat("\n")
  cat("\n La moda de sepal_Length es : ")
  cat(find_mode(df$sepal_Length))
  cat("\n La moda de sepal_Width es : ")
  cat(find_mode(df$sepal_Width))
  cat("\n La moda de petal_Length es : ")
  cat(find_mode(df$petal_Length))
  cat("\n La moda de petal_Width es : ")
  cat(find_mode(df$petal_Width))
  cat("\n La moda de class es : ")
  cat(find_mode(df$class))
  cat("\n La moda de area_Sepal es : ")
  cat(find_mode(df$area_Sepal))
  cat("\n_______________________________________________")
  
}

print_moda_datos <- function(){
  cat("\n La moda de los valores para Iris Setosa")
  print_moda(iris_setosa_Data)
  cat("\n La moda de los valores para Iris Versicolor")
  print_moda(iris_Versicolor_Data)
  cat("\n La moda de los valores para Iris Virginica")
  print_moda(iris_Virginica_Data)
}


#mediana funciones
median_func <- function(df){
  mid <- seq(floor((length(df)+1)/2),ceiling((length(df)+1)/2))
  mean(sort(df)[mid])
}

print_Mediana <- function(df){
  cat("\n")
  cat("\n La mediana de sepal_Length es : ")
  cat(median_func(df$sepal_Length))
  cat("\n La mediana de sepal_Width es : ")
  cat(median_func(df$sepal_Width))
  cat("\n La mediana de petal_Length es : ")
  cat(median_func(df$petal_Length))
  cat("\n La mediana de petal_Width es : ")
  cat(median_func(df$petal_Width))
  cat("\nLa mediana no aplica para clase\n")
  cat("\n La mediana de area_Sepal es : ")
  cat(median_func(df$area_Sepal))
  cat("\n_______________________________________________")
}

print_mediana_datos <- function(){
  cat("\n La mediana de los valores para Iris Setosa")
  print_Mediana(iris_setosa_Data)
  cat("\n La mediana de los valores para Iris Versicolor")
  print_Mediana(iris_Versicolor_Data)
  cat("\n La mediana de los valores para Iris Virginica")
  print_Mediana(iris_Virginica_Data)
}

#Media aritmetica funciones

mean_fun<-function(df){
  mean_c= sum(df)/length((df))
  return(mean_c)
}

print_mean <- function(df){
  cat("\n")
  cat("\n La media de sepal_Length es : ")
  cat(mean_fun(df$sepal_Length))
  cat("\n La media de sepal_Width es : ")
  cat(mean_fun(df$sepal_Width ))
  cat("\n La media de petal_Length es : ")
  cat(mean_fun(df$petal_Length))
  cat("\n La media de petal_Width es : ")
  cat(mean_fun(df$petal_Width))
  cat("\nLa media no aplica para clase\n")
  cat("\n La media de area_Sepal es : ")
  cat(mean_fun(df$area_Sepal))
  cat("\n_______________________________________________")
}

print_mean_datos <- function(){
  cat("\n La media de los valores para Iris Setosa")
  print_mean(iris_setosa_Data)
  cat("\n La media de los valores para Iris Versicolor")
  print_mean(iris_Versicolor_Data)
  cat("\n La media de los valores para Iris Virginica")
  print_mean(iris_Virginica_Data)
}

#varianza funciones

varFunc <- function(x){
  x <- as.numeric(as.character(x))[!is.na(as.numeric(as.character(x)))]
  sum((x-mean(x))^2) / (length(x)-1)
}

print_var <- function(df){
  cat("\n")
  cat("\n La Varianza de sepal_Length es : ")
  cat(varFunc(df$sepal_Length))
  cat("\n La Varianza de sepal_Width es : ")
  cat(varFunc(df$sepal_Width ))
  cat("\n La Varianza de petal_Length es : ")
  cat(varFunc(df$petal_Length))
  cat("\n La Varianza de petal_Width es : ")
  cat(varFunc(df$petal_Width))
  cat("\nLa Varianza no aplica para clase\n")
  cat("\n La Varianza de area_Sepal es : ")
  cat(varFunc(df$area_Sepal))
  cat("\n_______________________________________________")
}

print_var_datos <- function(){
  cat("\n La Varianza de los valores para Iris Setosa")
  print_var(iris_setosa_Data)
  cat("\n La Varianza de los valores para Iris Versicolor")
  print_var(iris_Versicolor_Data)
  cat("\n La Varianza de los valores para Iris Virginica")
  print_var(iris_Virginica_Data)
}



#Desviacion estandar

print_des <- function(df){
  cat("\n")
  cat("\n La Desviacion estandar de sepal_Length es : ")
  cat(sqrt(varFunc(df$sepal_Length)))
  cat("\n La Desviacion estandar de sepal_Width es : ")
  cat(sqrt(varFunc(df$sepal_Width )))
  cat("\n La Desviacion estandar de petal_Length es : ")
  cat(sqrt(varFunc(df$petal_Length)))
  cat("\n La Desviacion estandar de petal_Width es : ")
  cat(sqrt(varFunc(df$petal_Width)))
  cat("\nLa Desviacion estandar no aplica para clase\n")
  cat("\n La Desviacion estandar de area_Sepal es : ")
  cat(sqrt(varFunc(df$area_Sepal)))
  cat("\n_______________________________________________")
}

print_des_datos <- function(){
  cat("\n La Desviacion estandar de los valores para Iris Setosa")
  print_des(iris_setosa_Data)
  cat("\n La Desviacion estandar de los valores para Iris Versicolor")
  print_des(iris_Versicolor_Data)
  cat("\n La Desviacion estandar de los valores para Iris Virginica")
  print_des(iris_Virginica_Data)
}


##Programa inicio 
names <- cbind("sepal_Length","sepal_Width", "petal_Length", "petal_Width", "class")

df <- read.csv("D:/Documentos/Licenciatura en Ciencia de Datos/Quinto Semestre/AVD/Proyecto2/iris.csv", header = TRUE,col.names =names)

sepal_l <- df["sepal_Length"]
sepal_w <- df["sepal_Width"]

area_sepal <-  sepal_l * sepal_w
colnames(area_sepal) <- "area_Sepal"
df <- cbind(df, area_sepal)
cat("Imprimir DF, con las areas: ")
cat(df)

iris_setosa_Data <-df[which(df$class=="Iris-setosa"),]
iris_Versicolor_Data <- df[which(df$class=="Iris-versicolor"),]
iris_Virginica_Data <- df[which(df$class=="Iris-virginica"),]

print_moda_datos()
print_mediana_datos()
print_mean_datos()
print_var_datos()
print_des_datos()

