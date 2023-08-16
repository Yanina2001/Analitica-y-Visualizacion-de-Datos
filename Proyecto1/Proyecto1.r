iris <- read.csv("C:/Users/Usuario/Desktop/ESCOM/Ciencias/Quinto/Analítica y visualización de datos/Proyecto1/iris.csv", header = TRUE)

sepleng_setosa <- iris$sepal.length [1:50]
sepwid_setosa <- iris$sepal.width [1:50]
petleng_setosa <- iris$petal.length [1:50]
petwid_setosa <- iris$petal.width [1:50]

sepleng_versicolor <- iris$sepal.length [51:100]
sepwid_versicolor <- iris$sepal.width [51:100]
petleng_versicolor <- iris$petal.length [51:100]
petwid_versicolor <- iris$petal.width [51:100]

sepleng_virginica <- iris$sepal.length [101:150]
sepwid_virginica <- iris$sepal.width [101:150]
petleng_virginica <- iris$petal.length [101:150]
petwid_virginica <- iris$petal.width [101:150]

bubble_sort <- function(x){
  n <- length(x)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      if(x[i] >= x[j]){
        temp <- x[i]
         x[i] <- x[j]
          x[j] <- temp
      }
    }
  }
  return(x)
}

sum <- 0
media <- function(x) {
    for(i in x) {
    sum <- sum + i
    }
    return(sum /length(x))
}

mediana <- function(x){
   x <- bubble_sort(x)
   if((length(x) %% 2) == 0){
    return((x[length(x)/2] + x[length(x)/2 + 1]) / 2)
   }
   else{
     return(x[(length(x)/2) + 0.5])
   }
}

moda <- function(x) {                     
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

varianza <- function(x){
  x <- as.numeric(x)
  x <- na.omit(x)
  m <- media(x)
  return(
    sum((x-m)^2, na.rm = TRUE)/(length(x) - 1)
  )
}

desvest <- function(x) {
    sqrt(varianza(x))  
}

print("")
print("////////////////////////////////////")
print("---------------SETOSA---------------")
print("")

mediana_sepleng_setosa <- mediana(sepleng_setosa)
print("La mediana de 'sepal leng' en Iris setosa es:'")
print(mediana_sepleng_setosa)
mediana_sepwid_setosa <- mediana(sepwid_setosa)
print("La mediana de 'sepal width' en Iris setosa es:'")
print(mediana_sepwid_setosa)
mediana_petleng_setosa <- mediana(petleng_setosa)
print("La mediana de 'petal leng' en Iris setosa es:'")
print(mediana_petleng_setosa)
mediana_petwid_setosa <- mediana(petwid_setosa)
print("La mediana de 'sepal width' en Iris setosa es:'")
print(mediana_petwid_setosa)

print("-----------------------------")

moda_sepleng_setosa <- moda(sepleng_setosa)
print("La moda de 'sepal leng' en Iris setosa es:'")
print(moda_sepleng_setosa)
moda_sepwid_setosa <- moda(sepwid_setosa)
print("La moda de 'sepal width' en Iris setosa es:'")
print(moda_sepwid_setosa)
moda_petleng_setosa <- moda(petleng_setosa)
print("La moda de 'petal leng' en Iris setosa es:'")
print(moda_petleng_setosa)
moda_petwid_setosa <- moda(petwid_setosa)
print("La moda de 'sepal width' en Iris setosa es:'")
print(moda_petwid_setosa)

print("-----------------------------")

varianza_sepleng_setosa <- varianza(sepleng_setosa)
print("La varianza de 'sepal leng' en Iris setosa es:'")
print(varianza_sepleng_setosa)
varianza_sepwid_setosa <- varianza(sepwid_setosa)
print("La varianza de 'sepal width' en Iris setosa es:'")
print(varianza_sepwid_setosa)
varianza_petleng_setosa <- varianza(petleng_setosa)
print("La varianza de 'petal leng' en Iris setosa es:'")
print(varianza_petleng_setosa)
varianza_petwid_setosa <- varianza(petwid_setosa)
print("La varianza de 'sepal width' en Iris setosa es:'")
print(varianza_petwid_setosa)

print("-----------------------------")

desvest_sepleng_setosa <- desvest(sepleng_setosa)
print("La desviacion estandar de 'sepal leng' en Iris setosa es:'")
print(desvest_sepleng_setosa)
desvest_sepwid_setosa <- desvest(sepwid_setosa)
print("La desviacion estandar de 'sepal width' en Iris setosa es:'")
print(desvest_sepwid_setosa)
desvest_petleng_setosa <- desvest(petleng_setosa)
print("La desviacion estandar de 'petal leng' en Iris setosa es:'")
print(desvest_petleng_setosa)
desvest_petwid_setosa <- desvest(petwid_setosa)
print("La desviacion estandar de 'sepal width' en Iris setosa es:'")
print(desvest_petwid_setosa)

print("")
print("////////////////////////////////////")
print("-------------VERSICOLOR-------------")
print("")

mediana_sepleng_versicolor <- mediana(sepleng_versicolor)
print("La mediana de 'sepal leng' en Iris versicolor es:'")
print(mediana_sepleng_versicolor)
mediana_sepwid_versicolor <- mediana(sepwid_versicolor)
print("La mediana de 'sepal width' en Iris versicolor es:'")
print(mediana_sepwid_versicolor)
mediana_petleng_versicolor <- mediana(petleng_versicolor)
print("La mediana de 'petal leng' en Iris versicolor es:'")
print(mediana_petleng_versicolor)
mediana_petwid_versicolor <- mediana(petwid_versicolor)
print("La mediana de 'sepal width' en Iris versicolor es:'")
print(mediana_petwid_versicolor)

print("-----------------------------")

moda_sepleng_versicolor <- moda(sepleng_versicolor)
print("La moda de 'sepal leng' en Iris versicolor es:'")
print(moda_sepleng_versicolor)
moda_sepwid_versicolor <- moda(sepwid_versicolor)
print("La moda de 'sepal width' en Iris versicolor es:'")
print(moda_sepwid_versicolor)
moda_petleng_versicolor <- moda(petleng_versicolor)
print("La moda de 'petal leng' en Iris versicolor es:'")
print(moda_petleng_versicolor)
moda_petwid_versicolor <- moda(petwid_versicolor)
print("La moda de 'sepal width' en Iris versicolor es:'")
print(moda_petwid_versicolor)

print("-----------------------------")

varianza_sepleng_versicolor <- varianza(sepleng_versicolor)
print("La varianza de 'sepal leng' en Iris versicolor es:'")
print(varianza_sepleng_versicolor)
varianza_sepwid_versicolor <- varianza(sepwid_versicolor)
print("La varianza de 'sepal width' en Iris versicolor es:'")
print(varianza_sepwid_versicolor)
varianza_petleng_versicolor <- varianza(petleng_versicolor)
print("La varianza de 'petal leng' en Iris versicolor es:'")
print(varianza_petleng_versicolor)
varianza_petwid_versicolor <- varianza(petwid_versicolor)
print("La varianza de 'sepal width' en Iris versicolor es:'")
print(varianza_petwid_versicolor)

print("-----------------------------")

desvest_sepleng_versicolor <- desvest(sepleng_versicolor)
print("La desviacion estandar de 'sepal leng' en Iris versicolor es:'")
print(desvest_sepleng_versicolor)
desvest_sepwid_versicolor <- desvest(sepwid_versicolor)
print("La desviacion estandar de 'sepal width' en Iris versicolor es:'")
print(desvest_sepwid_versicolor)
desvest_petleng_versicolor <- desvest(petleng_versicolor)
print("La desviacion estandar de 'petal leng' en Iris versicolor es:'")
print(desvest_petleng_versicolor)
desvest_petwid_versicolor <- desvest(petwid_versicolor)
print("La desviacion estandar de 'sepal width' en Iris versicolor es:'")
print(desvest_petwid_versicolor)

print("")
print("////////////////////////////////////")
print("-------------VIRGINICA--------------")
print("")

mediana_sepleng_virginica <- mediana(sepleng_virginica)
print("La mediana de 'sepal leng' en Iris virginica es:'")
print(mediana_sepleng_virginica)
mediana_sepwid_virginica <- mediana(sepwid_virginica)
print("La mediana de 'sepal width' en Iris virginica es:'")
print(mediana_sepwid_virginica)
mediana_petleng_virginica <- mediana(petleng_virginica)
print("La mediana de 'petal leng' en Iris virginica es:'")
print(mediana_petleng_virginica)
mediana_petwid_virginica <- mediana(petwid_virginica)
print("La mediana de 'sepal width' en Iris virginica es:'")
print(mediana_petwid_virginica)

print("-----------------------------")

moda_sepleng_virginica <- moda(sepleng_virginica)
print("La moda de 'sepal leng' en Iris virginica es:'")
print(moda_sepleng_virginica)
moda_sepwid_virginica <- moda(sepwid_virginica)
print("La moda de 'sepal width' en Iris virginica es:'")
print(moda_sepwid_virginica)
moda_petleng_virginica <- moda(petleng_virginica)
print("La moda de 'petal leng' en Iris virginica es:'")
print(moda_petleng_virginica)
moda_petwid_virginica <- moda(petwid_virginica)
print("La moda de 'sepal width' en Iris virginica es:'")
print(moda_petwid_virginica)

print("-----------------------------")

varianza_sepleng_virginica<- varianza(sepleng_virginica)
print("La varianza de 'sepal leng' en Iris virginica es:'")
print(varianza_sepleng_virginica)
varianza_sepwid_virginica <- varianza(sepwid_virginica)
print("La varianza de 'sepal width' en Iris virginica es:'")
print(varianza_sepwid_virginica)
varianza_petleng_virginica <- varianza(petleng_virginica)
print("La varianza de 'petal leng' en Iris virginica es:'")
print(varianza_petleng_virginica)
varianza_petwid_virginica <- varianza(petwid_virginica)
print("La varianza de 'sepal width' en Iris virginica es:'")
print(varianza_petwid_virginica)

print("-----------------------------")

desvest_sepleng_virginica <- desvest(sepleng_virginica)
print("La desviacion estandar de 'sepal leng' en Iris virginica es:'")
print(desvest_sepleng_virginica)
desvest_sepwid_virginica <- desvest(sepwid_virginica)
print("La desviacion estandar de 'sepal width' en Iris virginica es:'")
print(desvest_sepwid_virginica)
desvest_petleng_virginica <- desvest(petleng_virginica)
print("La desviacion estandar de 'petal leng' en Iris virginica es:'")
print(desvest_petleng_virginica)
desvest_petwid_virginica <- desvest(petwid_virginica)
print("La desviacion estandar de 'sepal width' en Iris virginica es:'")
print(desvest_petwid_virginica)