library(dplyr)
library(skimr)
library(NLP)
library(tm)
library(tmap)
library(tidyverse)


get_similar_document <- function(Cosine_Similarity_dataframe, document_to_find_similarity, n_recommendations = 1){
  sort(Cosine_Similarity_dataframe[document_to_find_similarity, ], decreasing = TRUE)[1:( n_recommendations)]
}


df <- read.csv("./covid_abstracts.csv")
  
dim(df)
colnames(df)
head(df)

summary(df)
glimpse(df)
skim(df)

df_Sample = df[6501:6600,]

Data <- Corpus(VectorSource(df_Sample$abstract))
Data <- tm_map(Data,tolower) 
Data <- tm_map(Data, removeNumbers) 
Data <- tm_map(Data, removePunctuation) 
Data <- tm_map(Data, stripWhitespace) 
Data <- tm_map(Data, removeWords,stopwords("english")) 
Data <- tm_map(Data, removeWords,c('landline'))
  
tdm_tfidf <- DocumentTermMatrix(Data,
                                  control = list(weighting = weightTfIdf))
m_tfidf <- as.matrix(tdm_tfidf)
  
library(text2vec)
Cosine_Similarity <- sim2(m_tfidf, y = NULL, method = c("cosine"), norm = c("l2"))
Cosine_Similarity_dataframe <- as.data.frame(Cosine_Similarity)
  
diag(Cosine_Similarity_dataframe) <- NA
myList <- list()
  
for (i in 1:100){
  a = get_similar_document(Cosine_Similarity_dataframe, i)
  myList <- append(myList, a)
}
  
cat("\n\nDistancias del coseno para nuestra muestra: \n\n")  
for (i in 1:100){
  minim = 6500
  corect = 6500 + i 
  number = strtoi(names(myList)[i])
  a = number + minim
  cosa = myList[i]
  cat("\nLa distancia del coseno mayor de", corect)
  cat(" es con", a)
  cat(" y es", cosa[[c(1)]])
}
  
  
data_temp = data.frame(strtoi(names(myList)))
colnames(data_temp) <- "Distancia_cos"
df_temp <- tibble::rowid_to_column(data_temp, "PrimerValor")
  
  
df_li <- data.frame(matrix(unlist(myList), nrow=length(1), byrow=TRUE))
final_df <- as.data.frame(t(df_li))
row.names(final_df) <- NULL
  
df_new <- cbind(df_temp, final_df)
  
final_df <- tibble::rowid_to_column(final_df, "index")
final_df = df_new[order(df_new$V1,decreasing = TRUE ),c(1,2,3)]
  
cat("\n\nDistancia del COSENO, mayor a menor.\n\n")
  
for (i in 1:100){
  minim = 6500
  corect = 6500 + final_df[i,1]
  number = final_df[i,2]
  a = number + minim
  cosa = myList[i]
  cat("\nLa distancia del coseno de", corect)
  cat(" es con", a)
  cat(" y es", final_df[i,3])
}
  
print("\nVisuacion por el titulo, similitudes por titulo\n")
  
for (i in 1:100){
  minim = 6500
  corect = 6500 + final_df[i,1]
  number = final_df[i,2]
  a = number + minim
  cosa = myList[i]
  cat("\nEs: \n", df[corect,1])
  cat("\ncon: \n", df[a,1])
}
