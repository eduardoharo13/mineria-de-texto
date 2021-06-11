#Análisis de twitters durante el atentado de Charlottesville
#Descargamos las bases de datos desde Kaggle
#Link:https://www.kaggle.com/vincela9/charlottesville-on-twitter

#Cargamos las bases de datos
library(parallel)
library(haven)
library(stopwords)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(grDevices)
library(animation)
library(ggplot2)

charlot<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/aug15_sample/aug15_sample.csv",header = T, sep=",")

# Aislar el texto con los tweets
charlot_tweets <- charlot$full_text


# Creaci?n de un corpus (colecci?n de documentos)                       
#-------------------------------------------------
# Crear un vector de tipo source 
# (interpretar cada elemento del vector como un documento)
charlot_source <- VectorSource(charlot_tweets)

# Crear un corpus  volatil (en memoria RAM)   
charlot_corpus <- VCorpus(charlot_source)

# Mostrar el objeto
charlot_corpus

# Mostrar el 1er tweet del corpus
charlot_corpus[[1]][1]
content(charlot_corpus[[1]])



# Limpieza y Preprocesamiento                      
#-------------------------------------------------
#stopwords
sw<-stopwords::stopwords("en", source = "stopwords-iso")
sw<-c(sw,"via","just","amp")
#funcion para remover las urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,sw)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus<- tm_map(corpus,content_transformer(removeURL))
  return(corpus)
}

# Aplicar la funci?n personalizada 
clean_corp <-clean_corpus(charlot_corpus)


# Mostrar los tweets limpios
clean_corp[[1]][1]

# Mostrar el texto original
charlot$full_text[1]


# Crear un Document Term Matrix (DTM)                    
#-------------------------------------------------
# Crear el dtm del corpus
charlot_dtm <- DocumentTermMatrix(clean_corp)
charlot_dtm

# crea matriz de terminos
charlot_tdm<- TermDocumentMatrix(clean_corp)
charlot_tdm


# Convertir dtm a matriz
charlot_m <- as.matrix(charlot_tdm)

# Mostrar las dimensiones
dim(charlot_m)


# Calcular la frecuencia de los t?rminos
term_frequency <- rowSums(charlot_m)

# Ordenar los terminos de acuerdo a su frecuencia
term_frequency <- sort(term_frequency, decreasing = TRUE)

# Ver los 20 terminos mas frecuentes
term_frequency[1:20]

# Grafico de barras con los terminos mas frecuentes
barplot(term_frequency[1:20], col = "tan", las = 2)


## Nubes de palabras (Word Clouds)
#------------------------------------------
# Vector de terminos
terms_vec <- names(term_frequency)

# Crear una nube de palabras 
wordcloud(terms_vec,term_frequency, scale=c(8,.2),min.freq=100,max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8, "Dark2"))
