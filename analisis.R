#Análisis de twitters durante el atentado de Charlottesville
#Descargamos las bases de datos desde Kaggle
#Link:https://www.kaggle.com/vincela9/charlottesville-on-twitter

#Cargamos las bases de datos
library(haven)
library(stopwords)
library(tm)
charlot15<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/aug15_sample/aug15_sample.csv",header = T, sep=",")
charlot16<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/aug16_sample/aug16_sample.csv",header = T, sep=",")
charlot17<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/aug17_sample/aug17_sample.csv",header = T, sep=",")
charlot18<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/aug18_sample/aug18_sample.csv",header = T, sep=",")
#tweet_count_time_series<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/tweet_count_time_series.csv/tweet_count_time_series.csv",header = T, sep=",")

#Realizamos un append entre las 2 bases de datos
charlot<-charlot15

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
sw<-stopwords::stopwords("en", source = "snowball")
#funcion para remover las urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, sw)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus<- tm_map(corpus,content_transformer(removeURL))
  return(corpus)
}

# Aplicar la funci?n personalizada 
clean_corp <-clean_corpus(charlot_corpus)


# Mostrar los tweets limpios
clean_corp[[1]][1]

# Mostrar el texto original
charlot15$full_text[1]


# Crear un Document Term Matrix (DTM)                    
#-------------------------------------------------
# Crear el dtm del corpus
charlot_dtm <- DocumentTermMatrix(clean_corp)

# Mostrar el dtm
charlot_dtm

# Convertir dtm a matriz
charlot_m <- as.matrix(charlot_dtm)

# Mostrar las dimensiones
dim(charlot_m)

# crea matriz de terminos
charlot_tdm<- TermDocumentMatrix(clean_corp)
charlot_tdm


