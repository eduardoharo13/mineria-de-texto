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




# Aislar el texto con los tweets: coffee_tweets
charlot_tweets <- charlot15$full_text

# Creaci?n de un corpus (colecci?n de documentos)                       
#-------------------------------------------------


# Crear un vector de tipo source 
# (interpretar cada elemento del vector como un documento)
charlot_source <- VectorSource(charlot_tweets)

# Crear un corpus  volatil (en memoria RAM)   
charlot_corpus <- VCorpus(charlot_source)

# Mostrar el objeto
charlot_corpus

# Mostrar el 15avo tweet en coffee_corpus
charlot_corpus[[15]][1]
content(charlot_corpus[[15]])

# Mostrar los metadatos del 15avo tweet en coffee_corpus
charlot_corpus[[15]][2]
meta(charlot_corpus[[15]])



sw<-stopwords(language = "en", source = "snowball", simplify = TRUE)
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

# Limpieza y Preprocesamiento                      
#-------------------------------------------------
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, sw)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus<- tm_map(corpus,removeURL)
  return(corpus)
}

# Aplicar la funci?n personalizada 
clean_corp <-clean_corpus(charlot_corpus)

# Mostrar los tweets limpios
clean_corp[[33]][1]

# Mostrar el texto original
charlot15$full_text[33]


# Crear un Document Term Matrix (DTM)                    
#-------------------------------------------------
# Crear el dtm del corpus
charlot_dtm <- DocumentTermMatrix(clean_corp)

# Mostrar el dtm
coffee_dtm

# Convertir dtm a matriz
coffee_m <- as.matrix(coffee_dtm)

# Mostrar las dimensiones
dim(coffee_m)

# crea matriz de terminos
charlot_tdm<- TermDocumentMatrix(clean_corp)
m_discursos

#Matriz inversa dtm.tfidf_discursos 
dtm.tfidf_discursos <- DocumentTermMatrix(discursos, control=list(weighting=weightTfIdf))
dtm.tfidf_discursos
