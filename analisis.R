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
library(quanteda)
library(stringr)
library(broom)
library(tidyr)
library(tidyverse)

charlot<-read.csv("D:/Maestria_Estadistica_PUCP/III-ciclo/temas en estadistica contemporanea/charlotesville/aug15_sample/aug15_sample.csv",header = T, sep=",")

# Aislar el texto con los tweets
charlot_tweets <- charlot$full_text
charlot_tweets <- str_replace_all(string=charlot_tweets, pattern= "[&â€¦™ðŸ¥]" , replacement= "")


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
#Función para remover caracteres especiales
removecaract<- function(x){
  gsub("/"," ", x)
  gsub("@", " ", x)
  gsub("\\|", " ", x)
}

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,sw)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus<- tm_map(corpus,content_transformer(removeURL))
  corpus<- tm_map(corpus,content_transformer(removecaract))
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


# Calcular la frecuencia de los terminos
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
#Nube de palabras con los términos más usados 
wordcloud(terms_vec,term_frequency, scale=c(8,.2),min.freq=100,max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8, "Dark2"))

#############################
########Topic modelling#####
###########################
library(topicmodels)
library(doParallel)
library(scales)

#Hacemos validación cruzada para obtener el número de tópicos a utilizar
burnin = 1000
iter = 1000
keep = 50

#Creamos la data de entrenamiento y prueba
n <- nrow(charlot_dtm)
splitter <- sample(1:n, round(n * 0.75))
train_set <- charlot_dtm[splitter, ]
valid_set <- charlot_dtm[-splitter, ]

#Creamos las listas para obtener los indices de perplejidad de cada tópico
fitted<-list()
perp_train<-list()
perp_test<-list()

#Realizamos el cálculo por el método de estimación de Gibbs
for (i in 2:20){
fitted[[i]] <- LDA(train_set, k = i, method = "Gibbs",
              control = list(burnin = burnin, iter = iter, keep = keep) )
perp_train[[i]]<-perplexity(fitted[[i]], newdata = train_set)
perp_test[[i]]<-perplexity(fitted[[i]], newdata = valid_set)
}


mod_gibbs= as.data.frame(do.call(rbind, lapply(perp_test, unlist)))
mod_gibbs$numero<-cbind(2:20)
mod_gibbs$numero[mod_gibbs$V1==min(mod_gibbs$V1)]

#Realizamos el cálculo por el método de topicos correlacionados (CTM) usando VEM
fitted_ctm<-list()
perp_train_ctm<-list()
perp_test_ctm<-list()

for (i in 2:20){
  fitted_ctm[[i]] <- CTM(train_set, k = i, control=list(seed=S,var=list(tol=tt),
                                                    em=list(tol=10*tt)))
  perp_train_ctm[[i]]<-perplexity(fitted_ctm[[i]], newdata = train_set)
  perp_test_ctm[[i]]<-perplexity(fitted_ctm[[i]], newdata = valid_set)
}


mod_ctm= as.data.frame(do.call(rbind, lapply(perp_test_ctm, unlist)))
mod_ctm$numero<-cbind(2:20)
mod_ctm$numero[mod_ctm$V1==min(mod_ctm$V1)]

#Ambos modelos dan un menor puntaje al indice de perplejidad por lo que 
# se decide utilizar 20 tópicos



###############################
#######Estimación##############
###############################

#Se eliminan las filas vacias
sel_idx <- slam::row_sums(charlot_dtm) > 0
charlot_dtm <- charlot_dtm[sel_idx, ]


# Número de temas
K <- 20
#Definimos una semilla
S = 2021
#Tolerancia
tt = 10^-4
#Estimamos tanto el modelo mediante el método de Gibbs como el CTM
charlot_tm=list(R1=LDA(charlot_dtm,k=20,method="Gibbs",
                   control=list(seed=S,burnin=500,thin=100,iter=500)),
                R2=CTM(charlot_dtm,k=20,control=list(seed=S,var=list(tol=tt),
                                             em=list(tol=10*tt))))

#Términos del modelo obtenidos mediante el método de estimación de Gibbs
terms(charlot_tm$R1, 20)
#Términos del modelo obtenidos mediante el método CTM
terms(charlot_tm$R2, 20)



# Probabilidad topico 
charlot_tm_gamma <- tidy(charlot_tm[["R1"]]@gamma, matrix = "gamma")
glimpse(charlot_tm_gamma)

