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
library(tidytext)
library(topicmodels)
library(doParallel)
library(scales)
library(slam)

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
# Determinar el Numero de Topicos
library(ldatuning)

result <- FindTopicsNumber(
  charlot_dtm,
  topics = seq(from = 2, to = 40, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 6L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

#El método de Griffiths 2004 da como número de topicos óptimo  29
result[which(result$Griffiths2004==max(result$Griffiths2004),arr.ind = T),]$topics

#El método de CaoJuan2009 da como número de topicos óptimo  38
result[which(result$CaoJuan2009==min(result$CaoJuan2009),arr.ind = T),]$topics

#El método de CaoJuan2009 da como número de topicos óptimo  40
result[which(result$Arun2010==min(result$Arun2010),arr.ind = T),]$topics

#Para propósitos de este trabajo se selecciona 29 tópicos

#Tambien realizamos la validación con el índice de perplejidad
#con los métodos de gibbs y tópicos correlacionados (ctm)


##############################
###########Estimacion#########
##############################

#Creamos la data de entrenamiento y prueba
n <- nrow(charlot_dtm)
#Se eliminan las filas vacias
sel_idx <- slam::row_sums(charlot_dtm) > 0
charlot_dtm <- charlot_dtm[sel_idx, ]

#se realiza el split de la data
splitter <- sample(1:n, round(n * 0.75))
train_set <- charlot_dtm[splitter, ]
valid_set <- charlot_dtm[-splitter, ]

#Creamos las listas para obtener los indices de perplejidad de cada tópico
fitted<-list()
perp_train<-list()
perp_test<-list()



for (i in 2:29){
fitted[[i]] <- LDA(train_set, k = i, method = "Gibbs",
              control = list(burnin =1000, iter = 500, keep = 50) )
perp_train[[i]]<-perplexity(fitted[[i]], newdata = train_set)
perp_test[[i]]<-perplexity(fitted[[i]], newdata = valid_set)
}


mod_gibbs= as.data.frame(do.call(rbind, lapply(perp_test, unlist)))
mod_gibbs$numero<-cbind(2:29)
mod_gibbs$numero[mod_gibbs$V1==min(mod_gibbs$V1)]

#Realizamos el cálculo por el método de topicos correlacionados (CTM) usando VEM
fitted_ctm<-list()
perp_train_ctm<-list()
perp_test_ctm<-list()

for (i in 2:29){
  
  fitted_ctm[[i]] <- CTM(train_set, k = i, control=list(seed=2021,var=list(tol=10^-4),
                                                    em=list(tol=10*10^-4)))
  perp_train_ctm[[i]]<-perplexity(fitted_ctm[[i]], newdata = train_set)
  perp_test_ctm[[i]]<-perplexity(fitted_ctm[[i]], newdata = valid_set)
}


mod_ctm= as.data.frame(do.call(rbind, lapply(perp_test_ctm, unlist)))
mod_ctm$numero<-cbind(2:29)
mod_ctm$numero[mod_ctm$V1==min(mod_ctm$V1)]

#Ambos modelos dan un menor puntaje al indice de perplejidad por lo que 
# se decide utilizar 29 tópicos



###############################
#######Estimación##############
###############################

#Estimamos tanto el modelo mediante el método de Gibbs como el CTM
charlot_tm=list(R1=LDA(charlot_dtm,k=29,method="Gibbs",
                   control=list(seed=S,burnin=500,thin=100,iter=500)),
                R2=CTM(charlot_dtm,k=29,control=list(seed=2021,var=list(tol=10^-4),
                                             em=list(tol=10*10^-4))))

#Primeros 8 términos obtenidos del método de estimación de Gibbs
terms(charlot_tm$R1,8)
#Primeros 8 términos obtenidos del  método CTM
terms(charlot_tm$R2,8)


##Método de Gibbs
# Probabilidad tema por palabra
charlot_tm_gibbs_beta <- tidy(charlot_tm[["R1"]], matrix = "beta")
glimpse(charlot_tm_beta)

# Probabilidad topico por documento
charlot_tm_gibbs_gamma <- tidy(charlot_tm[["R1"]], matrix = "gamma")
glimpse(charlot_tm_gamma)


##Método CTM
# Probabilidad tema por palabra
charlot_tm_ctm_beta <- tidy(charlot_tm[["R2"]], matrix = "beta")
glimpse(charlot_tm_beta)

# Probabilidad topico por documento
charlot_tm_ctm_gamma <- tidy(charlot_tm[["R2"]], matrix = "gamma")
glimpse(charlot_tm_gamma)


# principales terminos en cada topico
charlot_tm_gibbs_beta %>% 
  group_by(topic) %>%
  top_n(6) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% # vamos a mostrarlo como grafico
  ggplot(aes(x=reorder(term, (beta)),y=beta)) + 
  geom_col() +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip()

