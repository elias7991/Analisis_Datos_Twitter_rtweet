# *******************LIBRERIA DE MANEJO DE API TWITTER*******************
library(rtweet)
# *******************LIBRERIA DE REPRESENTACION DE DATOS*******************
library("ggplot2")
# *******************REPRESENTACION ORDENADA DE TEXTO EN CONSOLA*******************
library(tidytext)
# *******************CONTIENE FUNCIONES DE CONTEO ENTRO OTRAS IMPORTANTES*******************
library(dplyr)



# *******************INFORMACION PARA AUTENTICACION*******************
app_name <- "Analisis_Datos_Twitter"
consumer_key <- "H2cC7eKrsCHqBoBZG6XYNeFAi"
consumer_secret <- "z2d9Riqn3JyeOvkQ0R0E6P89Huo22SEB92GdwAbK69SsS4zt3i"
access_token <- "1443289288817483787-Y23CyxwSuGhXX0M6yZzGKbmsvqNMQV"
access_token_secret <- "18nVpC79FpzY46PrkAQiB9gbXCT7XTRvoL1bJQQbtXIDr"


# *******************SECCION DE CREACION DE TOKEN DE USUARIO*******************
# *******************Twitter OAuth (token 1.0)*******************
token <- create_token(
  app = app_name,
  consumer_key,
  consumer_secret,
  access_token,
  access_token_secret
)

# *******************LISTA DE PALABRAS A IGNORAR*******************
no_usar <- list("yo", "tu", "el", "él", "nosotros", "ellos", "quien", "quién",
                "que", "qué", "a", "por", "no", "soy", "es", "hay", "porque",
                "acá", "los", "peor", "no", "en", "quienes", "son", "de")

# *******************ITEM 1*******************

# *******************PARAMETROS DE LA BUSQUEDA CONJUNTO 1 DE TWEETS*******************
# filtro de busqueda, como todas las palabras llevan al menos una vocal
query <- "a OR e OR i OR o OR u"
# numero maximo de tweets a consultar
max_tweets <- 10
# queremos solo tweets recientes
tipo_busqueda <- "recent"

# *******************BUSQUEDA DE TWEETS CONJUNTO 1*******************
tweets_1 <- search_tweets(
  q = query,
  n = max_tweets,
  include_rts = FALSE,
  lang = "es"
)

# ******************GUARDAR  EN TXT CONJUNTO 1******************
write.table(tweets_1$text, file = "conjunto_1.txt", fileEncoding = "latin1", sep = ",")

# ******************GUARDAMOS LA UBICACION DEL ARCHIVO DE TEXTO 1******************
ubicacion = "C:\\Users\\elias\\OneDrive\\Escritorio\\MIS COSAS\\POLITÉCNICA\\Primer Semestre 2021\\Estructura de los Lenguajes\\Segunda Parcial\\Semana 2\\r\\conjunto_1.txt"

# ******************TRAEMOS EL CONTENIDO DEL ARCHIVO COMO TAL******************
texto_1 <- read.table(ubicacion, sep = ",", encoding = "latin1")

# ******************CONVERTIMOS TODO A MINUSCULA******************
texto_1 <- tolower(texto_1$x)

# ******************SEPARAMOS POR ESPACIOS EL RESULTADO ANTERIOR Y SE GUARDA EN UN DATAFRAME******************
texto_1_split = strsplit(texto_1, split = " ")

# ******************AQUI ENLISTA CADA UNA DE LAS PALABRAS QUE SE ENCONTRARON******************
texto_1_col = as.character(unlist(texto_1_split))

# ******************LO CONVERTIMOS EN UN DATA FRAME******************
texto_1_col = data.frame(texto_1_col)

# ******************CAMBIAMOS EL NOMBRE DE LA COLUMNA POR "PALABRAS"******************
names(texto_1_col) = c("Palabras")

# ******************PODEMOS ELIMINAR ALGUNOS CARACTERES QUE NO NECESITAMOS******************
texto_1_col$Palabras = sub("([[:space:]])", "", texto_1_col$Palabras)
texto_1_col$Palabras = sub("([[:digit:]])", "", texto_1_col$Palabras)
texto_1_col$Palabras = sub("([[:punct:]])", "", texto_1_col$Palabras)

# ******************ELIMINAMOS TODAS LAS PALABRAS QUE NO QUEREMOS USAR******************
for (palabra in no_usar) {
  texto_1_col$Palabras = sub(palabra, "", texto_1_col$Palabras)
}

# ******************CREAMOS UNA LISTA VACIA QUE CONTENDRA LAS PALABRAS DEL CONJUNTO 1******************
palabras_1 <- list()

# ******************CREAMOS EL ITERADOR******************
i <- length(palabras_1)

# ******************GUARDAMOS TODAS LAS PALABRAS DEL CONJUNTO 1 EN UNA LISTA******************
for (palabra in texto_1_col$Palabras){
  palabras_1[[(i+1)]] <- palabra
  i <- i + 1
}

# ******************HISTOGRAMA 1******************
histograma_1 <- ggplot(texto_1_col, aes(x=as.array(Palabras))) +
  geom_bar(position = 'identity', show.legend = FALSE) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_classic()
histograma_1 + theme(axis.text.x = element_text(angle = 90))


# *******************ITEM 2*******************

# *******************PARAMETROS DE LA BUSQUEDA CONJUNTO 2 DE TWEETS*******************
# filtro de busqueda
query <- "Yolanda Park"

# *******************BUSQUEDA DE TWEETS CONJUNTO 2*******************
tweets_2 <- search_tweets(
  q = query,
  n = max_tweets,
  type = tipo_busqueda,
  include_rts = FALSE,
  lang = "es"
  # esperar por si sobrepasamos el limite de query
  #retryonratelimit = TRUE
)

# ******************GUARDAR  EN TXT CONJUNTO 2******************
write.table(tweets_2$text, file = "conjunto_2.txt", fileEncoding = "latin1", sep = ",")

# ******************GUARDAMOS LA UBICACION DEL ARCHIVO DE TEXTO 2******************
ubicacion = "C:\\Users\\elias\\OneDrive\\Escritorio\\MIS COSAS\\POLITÉCNICA\\Primer Semestre 2021\\Estructura de los Lenguajes\\Segunda Parcial\\Semana 2\\r\\conjunto_2.txt"

# ******************TRAEMOS EL CONTENIDO DEL ARCHIVO COMO TAL******************
texto_2 <- read.table(ubicacion, sep = ",", encoding = "latin1")

# ******************CONVERTIMOS TODO A MINUSCULA******************
texto_2 <- tolower(texto_2$x)

# ******************SEPARAMOS POR ESPACIOS EL RESULTADO ANTERIOR Y SE GUARDA EN UN DATAFRAME******************
texto_2_split = strsplit(texto_2, split = " ")

# ******************AQUI ENLISTA CADA UNA DE LAS PALABRAS QUE SE ENCONTRARON******************
texto_2_col = as.character(unlist(texto_2_split))

# ******************LO CONVERTIMOS EN UN DATA FRAME******************
texto_2_col = data.frame(texto_col)

# ******************CAMBIAMOS EL NOMBRE DE LA COLUMNA POR "PALABRAS"******************
names(texto_2_col) = c("Palabras")

# ******************PODEMOS ELIMINAR ALGUNOS CARACTERES QUE NO NECESITAMOS******************
texto_2_col$Palabras = sub("([[:space:]])", "", texto_2_col$Palabras)
texto_2_col$Palabras = sub("([[:digit:]])", "", texto_2_col$Palabras)
texto_2_col$Palabras = sub("([[:punct:]])", "", texto_2_col$Palabras)

# ******************ELIMINAMOS TODAS LAS PALABRAS QUE NO QUEREMOS USAR******************
for (palabra in no_usar) {
  texto_2_col$Palabras = sub(palabra, "", texto_2_col$Palabras)
}

# ******************CREAMOS UNA LISTA VACIA QUE CONTENDRA LAS PALABRAS DEL CONJUNTO 1******************
palabras_2 <- list()

# ******************CREAMOS EL ITERADOR******************
i <- length(palabras_2)

# ******************GUARDAMOS TODAS LAS PALABRAS DEL CONJUNTO 1 EN UNA LISTA******************
for (palabra in texto_2_col$Palabras){
  palabras_2[[(i+1)]] <- palabra
  i <- i + 1
}

# ******************HISTOGRAMA 2******************
histograma_2 <- ggplot(texto_2_col, aes(x=as.array(Palabras))) +
  geom_bar(position = 'identity', show.legend = FALSE) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_classic()
histograma_2 + theme(axis.text.x = element_text(angle = 90))


# ******************CONTAMOS EL LARGOR DE CADA UNA DE LAS PALABRAS******************
# texto_col$dim = nchar(texto_col$Palabras)

# *******************ITEM 3*******************

# *******************PALABRAS QUE SE ENCUENTRAN EN LA INTERSECCION*******************
interseccion <- intersect(palabras_1, palabras_2)
interseccion

# *******************ITEM 4*******************

union <- c(uno_no_dos, interseccion, dos_no_uno)
union

# *******************ITEM 5*******************

# *******************NUMERO DE TWEETS POR FECHA*******************
ggplot(tweets_1, aes(x=as.Date(created_at))) +
  geom_histogram(position = 'identity', bins = 30, show.legend = FALSE) +
  labs(x = "Fecha", y = "Numero de tweets") + 
  theme_classic()


# *******************ITEM 6*******************

# *******************NUMERO DE PERSONAS QUE TWITTEARON POR FECHA*******************

# ******************EXTRAEMOS UN PEQUEÑO DATAFRAME QUE CONTIENE ID DE USUARIO Y FECHA******************
usuarios_fecha_rep <- tweets_1[,c(1,3)]

# ******************ELIMINAMOS LOS ID REPETIDOS******************
usuarios_fecha_no_rep <- usuarios_fecha_rep %>% group_by(created_at) %>% distinct(user_id)

# ******************Y GRAFICAMOS CONTANDO LOS ID QUE SE LEEN POR FECHA******************
ggplot(usuarios_fecha_no_rep, aes(x=as.Date(created_at))) + 
  geom_histogram(position = 'identity', bins = 30, show.legend = FALSE) +
  labs(x = "Fecha", y = "Numero de usuarios") +
  theme_classic()
  

# ******************ITEM 7******************

# ******************CONTAMOS LAS VECES QUE APARECE CADA ID PARA SABER CUANTOS TWEETS REALIZO******************
tweets_usuario <- tweets_1 %>% count(screen_name, sort = TRUE)

# *****************GRAFICAMOS EL RESULTADO*****************
grafico <- ggplot(tweets_usuario, aes(x=as.array(screen_name))) +
  geom_bar(position = 'identity', show.legend = FALSE) +
  labs(x = "Nombre de usuario", y = "Numero de tweets") +
  theme_classic()
grafico + theme(axis.text.x = element_text(angle = 90))
  

# *******************ITEM 8*******************

# *******************ENCONTRAR LAS PALABRAS DIFERENTES*******************
# ******************* PALABRAS DEL CONJUNTO 1 QUE NO ESTAN EN EL CONJUNTO 2
uno_no_dos <- setdiff(palabras_1, palabras_2)
uno_no_dos

# ******************* PALABRAS DEL CONJUNTO 2 QUE NO ESTAN EN EL CONJUNTO 1
dos_no_uno <- setdiff(palabras_2, palabras_1)
dos_no_uno


