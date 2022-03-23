# *******************LIBRERIA DE MANEJO DE API TWITTER*******************
library(rtweet)
# *******************LIBRERIA DE REPRESENTACION DE DATOS*******************
library(ggplot2)
# *******************REPRESENTACION ORDENADA DE TEXTO EN CONSOLA*******************
library(tidytext)
# *******************FUNCIONES DE STRING*******************
library(tidyverse)
# *******************CONTIENE FUNCIONES DE CONTEO ENTRO OTRAS IMPORTANTES*******************
library(dplyr)



# *******************INFORMACION PARA AUTENTICACION*******************
app_name <- "Analisis_Datos_Twitter"
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_token_secret <- ""


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
                "acá", "los", "peor", "no", "en", "quienes", "son", "de", "al",
                "me", "con", "mi", "si", "sí", "una", "ya", "y", "o", "u", "eso",
                "la", "del", "las", "aun", "aún", "creo", "voy", "ver", "fin", 
                "pq", "xq", "fin", "pero", "sos", "somos", "son", "ni", "nos", "berizzo",
                "Berizzo")

# *******************ITEM 1*******************

# *******************PARAMETROS DE LA BUSQUEDA CONJUNTO 1 DE TWEETS*******************
# filtro de busqueda, como todas las palabras llevan al menos una vocal
query <- "a OR e OR i OR o OR u"
# numero maximo de tweets a consultar
max_tweets <- 5000
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
#eliminar los emojis
texto_1_col$Palabras = gsub("<\\S+", "", texto_1_col$Palabras)
# eliminar los espacios
texto_1_col$Palabras = sub("([[:space:]])", "", texto_1_col$Palabras)
#texto_1_col$Palabras = sub("([[:digit:]])", "", texto_1_col$Palabras)
texto_1_col$Palabras = sub("([[:punct:]])", "", texto_1_col$Palabras)
# eliminamos los arrobas
texto_1_col$Palabras = gsub("@\\w+", "", texto_1_col$Palabras)
# eliminamos los links
texto_1_col$Palabras = gsub("http\\S+", "", texto_1_col$Palabras)
# eliminamos los hashtag
texto_1_col$Palabras = gsub("#\\S+", "", texto_1_col$Palabras)
# eliminamos todos los numeros
texto_1_col$Palabras = gsub("\\d+", "", texto_1_col$Palabras)


# ******************ELIMINAMOS TODAS LAS PALABRAS QUE NO QUEREMOS USAR******************
for (palabra in no_usar) {
  patron <- str_c("^", palabra, "$")
  texto_1_col$Palabras = sub(patron, "", texto_1_col$Palabras)
}

# ******************ELIMINAMOS TODAS LAS PALABRAS CORTAS******************
for (palabra in texto_1_col$Palabras) {
  if (str_length(palabra) < 4) {
    patron <- str_c("^", palabra, "$")
    texto_1_col$Palabras = sub(patron, "", texto_1_col$Palabras)
  }
}

# ******************AHORA DEBEMOS ELIMINAR LAS FILAS VACIAS******************
texto_1_col <- texto_1_col[str_length(texto_1_col$Palabras) > 0, ]

# ******************LO CONVERTIMOS EN UN DATA FRAME******************
texto_1_col = data.frame(texto_1_col)

# ******************CAMBIAMOS EL NOMBRE DE LA COLUMNA POR "PALABRAS"******************
names(texto_1_col) = c("Palabras")

# ******************CREAMOS UNA LISTA VACIA QUE CONTENDRA LAS PALABRAS DEL CONJUNTO 1******************
palabras_1 <- list()

# ******************CREAMOS EL ITERADOR******************
i <- length(palabras_1)

# ******************GUARDAMOS TODAS LAS PALABRAS DEL CONJUNTO 1 EN UNA LISTA******************
for (palabra in texto_1_col$Palabras){
  palabras_1[[(i+1)]] <- palabra
  i <- i + 1
}

# ************************************************************************
# ******************GRAFICO DEL ITEM 1************************************
# ************************************************************************

# ******************HISTOGRAMA 1******************
# ******************CONTAMOS LAS PALABRA******************
conteo_1 <- texto_1_col %>% count(Palabras)
# ******************SACAMOS LOS 20 VALORES MÁS ALTOS******************
top_20 <- top_n(conteo_1, n = 20)
# ******************REALIZAMOS EL GRAFICO DEL ITEM 1******************
histograma_1 <- ggplot(top_20, aes(x=as.array(Palabras), y=n)) +
  geom_bar(position = 'identity', show.legend = FALSE, stat = "identity") +
  ggtitle("Grafico item 1 - Palabras del conjunto 1") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_bw()

histograma_1 + theme(axis.text.x = element_text(angle = 90))






# *******************ITEM 2*******************

# *******************PARAMETROS DE LA BUSQUEDA CONJUNTO 2 DE TWEETS*******************
# filtro de busqueda
query <- "Berizzo"
# *******************BUSQUEDA DE TWEETS CONJUNTO 2*******************
tweets_2 <- search_tweets(
  q = query,
  n = max_tweets,
  type = tipo_busqueda,
  include_rts = FALSE,
  lang = "es"
)

# ******************GUARDAR  EN TXT CONJUNTO 1******************
write.table(tweets_2$text, file = "conjunto_2.txt", fileEncoding = "latin1", sep = ";")

# ******************GUARDAMOS LA UBICACION DEL ARCHIVO DE TEXTO 1******************
ubicacion = "C:\\Users\\elias\\OneDrive\\Escritorio\\MIS COSAS\\POLITÉCNICA\\Primer Semestre 2021\\Estructura de los Lenguajes\\Segunda Parcial\\Semana 2\\r\\conjunto_2.txt"

# ******************TRAEMOS EL CONTENIDO DEL ARCHIVO COMO TAL******************
texto_2 <- read.table(ubicacion, sep = ",", encoding = "latin1")

# ******************CONVERTIMOS TODO A MINUSCULA******************
texto_2 <- tolower(texto_2$V1)

# ******************SEPARAMOS POR ESPACIOS EL RESULTADO ANTERIOR Y SE GUARDA EN UN DATAFRAME******************
texto_2_split = strsplit(texto_2, split = " ")

# ******************AQUI ENLISTA CADA UNA DE LAS PALABRAS QUE SE ENCONTRARON******************
texto_2_col = as.character(unlist(texto_2_split))

# ******************LO CONVERTIMOS EN UN DATA FRAME******************
texto_2_col = data.frame(texto_2_col)

# ******************CAMBIAMOS EL NOMBRE DE LA COLUMNA POR "PALABRAS"******************
names(texto_2_col) = c("Palabras")

# ******************PODEMOS ELIMINAR ALGUNOS CARACTERES QUE NO NECESITAMOS******************
#eliminar los emojis
texto_2_col$Palabras = gsub("<\\S+", "", texto_2_col$Palabras)
# eliminar los espacios
texto_2_col$Palabras = sub("([[:space:]])", "", texto_2_col$Palabras)
texto_2_col$Palabras = sub("([[:punct:]])", "", texto_2_col$Palabras)
# eliminamos los arrobas
texto_2_col$Palabras = gsub("@\\w+", "", texto_2_col$Palabras)
# eliminamos los links
texto_2_col$Palabras = gsub("http\\S+", "", texto_2_col$Palabras)
# eliminamos los hashtag
texto_2_col$Palabras = gsub("#\\S+", "", texto_2_col$Palabras)
# eliminamos todos los numeros
texto_2_col$Palabras = gsub("\\d+", "", texto_2_col$Palabras)

# ******************ELIMINAMOS TODAS LAS PALABRAS QUE NO QUEREMOS USAR******************
for (palabra in no_usar) {
  patron <- str_c("^", palabra, "$")
  texto_2_col$Palabras = sub(patron, "", texto_2_col$Palabras)
}

# ******************ELIMINAMOS TODAS LAS PALABRAS CORTAS******************
for (palabra in texto_2_col$Palabras) {
  if (str_length(palabra) < 4) {
    patron <- str_c("^", palabra, "$")
    texto_2_col$Palabras = sub(patron, "", texto_2_col$Palabras)
  }
}

# ******************AHORA DEBEMOS ELIMINAR LAS FILAS VACIAS******************
texto_2_col <- texto_2_col[str_length(texto_2_col$Palabras) > 0, ]

# ******************LO CONVERTIMOS EN UN DATA FRAME******************
texto_2_col = data.frame(texto_2_col)

# ******************CAMBIAMOS EL NOMBRE DE LA COLUMNA POR "PALABRAS"******************
names(texto_2_col) = c("Palabras")

# ******************CREAMOS UNA LISTA VACIA QUE CONTENDRA LAS PALABRAS DEL CONJUNTO 1******************
palabras_2 <- list()

# ******************CREAMOS EL ITERADOR******************
i <- length(palabras_2)

# ******************GUARDAMOS TODAS LAS PALABRAS DEL CONJUNTO 1 EN UNA LISTA******************
for (palabra in texto_2_col$Palabras){
  palabras_2[[(i+1)]] <- palabra
  i <- i + 1
}


# ************************************************************************
# ******************GRAFICO DEL ITEM 2************************************
# ************************************************************************

# ******************HISTOGRAMA 2******************
# ******************CONTAMOS LAS PALABRA******************
conteo_2 <- texto_2_col %>% count(Palabras)
# ******************SACAMOS LOS 20 VALORES MÁS ALTOS******************
top_20 <- top_n(conteo_2, n = 20)
# ******************REALIZAMOS EL GRAFICO DEL ITEM 1******************
histograma_2 <- ggplot(top_20, aes(x=as.array(Palabras), y=n)) +
  geom_bar(position = 'identity', show.legend = FALSE, stat = "identity") +
  ggtitle("Grafico item 2 - Palabras del conjunto 2") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_bw()

histograma_2 + theme(axis.text.x = element_text(angle = 90))








# *******************ITEM 3*******************

# *******************PALABRAS QUE SE ENCUENTRAN EN LA INTERSECCION*******************
# *******************COMBINAMOS LOS DATAFRAME QUE CONTIENEN LOS CONTEOS DE PALABRAS*****************
m <- merge(conteo_1, conteo_2, by = "Palabras")
# *******************MUTAMOS EL DATAFRAME AGREGANDO UNA NUEVA COLUMNA QUE RESULTA DE SUMAR LAS ANTERIORES*************
m <- mutate(m, n = n.x+n.y)

# ******************SACAMOS LOS 20 VALORES MÁS ALTOS******************
top_20 <- top_n(m, n = 20, wt = n)
# ******************REALIZAMOS EL GRAFICO DEL ITEM 3******************
histograma_3 <- ggplot(top_20, aes(x=as.array(Palabras), y=n)) +
  geom_bar(position = 'identity', show.legend = FALSE, stat = "identity") +
  ggtitle("Grafico item 3 - Tendencias en la interseccion") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_bw()

histograma_3 + theme(axis.text.x = element_text(angle = 90))






# *******************ITEM 4*******************
# *******************UNIMOS LOS DATAFRAME*******************
u <- union_all(conteo_1, conteo_2)
# ******************SACAMOS LOS 20 VALORES MÁS ALTOS******************
top_20 <- top_n(u, n = 20)
# ******************REALIZAMOS EL GRAFICO DEL ITEM 3******************
histograma_4 <- ggplot(top_20, aes(x=as.array(Palabras), y=n)) +
  geom_bar(position = 'identity', show.legend = FALSE, stat = "identity") +
  ggtitle("Grafico item 4 - Tendencias en la union") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_bw()

histograma_4 + theme(axis.text.x = element_text(angle = 90))







# *******************ITEM 5*******************

# *******************NUMERO DE TWEETS POR FECHA*******************
ggplot(tweets_1, aes(x=as.Date(created_at))) +
  geom_histogram(position = 'identity', bins = 30, show.legend = FALSE) +
  labs(x = "Fecha", y = "Numero de tweets") + 
  ggtitle("Grafico item 5 - Numero de tweets por fecha") + 
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
  ggtitle("Grafico item 6 - Numero de usuarios por fecha") +
  theme_classic()
  








# ******************ITEM 7******************

# ******************CONTAMOS LAS VECES QUE APARECE CADA ID PARA SABER CUANTOS TWEETS REALIZO******************
tweets_usuario <- tweets_1 %>% count(screen_name, sort = TRUE)

top_20 <- top_n(tweets_usuario, n = 20, wt = n)

# *****************GRAFICAMOS EL RESULTADO*****************
grafico <- ggplot(top_20, aes(x=as.array(screen_name))) +
  geom_bar(position = 'identity', show.legend = FALSE) +
  labs(x = "Nombre de usuario", y = "Numero de tweets") +
  ggtitle("Grafico item 7 - Numero de tweets por usuario")
  theme_classic()
grafico + theme(axis.text.x = element_text(angle = 90))
  






# *******************ITEM 8*******************

# *******************ENCONTRAR LAS PALABRAS DIFERENTES*******************
# ******************* PALABRAS DEL CONJUNTO 1 QUE NO ESTAN EN EL CONJUNTO 2
uno_no_dos <- setdiff(conteo_1, conteo_2)
top_20 <- top_n(uno_no_dos, n = 20)
# ******************REALIZAMOS EL GRAFICO DEL ITEM 3******************
histograma_8_1 <- ggplot(top_20, aes(x=as.array(Palabras), y=n)) +
  geom_bar(position = 'identity', show.legend = FALSE, stat = "identity") +
  ggtitle("Grafico item 8 - Diferencia 1 menos 2") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_bw()

histograma_8_1 + theme(axis.text.x = element_text(angle = 90))





# ******************* PALABRAS DEL CONJUNTO 2 QUE NO ESTAN EN EL CONJUNTO 1
dos_no_uno <- setdiff(conteo_2, conteo_1)
top_20 <- top_n(dos_no_uno, n = 20)
# ******************REALIZAMOS EL GRAFICO DEL ITEM 3******************
histograma_8_2 <- ggplot(top_20, aes(x=as.array(Palabras), y=n)) +
  geom_bar(position = 'identity', show.legend = FALSE, stat = "identity") +
  ggtitle("Grafico item 8 - Diferencia 2 menos 1") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5) +
  labs(x = "Palabras", y = "Frecuencia Absoluta") +
  theme_bw()

histograma_8_2 + theme(axis.text.x = element_text(angle = 90))


