# Librerias:
library(tidyverse)
library(corrplot)
###################

#1) Descarga el csv de los datos del COVID http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip,
# importa los datos en R.  RECOMENDACION: usar read_csv

Datos <- read_csv("covid_dataset.csv")
# Datos totales. Datos <- read_csv("220424COVID19MEXICO.csv")

#2) Extrae una muestra aleatoria de 100k registros y asignala en una nueva variable. A partir de ahora trabaja con este dataset
# HINT: usar funcion sample_n de dplyr

# Datos <- sample_n(Datos, size = 100000)
# muestra
# Intent? pero mi pc no pudo con el trabajo
#3)Haz un resumen estadistico del dataset y 
# tambien muestra los tipos de datos por columna

# Resumen estad?stico
summary(Datos)
# Tipo de datos
glimpse(Datos)

#4)Filtra los renglones que dieron positivo para SARS-COVID y calcula el numero de registros
## Los casos positivos son aquellos que en la columna CLASIFICACION_FINAL tienen 1, 2 o 3


# Data frame con los resgistros de casos positivos
Casos_positivos <- Datos %>% select(CLASIFICACION_FINAL) %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3 ) %>%
  summarise("Casos Positivos" = n())
Casos_positivos


#5)Cuenta el numero de registros nulos por columna (HINT: Usar sapply o map, e is.na)

Dato_faltantes <- Datos %>% 
  sapply(is.na) %>% 
  as.data.frame() %>% 
  sapply(sum) %>% 
  as.data.frame()

Dato_faltantes

#6)
##a)Calcular la media de edades de los contagiados de covid

Media_casos_positivos <- Datos$EDAD %>% mean()
Media_casos_positivos

##b)Realiza un Histograma de las edades de los contagiados 

ggplot(Datos, aes(x = EDAD)) +
  geom_histogram(binwidth=0.1, aes(fill=..count..), col='black')

# Se observa que existe una tendencia a los adultos jovenes de la enfermedad. 

##c)Realiza una grafica de densidad de edades de los contagiados

ggplot(Datos, aes(x = EDAD))+
  geom_density(position = "stack")

#7)Agregar una columna nueva al dataframe que tenga valor 1 cuando la fecha
# de defuncion no es valor nulo y 0 cuando es nulo 
## La columna que contiene la fecha de defuncion se llama FECHA_DEF 
## HINT: Usa mutate, ifelse e is.na

Datos_columna_extra <- Datos %>% 
  mutate(Categoria_Defuncion = ifelse(is.na(FECHA_DEF), 0, 1))

#8)Hacer un boxplot de edades de los muertos por covid vs lo que no murieron para ver si detectamos diferencias y escribe tus conclusiones

# Cambiamos de nombres de "0" a "Muertes" y "1" a "vivos" para la estetica de la
# gr?fiaca. Con este data frame vamos a graficas los boxplots, por eso no tenemos 
# toda la tabla.

Datos_edad_fallecitos <-  Datos_columna_extra %>% 
  transform(Categoria_Defuncion = case_when(
  Categoria_Defuncion == 1 ~ "Muertes",
  TRUE ~ "Vivos"
  )) 


ggplot(data = Datos_edad_fallecitos, aes(x = as.factor(Categoria_Defuncion), y =  EDAD, fill = Categoria_Defuncion )) + 
  geom_boxplot() + ylab("Edad") + xlab("")

# Observaciones. Se distinque el hecho de que las personas fallecidas estaban en una edad superior a los 60 a?os,
# son atipicos las muertes en menores de 25 a?os, donde la mayoria de las personas contagiadas
# no fallecieron. 

summary(Datos_edad_fallecitos) 

#9)Transforma la columna CLASIFICACION_FINAL, que tenga valor de 1 si tiene 1, 2 o 3 como valor y que tenga 0 en cualquier otro caso
## HINT: Usar transform o mutate


Datos_Mod_Clas_Fin <- Datos %>% transform(CLASIFICACION_FINAL = case_when(
CLASIFICACION_FINAL == 1 ~ 1,
CLASIFICACION_FINAL == 2 ~ 1,
CLASIFICACION_FINAL == 3 ~ 1,
TRUE ~ 0
))

#10)Cuenta el numero de casos positivos agrupado por estado
# y realiza una grafica de barras de los 10 estados con mas casos
## HINT: Usar groupby, summarize, n(), y ggplot2

Datos_estados <- Datos_Mod_Clas_Fin %>% 
  group_by(ENTIDAD_NAC) %>% 
  summarize("n" = n()) %>% 
  arrange(desc(n)) 

ggplot(Datos_estados[1:10,], aes(x = ENTIDAD_NAC, y = n)) + geom_bar(stat="identity")


#11)Renombra la columna llamada CLASIFICACION FINAL para que ahora su nombre sea: "CONTAGIADO"

Datos <- Datos %>% rename(CLASIFICACION_FINAL = CONTAGIADO)

#12)Realiza una funcion que al aplicarla nos diga el procentaje 
# del total de registros que estan contagiados por Covid
#Ejemplo: al correr la funcion porcentaje_contagios(mi_dataframe) el resultado sea: 20.5%

mi_dataframe <- read_csv("covid_dataset.csv")

porcentaje_contagiados <- function(mi_dataframe) {
    x <-  mi_dataframe %>%
    transform(CLASIFICACION_FINAL = case_when(
      CLASIFICACION_FINAL == 1 ~ 1,
      CLASIFICACION_FINAL == 2 ~ 1,
      CLASIFICACION_FINAL == 3 ~ 1,
      TRUE ~ 0
    )) %>%
    select(CLASIFICACION_FINAL)
    
  sum(x$CLASIFICACION_FINAL) / length(x$CLASIFICACION_FINAL) * 100 
  
  }

porcentaje_contagiados(Datos)


#13)Realiza una matriz de corrrelacion entre las variables numericas 
# y concluye
## HINT: 

nums <- unlist(lapply(Datos_columna_extra, is.numeric)) 
Datos_numericos <- Datos_columna_extra[,nums]

cor(Datos_numericos) %>% corrplot()

## Se concluye la relacion entre las mujeres embarazadas con la edad. De intubados con UCI, embarazos con sexo pero esa
# es un poco evidente. Es importante recalcar que existe una correlaci?n mayor con las enfermedades
# respiratorias y cardiacas. 


#14)Realiza algun analisis, conteo por grupo y/o grafica que te parezca relevante para complementar el estudio y concluye

# Vamos a ver la relacion que existe entre fallecimientos con, intubados e hipertensos. Vimos
# Que la edad fue un factor importante.

# Este data frame es con datos de personas que fallecieron
Prom_14 <- Datos_columna_extra %>% filter(Categoria_Defuncion == 0)  %>%
  select(EDAD, SEXO, NEUMONIA, HIPERTENSION, CARDIOVASCULAR) %>%
 transform(SEXO = case_when(
  SEXO == 1 ~ "Femenino",
  SEXO == 2 ~ "Masculino",
  TRUE ~ "No se especifica"
 )) %>% 
  transform(NEUMONIA = case_when(
    NEUMONIA == 1 ~ "SI",
    NEUMONIA == 2 ~ "NO",
    NEUMONIA == 97 ~ "No aplica",
    TRUE ~ "No se especifica"
  )) %>% 
  transform(HIPERTENSION = case_when(
    HIPERTENSION == 1 ~ "SI",
    HIPERTENSION == 2 ~ "NO",
    HIPERTENSION == 97 ~ "No aplica",
    TRUE ~ "No se especifica"
  )) %>% 
  transform(CARDIOVASCULAR = case_when(
    CARDIOVASCULAR == 1 ~ "SI",
    CARDIOVASCULAR == 2 ~ "NO",
    CARDIOVASCULAR == 97 ~ "No aplica",
    TRUE ~ "No se especifica"
  ))  


ggplot(data = Prom_14, aes(x = as.factor(NEUMONIA), y =  EDAD, fill = SEXO )) + 
  geom_boxplot() + ylab("Edad") + xlab("Neumonia") + ggtitle("Fallecimientos relacionados con Neumonia")
 

ggplot(data = Prom_14, aes(x = as.factor(HIPERTENSION), y =  EDAD, fill = SEXO )) + 
  geom_boxplot() + ylab("Edad") + xlab("Hipertensi?n") + ggtitle("Fallecimientos relacionados con Hipertensi?n")


ggplot(data = Prom_14, aes(x = as.factor(CARDIOVASCULAR), y =  EDAD, fill = SEXO )) + 
  geom_boxplot() + ylab("Edad") + xlab("CARDIOVASCULAR") + ggtitle("Fallecimientos relacionados con problemas cardi?cos")

# Comentario: Se observa que las enfermedades relacionadas con el fallecimiento de las personas
# est? relacionadas con la edad. Por lo tanto el sustento de que la vacunaci?n sea por bloques de 
# de edad est? sustentado. M?s a?n las personas con edad menor a 50 a?os que dieron positivo a covid
# no fallecieron, quedando los lamentables decesos como valores atipicos. 



Calculos <- Datos_columna_extra %>% filter(Categoria_Defuncion == 0)  %>%
  select( NEUMONIA, HIPERTENSION, CARDIOVASCULAR) %>%
  transform(NEUMONIA = case_when(
    NEUMONIA == 1 ~ 1,
    NEUMONIA == 2 ~ 0,
    NEUMONIA == 97 ~ 0,
    TRUE ~ 0
  )) %>% 
  transform(HIPERTENSION = case_when(
    HIPERTENSION == 1 ~ 1,
    HIPERTENSION == 2 ~ 0,
    HIPERTENSION == 97 ~ 0,
    TRUE ~ 0
  )) %>% 
  transform(CARDIOVASCULAR = case_when(
    CARDIOVASCULAR == 1 ~ 1,
    CARDIOVASCULAR == 2 ~ 0,
    CARDIOVASCULAR == 97 ~ 0,
    TRUE ~ 0
  )) %>%
  sapply(sum) %>% as.data.frame()


(Calculos <- Calculos/length(Prom_14$EDAD) *100) 


# Comentarios: Los calculos anteriores muestran que la porci?n de la presencia
# de enfermedades cardio respiraotiras en los casos donde la persona lamentablemente
# falleci?. Dando que estas personas fueron y son el sector mas susceptible a la enfermedad 
# en cuesti?n.