cargarDatos <- function(){
  datos=read.csv2("baseball_raw.csv", header=T)
  datos
}

ordenarDatos <- function(datos){
datos_ordenados<-datos%>% 
  pivot_longer(X1:X1232,
               names_to="Observaciones",
               values_to="Valor") %>% 
  pivot_wider(names_from=X, values_from=Valor)%>%
  select(Observaciones:OSLG)
  datos_ordenados
}

limpiarDatos <- function(datos){
  datos_ordenados <- datos %>% 
  select(!RankSeason & !RankPlayoffs) %>% #Eliminación de RankSeason y RankPlayoffs
  drop_na() #Eliminación de valores faltantes
  #Ajustando los tipos de datos
  datos_ordenados$RS <- as.integer(datos_ordenados$RS)
  datos_ordenados$RA <- as.integer(datos_ordenados$RA)
  datos_ordenados$W <- as.integer(datos_ordenados$W)
  datos_ordenados$Playoffs <- as.integer(datos_ordenados$Playoffs)
  datos_ordenados$Playoffs <- as.logical(datos_ordenados$Playoffs)
  datos_ordenados$G <- as.integer(datos_ordenados$G)
  datos_ordenados$OBP <- as.double(datos_ordenados$OBP)
  datos_ordenados$SLG <- as.double(datos_ordenados$SLG)
  datos_ordenados$BA <- as.double(datos_ordenados$BA)
  datos_ordenados$OOBP <- as.double(datos_ordenados$OOBP)
  datos_ordenados$OSLG <- as.double(datos_ordenados$OSLG)
  datos_ordenados <- na.omit(datos_ordenados)
  datos_ordenados
}

eliminaOutliers <- function(datos,outliers){
  datoso <- filter(datos, !RS %in% c(outliers))
  datoso
}

variables <- function(){
  vars <- c("Número de observaciones" = "Observaciones",
    "Equipo" = "Team",
            "Liga" = "League",
  "Año" = "Year",
  "Carreras anotadas" = "RS",
  "Ejecuciones permitidas" = "RA",
  "Victorias" = "W",
  "Porcentaje en base" = "OBP",
  "Porcentaje de slugging" = "SLG",
  "Promedio de bateo" = "BA",
  "Playoffs" = "Playoffs",
  "Juegos jugados" = "G",
  "Porcentaje base del oponente" = "OOBP",
  "Porcentaje de slugging del oponente" = "OSLG")
  vars
}

equipos <- function(datos1){
  eq <- datos1 %>% group_by(League) %>% count(Team)%>%
    mutate(id=row_number(),.before=League)%>%
    summarise(equiposXliga=length(id))
  eq
}

equipoVictorias <- function(datos1){
  graf1 <- ggplot(datos1, aes(x=Team, y=W))+
    geom_point(color="darkblue")+
    labs(title="Relación entre el equipo y numero de victorias por año",
         x="Equipo",
         y="Número de victorias por año")
  graf1
}

bateoLiga <- function(datos1){
  graf1<- ggplot(datos1, aes(x=BA))+
    geom_histogram(aes(fill=League), alpha=0.5)+
    labs(title="Distribución del promedio de bateo cambia de acuerdo a la liga",
         x="Promedio de bateo",
         fill = "Liga")+
    scale_fill_brewer(palette = "Dark2")
}

oponenteVictorias <- function(datos1){
  graf <- ggplot(datos1, aes(x=OOBP, y=W))+
    geom_point(color="darkred")+
    labs(title="Relación entre el porcentaje en base del oponente y las victorias",
         x="Porcentaje en base del oponenete",
         y="Victorias")
}
