#aun no acabo pero ahi voy
source("funciones.R")
library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
#operaciones
#limpieza
datosIniciales <-cargarDatos()
datosOrdenados <- ordenarDatos(datosIniciales)
cantObservaciones <- length(datosOrdenados$RS)
datosLimpios <- limpiarDatos(datosOrdenados)
outliers <- boxplot(datosLimpios$RS)$out
cantOutliers <- length(outliers)
datosSoutliers <- eliminaOutliers(datosLimpios,outliers)
cantFinal <- length(datosSoutliers$RS)
grafOutliers <- plot_ly(y = datos_ordenados$RS, type = "box",
               name = "Outliers en carreras anotadas") 
#análisis
#A)¿Qué tipo de variables contiene la base?
variablesD <- as.data.frame(summary(datosSoutliers))
variablesD <- variablesD %>% mutate_all(trimws)
variablesD <- select(variablesD,Var2:Freq)
vars <- variables() 
#Equipos en las ligas
eqs <- equipos(datosSoutliers) 
#Relación de equipos con victorias
evRel <- equipoVictorias(datosSoutliers)
blRel <- bateoLiga(datosSoutliers)
ovRel <- oponenteVictorias(datosSoutliers)
# Interfaz
header <- dashboardHeader(title="Análisis baseball")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Limpieza de datos",
      tabName = "limpieza",
      icon = icon("broom")
    )
  ),
  sidebarMenu(
    menuItem(
      "Análisis exploratorio",
      tabName = "exploratorio",
      icon = icon("book-open"),
      menuSubItem("Tipo de variables",
                  tabName = "variablesMenu"),
      menuSubItem("Equipos por liga",
                  tabName = "equiposMenu"),
      menuSubItem("Relaciones entre variables",
                  tabName = "relacionesMenu")
    )
  )
)

#     menuItem(
#       "Modelo",
#       tabName = "modelo",
#       icon = icon("chart-line")
#     ),
#     menuItem(
#       "Página",
#       icon = icon("user"),
#       href = "http://sigma.iimas.unam.mx/claudiajrz/dialogos.html"
#     )
#   )
# )
# 
cuerpo <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "limpieza",
      fluidRow(
        infoBox("Obs. iniciales",cantObservaciones, icon = icon("eye"), color="orange",fill=T ),
        infoBox("Obs. depuradas",cantFinal, icon = icon("check"), color="green",fill=T ),
        infoBox("Outliers",cantOutliers, icon = icon("red"), color="blue",fill=T )
      ),
      fluidRow(
        column(width = 4,
            box( width = NULL,status="success",title = "Archivo de datos",
                tags$a(href="https://drive.google.com/file/d/1REeIV7YVaqT6zsRZEZLlN74eAVNyRH8x/view?usp=sharing", "Clic para ver los datos finales"))
        ),
        column(width = 8,
               box( width = NULL,status="success",title = "Detección de outliers",
                    grafOutliers)
        )
      )
    ),
    tabItem(
      tabName = "variablesMenu",
      fluidRow(
        column(width = 6,
               selectInput("variable", 
                           "Variable:",vars,selected=vars[1])
        ),
        column(width = 6,
               box( width = NULL,title = "Variable elegida",
                    textOutput("result"))
        )
      ),
      fluidRow(
        column(width = 6,
               tableOutput('tablaVariables'),options=list(
                 searching = FALSE
               ))
      )  
    ),
    tabItem(
      tabName = "equiposMenu",
      fluidRow(
        column(width = 6,
               tableOutput('equipoTabla'))
      ) 
    ),
    tabItem(
      tabName = "relacionesMenu",
      fluidRow(
        column(width = 12,
               plotOutput('relev'))
      ),
      fluidRow(
        column(width = 12,
               plotOutput('relbl'))
      ),
      fluidRow(
        column(width = 12,
               plotOutput('relov'))
      ) 
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  cuerpo
)

server <- function(input,output){
  output$result <- renderText({paste("La variable que legiste fue: ", input$variable)})
  output$tablaVariables <- renderTable(variablesD[variablesD$Var2 %in% input$variable,])
  output$equipoTabla <- renderTable(eqs)
  output$relev <- renderPlot(evRel)
  output$relbl <- renderPlot(blRel)
  output$relov <- renderPlot(ovRel)
}

shinyApp(ui, server )