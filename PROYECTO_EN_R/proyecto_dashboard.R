#aun no acabo pero ahi voy
source("funciones.R")
library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
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
variablesD <- summary(datosSoutliers)
vars <- variables() 
View(vars)
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
      icon = icon("book-open")
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
      tabName = "exploratorio",
      fluidRow(
        column(width = 4, tableOutput('variables'))
        )
      ),
      fluidRow(
        column(width = 12, selectInput("variable", "Variable:",))
    )
  )
    )
)
#     ,tabItem(
#       tabName = "modelo",
#       fluidRow(
#         column(width = 6, graf1),
#         column(width = 6, graf2)
#       ),
#       fluidRow(
#         column(width = 6, 
#                box( width = NULL,
#                     status="warning",
#                     title = "Modelo simple",
#                     graf1)
#         ),
#         column(width = 6,
#                box( width = NULL,
#                     status="success",
#                     title = "Modelo múltiple",
#                     graf2)
#         )
#       )
# 
#     )
#   )
# )
# 
# 
# 
ui <- dashboardPage(
  header,
  sidebar,
  cuerpo
)

server <- function(input,output){
  output$variables <- renderTable(variables)
}

shinyApp(ui, server )