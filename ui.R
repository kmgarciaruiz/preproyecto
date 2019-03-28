#clear all
rm(list = ls())

#Librerías de R
library(readxl)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(DT)
library(ggplot2)
#library(plotly)

#Funciones nuestras
source("r/grafica_histograma.R")
source("r/simplify_database.R")
source("r/statistical_analysis.R")
source("r/grafica_bivariada.R")

#Data Table options
options(DT.options = list(pageLength = 5, language = list(search = 'Búsqueda:')))

#Lectura de la base
mybase <- as.data.frame(read_xlsx("bases/datos_pi_2006_2014_paraKarl.xlsx"))

#Change database colnames
colnames(mybase)[which(colnames(mybase) == "FORMA_INGRESO")] <- "FORMA_DE_INGRESO" 
colnames(mybase)[which(colnames(mybase) == "PTOS_EX_DIAG")]  <- "EXAMEN_DE_DIAGNÓSTICO"
colnames(mybase)[which(colnames(mybase) == "ULT_ORD")]       <- "ÚLTIMO_SEMESTRE_ORDINARIO"
colnames(mybase)[which(colnames(mybase) == "SEM_CURSADOS")]  <- "SEMESTRES_CURSADOS"

#Formateo del último semestre inscrito
usem_min <- min(as.numeric(substr(mybase[,"ÚLTIMO_SEMESTRE_ORDINARIO"], 1, 4)))
usem_max <- max(as.numeric(substr(mybase[,"ÚLTIMO_SEMESTRE_ORDINARIO"], 1, 4)))
mybase[,"ÚLTIMO_SEMESTRE_ORDINARIO"] <- paste0(substr(mybase[,"ÚLTIMO_SEMESTRE_ORDINARIO"], 1, 4),"-",
                             substr(mybase[,"ÚLTIMO_SEMESTRE_ORDINARIO"], 5, 5))

#LIMPIEZA DE LA BASE
mybase[which(mybase[,"CARRERA"] == 21),"CARRERA"] <- "21.- Ing. Química"
mybase[which(mybase[,"CARRERA"] == 22),"CARRERA"] <- "22.- Ing. Química Metalúrgica"
mybase[which(mybase[,"CARRERA"] == 23),"CARRERA"] <- "23.- Química"
mybase[which(mybase[,"CARRERA"] == 24),"CARRERA"] <- "24.- Química Farmacéutico Biológica"
mybase[which(mybase[,"CARRERA"] == 28),"CARRERA"] <- "28.- Química en Alimentos"

#CAMBIO DE VARIABLE
mybase[which(mybase[,"FORMA_DE_INGRESO"] == 51) ,"FORMA_DE_INGRESO"] <- "ARTÍCULO 51"

#Base como global
mybase <<- mybase

#Opciones de semestre
semestres_opciones <- c()
for (i in usem_min:usem_max){
  semestres_opciones <- c(semestres_opciones, paste0(i, "-", c(1,2)))  
}
semestres_opciones <<- semestres_opciones

#Opciones de carrera 
carrera_opciones   <<- unique(mybase[,"CARRERA"])

#Opciones de ingreso
ingreso_opciones   <<- unique(mybase[,"FORMA_DE_INGRESO"])

#Opciones de análisis
analisis_opciones  <<- colnames(mybase)[-which(colnames(mybase) == "CUENTA")]

#Variables categóricas y continuas
categorical_vars   <<- c("CARRERA","GENERACION","FORMA_DE_INGRESO","ÚLTIMO_SEMESTRE_ORDINARIO")
continuous_vars    <<- c("PROMEDIO","AVANCE","EXAMEN_DE_DIAGNÓSTICO","SEMESTRES_CURSADOS")

#Mínimo y máximo de generación
mingen  <- min(as.numeric(mybase[,"GENERACION"]))
maxgen  <- max(as.numeric(mybase[,"GENERACION"]))

#Mínimo y máximo de examen diagnóstico
mindiag <- min(as.numeric(mybase[,"EXAMEN_DE_DIAGNÓSTICO"]))
maxdiag <- max(as.numeric(mybase[,"EXAMEN_DE_DIAGNÓSTICO"]))

#Mínimo y máximo de promedio
minprom <- min(as.numeric(mybase[,"PROMEDIO"]))
maxprom <- max(as.numeric(mybase[,"PROMEDIO"]))

#Mínimo y máximo de semestres cursados
minsem  <- min(as.numeric(mybase[,"SEMESTRES_CURSADOS"]))
maxsem  <- max(as.numeric(mybase[,"SEMESTRES_CURSADOS"]))


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("simplex"),
  
  titlePanel("INFORMACIÓN DEL ALUMNADO"),
  
  div("", style="background-color: black; margin-bottom: 1%; border-radius: 25px; min-height: 5px; width: 100%;"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fixedRow(
        searchInput("cuenta", "Cuenta", value = "", placeholder = "Indique número de cuenta",
                    btnSearch = NULL, btnReset = "Reset", resetValue = "", width = "100%"),
        selectizeInput('forma_ingreso', 'Forma Ingreso', 
                       choices = ingreso_opciones, multiple = TRUE),
        selectizeInput('carrera', 'Carrera', 
                       choices = carrera_opciones, multiple = TRUE),
        column(6,
          sliderInput("generacion", "Generación:",
                      min = mingen, 
                      max = maxgen,
                      value = c(mingen, maxgen), step = 1, sep = ""),
          sliderInput("examendiag", "Examen Diagnóstico:",
                      min = mindiag, 
                      max = maxdiag,
                      value = c(mindiag, maxdiag), step = 1),
          sliderInput("promedio", "Promedio carrera",
                      min = minprom, 
                      max = maxprom,
                      value = c(minprom, maxprom), step = 0.01)
        ),
        column(6,
          sliderInput("avance", "Avance (%)",
                      min = 0, max = 100,
                      value = c(0,100), step = 0.01),
          sliderTextInput("uinscrito", "Último Inscrito", 
                      choices = semestres_opciones,
                      selected = c(semestres_opciones[1], 
                                   semestres_opciones[length(semestres_opciones)]), grid = TRUE),
          sliderInput("semestres_cursados", "Semestres cursados",
                      min = minsem, max = maxsem,
                      value = c(minsem, maxsem), step = 1)
        )), width = 4),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", 
                 wellPanel(
                   fluidRow(
                     column(1, 
                            downloadButton('downloadTable', label = "")
                     ),
                     column(11, 
                            selectizeInput('variables_table', 'Variables a Tabular', 
                                           choices = c(analisis_opciones, "CUENTA"), 
                                           multiple = TRUE)
                     )
                   )
                 ),
                 DT::DTOutput("mytable")
        ),
        tabPanel("Gráfica", 
                 wellPanel(
                   fluidRow(
                     column(1, 
                      downloadButton('downloadPlot', label = "")
                     ),
                     column(11,
                            selectizeInput('variables_plot', 'Variables a Graficar', 
                                           choices = analisis_opciones, 
                                           multiple = TRUE,
                                           options = list(maxItems = 2))
                     )
                   )
                 ),
                 plotOutput("myplot")
                 ),
        tabPanel("Estadística", 
                 wellPanel(
                   fluidRow(
                     column(1, 
                       downloadButton('downloadStats', label = "")
                      ),
                     column(11, 
                       selectizeInput('variables_stats', 'Variables a Analizar', 
                                      choices = analisis_opciones, 
                                      multiple = TRUE,
                                      options = list(maxItems = 2))
                     )
                   )
                 ),
                 tableOutput("statistics")
                 )
      ),
      width = 8
    )
  ),
  div("", style="background-color: black; margin-bottom: 1%; border-radius: 25px; min-height: 5px; width: 100%;")
))
