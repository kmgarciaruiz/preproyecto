#TODO AGREGA UN SEMESTRE AL ÚLTIMO Y AL PRIMERO
#TODO POR NÚMERO DE CUENTA ARROJAR TODA LA INFO
#TODO DISTINGUIR Y CONTAR VARIABLES CATEGÓRICAS

#clear all
rm(list = ls())

#Directorio de rodrigo
setwd("~/Dropbox/Proyecto Química/Preproyecto")

#Librerías de R
library(readxl)
library(shiny)
library(shinyWidgets)
library(stringr)
library(DT)

#Funciones nuestras
source("r/Graficas.R")
source("r/simplify_database.R")
source("r/statistical_analysis.R")

#Data Table options
options(DT.options = list(pageLength = 5, language = list(search = 'Búsqueda:')))

#Lectura de la base
mybase <- as.data.frame(read_xlsx("bases/datos_pi_2006_2014_paraKarl.xlsx"))

#Formateo del último semestre inscrito
usem_min <- min(as.numeric(substr(mybase[,"ULT_ORD"], 1, 4)))
usem_max <- max(as.numeric(substr(mybase[,"ULT_ORD"], 1, 4)))
mybase[,"ULT_ORD"] <- paste0(substr(mybase[,"ULT_ORD"], 1, 4),"-",
                             substr(mybase[,"ULT_ORD"], 5, 5))

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
ingreso_opciones   <<- unique(mybase[,"FORMA_INGRESO"])

#Opciones de análisis
analisis_opciones  <<- colnames(mybase)[-which(colnames(mybase) == "CUENTA")]

#Variables categóricas y continuas
categorical_vars   <<- c("CARRERA","GENERACION","FORMA_INGRESO","ULT_ORD")
continuous_vars    <<- c("PROMEDIO","AVANCE","PTOS_EX_DIAG","SEM_CURSADOS")

#Mínimo y máximo de generación
mingen  <- min(as.numeric(mybase[,"GENERACION"]))
maxgen  <- max(as.numeric(mybase[,"GENERACION"]))

#Mínimo y máximo de examen diagnóstico
mindiag <- min(as.numeric(mybase[,"PTOS_EX_DIAG"]))
maxdiag <- max(as.numeric(mybase[,"PTOS_EX_DIAG"]))

#Mínimo y máximo de promedio
minprom <- min(as.numeric(mybase[,"PROMEDIO"]))
maxprom <- max(as.numeric(mybase[,"PROMEDIO"]))

#Mínimo y máximo de semestres cursados
minsem  <- min(as.numeric(mybase[,"SEM_CURSADOS"]))
maxsem  <- max(as.numeric(mybase[,"SEM_CURSADOS"]))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Alumnos"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fixedRow(
        searchInput("cuenta", "Cuenta", value = "", placeholder = "NO SIRVE",
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
        )),
        # wellPanel(
        #   div(style="display:inline-block;width:100%;text-align: center;",
        #     actionButton("button", "Calcular", style="color: #fff; background-color: #337ab7;")
        #   )
        # ),
      width = 4),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica", 
                 wellPanel(
                   selectizeInput('variables_plot', 'Variables a Graficar', 
                                  choices = analisis_opciones, 
                                  multiple = TRUE,
                                  options = list(maxItems = 3))
                 ),
                 plotOutput("myplot")
                 ),
        tabPanel("Estadística", 
                 wellPanel(
                   selectizeInput('variables_stats', 'Variables a Analizar', 
                                  choices = analisis_opciones, 
                                  multiple = TRUE,
                                  options = list(maxItems = 2))
                 ),
                 tableOutput("statistics")
                 ), 
        tabPanel("Tabla", 
                 wellPanel(
                   selectizeInput('variables_table', 'Variables a Tabular', 
                                  choices = c(analisis_opciones, "CUENTA"), 
                                  multiple = TRUE)
                 ),
                 DT::DTOutput("mytable")
                 )
      ),
      width = 8
    )
  )
))
