#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

tchoices <- c("Forma Ingreso", "Carrera", 
              "Generación", "Examen Diagnóstico", 
              "Promedio carrera", "Avance (%)", 
              "Último Inscrito", "Semestres cursados")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Alumnos"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fixedRow(
        searchInput("cuenta", "Cuenta", value = "", placeholder = "Busca clave de alumno",
                    btnSearch = NULL, btnReset = NULL, resetValue = "", width = "100%"),
        selectizeInput('forma_ingreso', 'Forma Ingreso', 
                       choices = c("PASE REGLAMENTADO", "CONCURSO FEBRERO", "SIN INFORMACIÓN",
                                   "CONCURSO JUNIO"), multiple = TRUE),
        selectizeInput('carrera', 'Carrera', 
                       choices = c("21.- Ingeniería Química", 
                                   "22.- Ingeniería Química Metalúrgica", 
                                   "23.- Química",
                                   "24.- Química de Alimentos",
                                   "25.- Química Farmacéutica Biológica"), multiple = TRUE),
        column(6,
          sliderInput("generacion", "Generación:",
                      min = 2006, max = 2019,
                      value = c(2006,2019), step = 1, sep = ""),
          sliderInput("examendiag", "Examen Diagnóstico:",
                      min = 0, max = 150,
                      value = c(0,150), step = 1),
          sliderInput("promedio", "Promedio carrera",
                      min = 0, max = 10,
                      value = c(0,10), step = 0.01)
        ),
        column(6,
          sliderInput("avance", "Avance (%)",
                      min = 0, max = 100,
                      value = c(0,100), step = 0.01),
          sliderTextInput("uinscrito", "Último Inscrito", 
                      choices = c("2006-1", "2006-2", "2007-1", "2007-1",
                                  "2008-1", "2008-2", "2009-1", "2009-2",
                                  "2010-1", "2010-2", "2011-1", "2011-2",
                                  "2012-1", "2012-2", "2013-1", "2013-2",
                                  "2014-1", "2014-2", "2015-1", "2015-2",
                                  "2016-1", "2016-2", "2017-1", "2017-2",
                                  "2018-1", "2018-2", "2019-1", "2019-2"
                                  ),
                      selected = c("2006-1", "2019-2"), grid = TRUE),
          sliderInput("semestres_cursados", "Semestres cursados",
                      min = 0, max = 20,
                      value = c(0,20), step = 1)
        )),
        wellPanel(
          div(style="display:inline-block;width:100%;text-align: center;",
            actionButton("button", "Calcular", style="color: #fff; background-color: #337ab7;")
          )
        ),
      width = 6),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica", 
                 wellPanel(
                   selectizeInput('variables_plot', 'Variables a Graficar', 
                                  choices = c("Forma Ingreso", "Carrera", 
                                              "Generación", "Examen Diagnóstico", 
                                              "Promedio carrera", "Avance (%)", 
                                              "Último Inscrito", "Semestres cursados"), 
                                  multiple = TRUE,
                                  options = list(maxItems = 3))
                 ),
                 plotOutput("myplot")
                 ),
        tabPanel("Estadística", 
                 wellPanel(
                   selectizeInput('variables_stats', 'Variables a Analizar', 
                                  choices = c("Forma Ingreso", "Carrera", 
                                              "Generación", "Examen Diagnóstico", 
                                              "Promedio carrera", "Avance (%)", 
                                              "Último Inscrito", "Semestres cursados"), 
                                  multiple = TRUE)
                 ),
                 verbatimTextOutput("statistics")
                 ), 
        tabPanel("Tabla", 
                 wellPanel(
                   selectizeInput('variables_table', 'Variables a Tabular', 
                                  choices = tchoices, 
                                  multiple = TRUE)
                 ),
                 dataTableOutput("mytable")
                 )
      ),
      width = 6
    )
  )
))
