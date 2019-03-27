#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  dataInput <- reactive({
    # variables_tabular <<- input$variables_table
    # generacion <<- input$generacion
    # examendiag <<- input$examendiag
    # avance     <<- input$avance
    # uinscrito  <<- input$uinscrito
    # promedio   <<- input$promedio
    # semestres_cursados <<- input$semestres_cursados
    # forma_ingreso <<- input$forma_ingreso
    # carrera <<- input$carrera
    # cuenta <<- input$cuenta
    # variables_stats <<- input$variables_stats
    # 
    # nuevabase <- simplify_database(mybase, generacion, examendiag,
    #                                promedio, avance, uinscrito,
    #                                semestres_cursados, forma_ingreso,
    #                                carrera, cuenta)  
    
    nuevabase <- simplify_database(mybase, input$generacion, input$examendiag,
                                   input$promedio, input$avance, input$uinscrito,
                                   input$semestres_cursados, input$forma_ingreso,
                                   input$carrera, input$cuenta)
    
    return(nuevabase)
    
  })
  
  output$myplot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    hist(mybase$PROMEDIO, col = 'darkgray', border = 'white')
    
  })
  
  output$statistics <- renderTable({
    #mytext <- statistical_analysis(nuevabase, variables_stats)
    mytablestats <- statistical_analysis(dataInput(), input$variables_stats, categorical_vars, continuous_vars)
    return(mytablestats)
  })
  
  output$mytable <- DT::renderDataTable({
    tabulado <- dataInput()
    if (!is.null(input$variables_table) & all(input$variables_table %in% colnames(tabulado))){
      if (length(input$variables_table) == 1){
        tabulado <- as.data.frame(tabulado[,input$variables_table])
        colnames(tabulado) <- input$variables_table
      } else {
        tabulado <- tabulado[,input$variables_table]  
      }
    } 
    
    return(tabulado)
  }, options = list(scrollX = TRUE))
  
})
