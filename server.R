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
  
  output$myplot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    hist(mybase$PROMEDIO, col = 'darkgray', border = 'white')
    
  })
  
  output$statistics <- renderText({
    
    generacion <<- input$generacion
    examendiag <<- input$examendiag
    avance     <<- input$avance
    uinscrito  <<- input$uinscrito
    promedio   <<- input$promedio
    semestres_cursados <<- input$semestres_cursados
    forma_ingreso <<- input$forma_ingreso
    carrera <<- input$carrera
    cuenta <<- input$cuenta
    # 
    # nuevabase <- simplify_database(mybase, generacion, examendiag,
    #                   promedio, avance, uinscrito,
    #                   semestres_cursados, forma_ingreso,
    #                   carrera, cuenta)
    # 
    nuevabase <- simplify_database(mybase, input$generacion, input$examendiag,
                                   input$promedio, input$avance, input$uinscrito,
                                   input$semestres_cursados, input$forma_ingreso,
                                   input$carrera, cuenta)
    
    
    if (nrow(nuevabase) > 0){
      mysummary <- summary(nuevabase[,"AVANCE"])
      desvest   <- sd(nuevabase[,"AVANCE"])
      mysummary <- c(mysummary, desvest)
      names(mysummary) <- c("Min      ", "25%      ", "Mediana  ", "Media    ", 
                            "75%      ", "Máx      ", "Desv.Est.")
      mytext <- ""
      for (i in 1:length(mysummary)){
        mytext <- paste(mytext,"\n",names(mysummary)[i], "=", mysummary[i])
      }
    } else {
      mytext <- "NO HAY OBSERVACIONES QUE CUMPLAN ESOS PARÁMETROS"
    }
    print(mytext)
  })
  
  output$mytable <- renderDataTable({
    data.frame(x = rnorm(10), y = rnorm(10))
    })
  
})
