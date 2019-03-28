
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    generacion <<- input$generacion
    examendiag <<- input$examendiag
    promedio   <<- input$promedio
    avance     <<- input$avance
    uinscrito  <<- input$uinscrito
    semestres  <<- input$semestres_cursados
    ingreso    <<- input$forma_ingreso
    carrera    <<- input$carrera
    cuenta     <<- input$cuenta

    
    nuevabase <- simplify_database(mybase, generacion, examendiag,
                                   promedio, avance, uinscrito,
                                   semestres, ingreso,
                                   carrera, cuenta)    
    
    nuevabase <- simplify_database(mybase, input$generacion, input$examendiag,
                                   input$promedio, input$avance, input$uinscrito,
                                   input$semestres_cursados, input$forma_ingreso,
                                   input$carrera, input$cuenta)
    
    return(nuevabase)
    
  })
  
  getStats <- reactive({
    
    mytablestats <- statistical_analysis(dataInput(), input$variables_stats, categorical_vars, continuous_vars)
    
    return(mytablestats)
    
  })
  
  createPlot <- reactive({
    
    # CASO VACÍO (PLACEHOLDER)
    if(length(input$variables_plot) == 0){
      
      myplot <- ggplot(data.frame()) + geom_point() + xlim(-5, 5) + ylim(-5, 5) + 
        annotate("text", x = 0, y = 0,label = "AGREGUE VARIABLES A GRAFICAR") +
        theme_void()
      
    #CASO UNA VARIABLE SELECCIONADA
    } else if (length(input$variables_plot) == 1) {
      myplot <- grafica_histograma(dataInput(), input$variables_plot, paste("ANÁLISIS DE", input$variables_plot), xlabel = input$variables_plot,
                                   categorical_vars = categorical_vars, continuous_vars = continuous_vars)
      
    #CASO DOS VARIABLES SELECCIONADAS    
    } else if (length(input$variables_plot) == 2) {
      myplot <- grafica_bivariada(dataInput(), input$variables_plot, paste("ANÁLISIS DE", input$variables_plot[1], "Y", input$variables_plot[2]),
                                  categorical_vars = categorical_vars, continuous_vars = continuous_vars)
    }
    
    return(myplot)
    
  })
  
  output$myplot <- renderPlot({
    
    createPlot()
    
  }, bg="transparent")
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste('Grafica-',Sys.Date(),'.pdf', sep='', collapse = "") },
    content = function(file) {
      ggsave(file, plot = createPlot(), device = "pdf", width = 10, height = 6)
    }
  )
  
  output$downloadStats <- downloadHandler(
    filename = function() { paste('Estadistica-', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(getStats(),file)
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() { paste('Tabla-', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(dataInput(),file)
    }
  )
  
  output$statistics <- renderTable({
    return(getStats())
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
