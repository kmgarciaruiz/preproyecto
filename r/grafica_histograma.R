#EJEMPLOS
#1) Leer data frame
#' library(ggplot2)
#' misdatos <- data.frame(x = rnorm(100))
#' grafica_histograma(misdatos)

grafica_histograma <- function(mydata, variables_plot, mytitle = "Análisis", xlabel = "Eje X",
                               categorical_vars = c("CARRERA","GENERACION","FORMA_DE_INGRESO","ULT_ORD"),
                               continuous_vars  = c("PROMEDIO","AVANCE","EXAMEN_DE_DIAGNÓSTICO","SEMESTRES_CURSADOS")){
  if(!is.data.frame(mydata)){
    stop("Data frame not included")
  }
  
  #Remove underscores from title
  mytitle <- gsub("_",' ', mytitle)
  xlabel  <- gsub("_",' ', xlabel)
  
  if(variables_plot %in% continuous_vars){
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot])) + 
      geom_histogram(aes(y=..density..), fill = "deepskyblue4", color = "black") +
      geom_density(size = 1.5, color = "tomato3") 
  } else {
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot])) + 
      geom_bar(aes(y=..count../sum(..count..), fill = mydata[,variables_plot]), color = "black") +
      labs(fill= gsub("_",' ',variables_plot))
  }
  
  myplot <- myplot + ggtitle(mytitle) + xlab(xlabel) + ylab("FRECUENCIA") + theme_classic() +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          legend.background = element_blank())
  
  return(myplot)
}

