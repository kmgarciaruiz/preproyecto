#EJEMPLOS
#1) Leer data frame
#' library(ggplot2)
#' misdatos <- data.frame(x = rnorm(100))
#' grafica_histograma(misdatos)

grafica_histograma <- function(mydata, variables_plot, mytitle = "Histograma", xlabel = "Eje X",
                               categorical_vars = c("CARRERA","GENERACION","FORMA_INGRESO","ULT_ORD"),
                               continuous_vars  = c("PROMEDIO","AVANCE","PTOS_EX_DIAG","SEM_CURSADOS")){
  if(!is.data.frame(mydata)){
    stop("Data frame not included")
  }
  
  if(variables_plot %in% continuous_vars){
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot])) + 
      geom_histogram(aes(y=..density..), fill = "deepskyblue3", color = "deepskyblue4") +
      geom_density(size = 1) +
      ggtitle(mytitle) + xlab(xlabel) + ylab("Frecuencia") + theme_bw()
  } else {
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot])) + 
      geom_bar(aes(y=..count../sum(..count..)), fill = "deepskyblue3", color = "deepskyblue4") +
      #geom_line(stat='count') +
      ggtitle(mytitle) + xlab(xlabel) + ylab("Frecuencia") + theme_bw()
  }
  return(myplot)
}

