#EJEMPLOS
#1) Leer data frame
#' library(ggplot2)
#' misdatos <- data.frame(x = rnorm(100))
#' grafica_histograma(misdatos)

grafica_histograma <- function(dataframe_plot, mytitle = "Título", xlabel = "Eje X"){
  if(!is.data.frame(dataframe_plot)){
    stop("Data frame not included")
  }
  
  myplot <- ggplot(dataframe_plot, aes(x = x)) + geom_histogram(aes(y=..density..)) +
    ggtitle(mytitle) + xlab(xlabel) + ylab("Conteo")
  return(myplot)
}

