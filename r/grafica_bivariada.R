#EJEMPLOS
#1) Leer data frame
#' library(ggplot2)
#' misdatos <- data.frame(x = rnorm(100))
#' grafica_histograma(misdatos)

grafica_bivariada <- function(mydata, variables_plot, mytitle = "Histograma",
                               categorical_vars = c("CARRERA","GENERACION","FORMA_INGRESO","ULT_ORD"),
                               continuous_vars  = c("PROMEDIO","AVANCE","PTOS_EX_DIAG","SEM_CURSADOS")){
  if(!is.data.frame(mydata)){
    stop("Data frame not included")
  }
  
  #Assign first continuous, second categorical
  if (variables_plot[2] %in% continuous_vars & variables_plot[1] %in% categorical_vars){
    aux <- variables_plot; variables_plot[1] <- aux[2]; variables_plot[2] <- aux[1];
  } 
  
  #Continua con categórica
  if(variables_plot[1] %in% continuous_vars & variables_plot[2] %in% categorical_vars){
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot[1]])) + 
      geom_density(aes(colour = mydata[,variables_plot[2]]), size = 1) + ylab("Frecuencia") + 
      labs(color=variables_plot[2]) 
    
  #Categórica con categórica  
  } else if(variables_plot[1] %in% categorical_vars & variables_plot[2] %in% categorical_vars){
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot[1]])) + 
      geom_bar(aes(y=..count.., fill = mydata[,variables_plot[2]]), color = "black", position = "dodge") + 
      ylab("Frecuencia") + labs(fill=variables_plot[2])  
    
  #Continua con continua  
  } else {
    myplot <- ggplot(mydata, aes(x = mydata[,variables_plot[1]], y = mydata[,variables_plot[2]])) + 
      geom_point(color = "deepskyblue4", alpha = 0.3) +
      geom_smooth(color = "tomato3", size = 2) + ylab(variables_plot[2]) 
  }
  
  #Add transparent background
  myplot <-  myplot + theme_classic() + ggtitle(mytitle) + xlab(variables_plot[1]) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
          panel.background = element_blank(), plot.background = element_blank(),
          legend.background = element_blank())
  
  return(myplot)
}

