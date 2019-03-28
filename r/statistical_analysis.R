#Selección de las variables sobre las cuales hacer estadística
statistical_analysis <- function(mydata, variables_stats,
                                 categorical_vars = c("CARRERA","GENERACION","FORMA_DE_INGRESO","ÚLTIMO_SEMESTRE_ORDINARIO"),
                                 continuous_vars  = c("PROMEDIO","AVANCE","EXAMEN_DE_DIAGNÓSTICO","SEMESTRES_CURSADOS")
                                 ){
  
  #Checamos que los datos contengan la información solicitada
  if (length(variables_stats) == 0) {
    mysummary <- data.frame(x = "SELECCIONA VARIABLES A ANALIZAR")
  } else if (nrow(mydata) == 0) {
    mysummary <- data.frame(x = "NO HAY OBSERVACIONES QUE CUMPLAN ESOS PARÁMETROS.")
  } else if (!all(variables_stats %in% colnames(mydata))){
    mysummary <- data.frame(x = "ERROR: VARIABLES NO HAYADAS EN DATOS.")

  #En caso afirmativo, analizamos si será tabla comparativa o no
  } else {
    
    #Names for summary table if continuous
    snames <- c("Min", "25%", "Mediana", "Media",  "75%", "Máx", "Desv Est")
    
    #Si sólo nos dieron una base hacemos un summary
    if (length(variables_stats) == 1){
      
      #Checamos si es continua
      if (variables_stats %in% continuous_vars){
        mysummary <- summary(mydata[,variables_stats[1]])
        desvest   <- sd(mydata[,variables_stats[1]])
        mysummary <- data.frame(matrix(c(mysummary, desvest), ncol = 7))
        colnames(mysummary) <- snames
      } else {
        mysummary <- table(mydata[,variables_stats[1]])
        mysummary <- data.frame(mysummary)
        colnames(mysummary)[1] <- variables_stats[1]
        colnames(mysummary)[2] <- "OBSERVACIONES"
      }
    } else if (length(variables_stats) == 2){
      
      #Assign first continuous, second categorical
      if (variables_stats[2] %in% continuous_vars & variables_stats[1] %in% categorical_vars){
        aux <- variables_stats; variables_stats[1] <- aux[2]; variables_stats[2] <- aux[1];
      } 
      
      #Check if continuous-categorical combination
      if (variables_stats[1] %in% continuous_vars && variables_stats[2] %in% categorical_vars){
        mysummary <- by(mydata[,variables_stats[1]], mydata[,variables_stats[2]], summary)
        desvest   <- by(mydata[,variables_stats[1]], mydata[,variables_stats[2]], sd)
        mysummary <- data.frame(cbind(do.call(rbind, mysummary), cbind(desvest)))
        mysummary <- cbind(rownames(mysummary), mysummary)
        colnames(mysummary) <- c(variables_stats[2], snames)
      } else if (variables_stats[1] %in% categorical_vars & variables_stats[2] %in% categorical_vars){
        mysummary <- table(mydata[,variables_stats[1]], mydata[,variables_stats[2]])
        mysummary <- data.frame(mysummary)
        colnames(mysummary)[1] <- variables_stats[1]
        colnames(mysummary)[2] <- variables_stats[2]
        colnames(mysummary)[3] <- "OBSERVACIONES"
      } else {
        mysummary <- data.frame(CORRELACION = cor(mydata[,variables_stats[1]], mydata[,variables_stats[2]]))
      }
    }
    
    #Agregar nombres
    
    
  }
  
  return(mysummary)
}