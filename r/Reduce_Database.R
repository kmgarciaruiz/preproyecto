
simplify_database <- function(mydata,
                              generacion = c(2006, 2019), 
                              examendiag = c(0, 150), 
                              promedio = c(0, 10), 
                              avance = c(0, 100), 
                              uinscrito = semestres_opciones,
                              semestres_cursados = c(0,20), 
                              forma_ingreso = NULL, 
                              carrera = NULL, cuenta = ""){
  
  
  #Check format for cuenta
  cuenta <- str_replace_all(cuenta, pattern=" ", repl="")
  
  #Get input names
  inames <- colnames(mydata)
  
  #Check if empty
  if (is.null(forma_ingreso)){
    forma_ingreso <- ingreso_opciones 
  }
  
  if (is.null(carrera)){
    carrera <- carrera_opciones 
  }
  
  #Crear un corrido de uinscrito
  if (uinscrito[1] != uinscrito[2]){
    anio <- as.numeric(substr(uinscrito,1,4))
    sems <- as.numeric(substr(uinscrito,6,6))
    uinscrito <- c()
    for (i in anio[1]:anio[2]){
      if (i == anio[1] & sems[1] == 2){
        uinscrito <- c(uinscrito, paste0(i, "-", 2))  
      } else if (i == anio[2] & sems[2] == 1){
        uinscrito <- c(uinscrito, paste0(i, "-", 1))  
      } else {
        uinscrito <- c(uinscrito, paste0(i, "-", c(1,2)))  
      }
    }
  }
  
  #Aquí falta un if que cheque la base es correcta
  
  #Por otro lado, si no nos dieron cuenta buscamos por los otros factores
  params       <- which(mydata[,"GENERACION"] >= generacion[1] & 
                        mydata[,"GENERACION"] <= generacion[2] & 
                        mydata[,"PTOS_EX_DIAG"] >= examendiag[1] & 
                        mydata[,"PTOS_EX_DIAG"] <= examendiag[2] &
                        mydata[,"PROMEDIO"] >= promedio[1]   & 
                        mydata[,"PROMEDIO"] <= promedio[2] & 
                        mydata[,"AVANCE"] >= avance[1] & 
                        mydata[,"AVANCE"] <= avance[2] &
                        mydata[,"SEM_CURSADOS"] >= semestres_cursados[1] & 
                        mydata[,"SEM_CURSADOS"] <= semestres_cursados[2] & 
                        mydata[,"ULT_ORD"] %in% uinscrito & 
                        mydata[,"FORMA_INGRESO"] %in% forma_ingreso &
                        mydata[,"CARRERA"] %in% carrera_opciones)
  

  
  #Reducción de la base
  if (length(params) > 0){
    mydata <- mydata[params,]  
  } else {
    mydata           <- data.frame(matrix(NA, ncol = ncol(mydata), nrow = 0))
    colnames(mydata) <- inames
  }
  
  
  #Si nos dieron cuenta buscamos sólo por cuenta
  if (cuenta != ""){
    localiza   <- which(mydata[,"CUENTA"] == cuenta)
    if (length(localiza) > 0){
      mydata <- mydata[localiza, ]
    } else {
      mydata           <- data.frame(matrix(NA, ncol = ncol(mydata), nrow = 0))
      colnames(mydata) <- inames
    }
  }
  
  return(mydata)
}
