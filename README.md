# preproyecto
Para correr, copia y pega el siguiente código en la consola de R

```
#CÓDIGO PARA CORRERLO
flag <- rep(FALSE, 6)
n    <- 1
while (n < 10 & any(flag == FALSE)){
  if (!require(readxl)){
    install.packages("readxl")
  }
  
  if (!require(shiny)){
    install.packages("readxl")
  }
  
  if (!require(shinyWidgets)){
    install.packages("readxl")
  }
  
  if (!require(stringr)){
    install.packages("readxl")
  }
  
  if (!require(DT)){
    install.packages("readxl")
  }
  
  if (!require(ggplot2)){
    install.packages("readxl")
  }
}

shiny::runGitHub( "preproyecto", "kmgarciaruiz") 
```  
