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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$myplot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    hist(rnorm(100), col = 'darkgray', border = 'white')
    
  })
  
  output$statistics <- renderText({
    mysummary <- summary(rnorm(100))
    desvest   <- sd(rnorm(100))
    mysummary <- c(mysummary, desvest)
    names(mysummary) <- c("Min      ", "25%      ", "Mediana  ", "Media    ", 
                          "75%      ", "Máx      ", "Desv.Est.")
    mytext <- ""
    for (i in 1:length(mysummary)){
      mytext <- paste(mytext,"\n",names(mysummary)[i], "=", mysummary[i])
    }
    print(mytext)
  })
  
  output$mytable <- renderDataTable({
    data.frame(x = rnorm(10), y = rnorm(10))
    })
  
})
