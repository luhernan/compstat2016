source("Tarea1/tarea1UI.R")
source("Tarea1/tarea1.R")
source("Ejemplo/ejemploUI.R")
source("Ejemplo/ejemplo.R")
source("Tarea2/tarea2UI.R")
source("Tarea2/tarea2.R")
source("Tarea3/tarea3UI.R")
source("Tarea3/tarea3.R")

library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    
    titlePanel("Estadistica Computacional 2016"),
    
    # Sidebar
    sidebarLayout(sidebarPanel(
      radioButtons(
        "tarea",
        label = "Escoge una tarea",
        choices = c(
          "Funcion Inversa" = "funInv",
          #"Aceptacion Rechazo" = "aceptacionRechazo",
          "Integral con Monte Carlo" = "integralMC",
          "Metropolis-Hastings" = "markovChain"
        ),
        selected = "funInv"
      )
    ),
    
    
    #Panel principal
    mainPanel(
      conditionalPanel(
        condition = "input.tarea=='funInv'", tarea1UI("tarea1")),
      #conditionalPanel(
      #  condition = "input.tarea=='aceptacionRechazo'", ejemploUI("ejemplo")),
      conditionalPanel(
        condition = "input.tarea=='integralMC'", tarea2UI("tarea2")),
      conditionalPanel(
        condition = "input.tarea=='markovChain'", tarea3UI("tarea3"))
    )
    )
  ))
    
    