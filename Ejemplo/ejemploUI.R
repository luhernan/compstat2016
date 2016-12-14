# NOTA: Se tiene que añadir 'ns' a todos los identificadores de input y outputs
ejemploUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Aceptacion-Rechazo"),
    
    textInput(
      inputId=ns("expresion1"), 
      label="Funcion f",
      value="function(x) 2*x"
    ),
    selectInput(
      inputId=ns("expresion2"), 
      label="Funcion g",
      choices=c("Uniforme(xmin, xmax)"="unif", "Exponencial(1) truncada a (xmin,xmax)"="exp", "Normal(0,1) truncada a (xmin,xmax)"="norm")
    ),
    sliderInput(ns("xmin"), "xmin1", min=-30, max=30, value=0),
    sliderInput(ns("xmax"), "xmax1", min=-30, max=20, value=1),
    sliderInput(ns("M"), "M1", min=0.1, max=100, value=1),
    numericInput(ns("nsim"), "Número de Simulaciones", value=100),
    #Este boton no tiene funcionalidad, habria que quitarlo
    #actionButton(ns("button1"), "Correr"),
    plotOutput(ns("Grafica")),
    h3("Resultados"),
    p("Tasa de Exito: ", textOutput(ns("tasa_exito"))),
    plotOutput(ns("hist_sim")),
    sliderInput(ns("nbins"), "nbins", value=20, min=10, max=100)
  )
  
}