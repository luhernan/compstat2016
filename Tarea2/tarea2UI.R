# Tarea2 UI function
tarea2UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Integral con Monte Carlo"),
    
    textInput(
      inputId=ns("expresion"), 
      label="Funcion f",
      value="function(x) sqrt(4-x^2)"
    ),
    numericInput(
      inputId = ns("superior"),
      label = "Limite superior",
      value = "2"
    ),
    numericInput(
      inputId = ns("inferior"),
      label = "Limite inferior",
      value = "0"
    ),
    numericInput(
      inputId = ns("alpha"),
      label = "Nivel de Confianza % (Intervalos)",
      value = "0.05"
    ),
    plotOutput(ns("grafica")),
    p(textOutput(ns("space"))),
    tableOutput(ns("table")),
    strong("Comparacion entre Metodos"),
    p(""),
    p("Valor aproximado (Metodo Numerico): ",textOutput(ns("aproximacion"))),
    p("Valor aproximado (Monte Carlo): ",textOutput(ns("montecarlo")))
  )
}
