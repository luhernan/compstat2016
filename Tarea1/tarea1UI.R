# Tarea1 UI function
tarea1UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Metodo de la Funcion Inversa"),
    
    numericInput(
      inputId = ns("lambda"),
      label = "Valor de Lambda",
      value = "3.0"
    ),
    numericInput(
      inputId = ns("simulaciones"),
      label = "# de Simulaciones",
      value = "1000"
    ),
    numericInput(
      inputId = ns("bins"),
      label = "Numero de Bins",
      value = "20"
    ),
    actionButton(ns("btn"), "Verificar"),
    p(textOutput(ns("res1"))),
    p(textOutput(ns("res2"))),
    htmlOutput(ns("summary")),
    plotOutput(ns("histograma")),
    downloadLink(ns("downloadData"), "Descargar Simulacion (.CSV)"),
    p(""),
    strong("Vector de Simulaciones"),
    p(""),
    textOutput(ns("simulaciones"))
  )
}
