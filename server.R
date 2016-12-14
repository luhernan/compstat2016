library(shiny)

function(input, output, session) {
  #El identificador con el que manda llamar al modulo debe ser el mismo que en el conditionalPanel de la UI general
  callModule(ejemplo, "ejemplo")
  callModule(tarea1, "tarea1")
  callModule(tarea2, "tarea2")
  callModule(tarea3, "tarea3")
}


