# Tarea3 UI function

tarea3UI <- function(id, label = "CSV file") {

  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Tiene Encabezados?"),
    selectInput(ns("dependiente"), "Carga un CSV primero",""),
    selectInput(ns("independiente"), "Carga un CSV primero",""),
    #checkboxGroupInput(ns("inCheckboxGroup"),"Carga un CSV primero",""),
    strong("Base de Datos"),
    dataTableOutput(ns("table")),
    strong("Grafica de Dispersion"),
    plotOutput(ns("plot")),
    strong("Graficas (Funciones de Densidad)"),
    plotOutput(ns("aprioria")),
    plotOutput(ns("apriorib")),
    plotOutput(ns("apriorig")),
    numericInput(
      inputId = ns("edo_inicial"),
      label = "Longitud de las Cadenas",
      value = 2,
      min = 1,
      max = 1
    ),
    numericInput(
      inputId = ns("simulaciones"),
      label = "Numero de Cadenas",
      value = "1000",
      min = 1
    )#,
    #p(strong("Trayectoria"), textOutput(ns("traj")))
    
    #dataTableOutput(ns("traj"))
    
    
    #tableOutput(ns("table")),
    #p(textOutput(ns("res1")))
   # tableOutput(ns("table"))
  )
}