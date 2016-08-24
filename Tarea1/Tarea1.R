#Autor: Luis Daniel Hernandez Sandoval
#https://luhernan.shinyapps.io/Tarea1/

library(shiny)
source("inversaD.R")

ui <- fluidPage(
  
  titlePanel("Metodo de la Funcion Inversa" , windowTitle = "Inversa"),
  numericInput(inputId = "lambda",
            label = "Valor de Lambda",
            value = "3.0"),
  numericInput(inputId = "simu",
               label = "# de Simulaciones",
               value = "1000"),
  actionButton("btn", "Verificar"),
  htmlOutput("summary"),
  plotOutput("hist")
)


server <- function(input, output){
  
  datalambda <- reactive ({ 
    input$lambda
    })
  
  datasimu <- reactive ({
    input$simu
   })
  
   observeEvent(input$btn, {
    output$summary <- renderPrint({
      Verifica(datasimu(),datalambda())
    })
    
  })
  
  output$hist <- renderPlot({
    hist(main = "Histograma de Simulaciones" ,Finv(runif(datasimu()),datalambda()))
  })

  }

shinyApp(ui = ui, server = server)