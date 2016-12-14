library(Rcpp)
library(invgamma)

sourceCpp('Tarea3/functions.cpp')


tarea3 <- function(input, output, session) {
  
  
  
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  
  max_value <- reactiveValues(estado = 0)
  
  dataframe <- reactive({
    data <- read.csv(userFile()$datapath,
                     header = input$heading)
    #Para actualizar las variables en la interfaz, conforme al CSV leido
    updateSelectInput(session, 
                      label = "Variable Dependiente (Y)", 
                      "dependiente", 
                      choices = names(data))
    updateSelectInput(session, 
                      label = "Variable Independiente (X)", 
                      "independiente", 
                      choices = names(data))
    #Para regresion multiple
    #updateCheckboxGroupInput(session, "inCheckboxGroup",
    #                         label = "Variable(s) Independiente(s) (X)",
    #                         choices = names(data)
    #                         )
    
    #Para generar el grafico de dispersion
    output$plot <- renderPlot({
      y <- input$dependiente
      x <- input$independiente
      plot(data[,x],data[,y], xlab="Variable Dependiente", ylab="Variable Independiente")
    })
    
    #La informacion leida del .CSV se almacena como una matriz
    data_matrix <- as.matrix(data)
    list('matrix' = data_matrix, 'data' = data)
  })
  
  #Se realiza la regresion, para determinar las distribuciones a priori
  nreg <- reactive({
    datos <- dataframe()$data
    equis <- datos[,input$independiente]
    ye <- datos[,input$dependiente]
    sp <- data.frame(equis,ye)
    splm <- lm(ye~equis,data=sp)
    summary_splm <- summary(splm)
    betas <- coefficients(summary_splm)
    list('betas' = betas, 'summary' = summary_splm)
  })
  
  
  ndist <- reactive({
    x <- seq(-50, 50, length=100)
    dnorm(x,round(nreg()$betas[1,1],digits=2),round(nreg()$betas[1,2],digits=2))
  })
  
  gdist <- reactive({
    x <- seq(-50, 50, length=100)
    dinvgamma(x,13.5,round(25*nreg()$summary$sigma,digits=2))
  })
  
  #Para las alfas, elegi la distribucion normal
  output$aprioria <- renderPlot({
    x <- seq(-50, 50, length=100)
    plot(x, ndist(), type="l", lty=2, xlab="X",ylab="Densidad", main=paste('A Priori Alfas'))
  })
  
  #Para las betas, elegi la distribucion normal
  output$apriorib <- renderPlot({
    x <- seq(-50, 50, length=100)
    plot(x, ndist(), type="l", lty=2, xlab="X",ylab="Densidad", main=paste('A Priori Betas'))
  })
  
  #Para la gamma, elegi la distribucion gamma
  output$apriorig <- renderPlot({
    x <- seq(-50, 50, length=100)
    plot(x, gdist(), type="l", lty=2, xlab="X",ylab="Densidad", main=paste('A Priori Eps'))
  })

  
  #'Observador' para delimitar el numero de estados con relacion al archivo seleccionado
  observeEvent(input$file, {
    maa <- dataframe()
    max_value$estado <- (dim(maa))[1]
    updateNumericInput(session, "edo_inicial", max = max_value$estado)
  })
  
  #Observador para delimitar el numero de estados con respecto al encabezado
  observeEvent(input$heading, {
    maa <- dataframe()
    #La dimension de la matriz obtenida del CSV permite determinar el numero
    #maximo de estados que el usuario puede seleccionar
    max_value$estado <- (dim(maa))[1]
    updateNumericInput(session, "edo_inicial", max = max_value$estado)
  })
  
  
  output$table <- renderDataTable({
    dataframe()$matrix
  })
  
  
  observe({
    msg <-
      sprintf("El archivo %s se cargo exitosamente", userFile()$name)
    cat(msg, "\n")
  })
  
  
  #output$traj <- renderText({
  #  markovchain_trajectory(
  #    init_state = input$edo_inicial,
  #    n_transitions = input$simulaciones,
  #    trans_mat = dataframe()
  #  )
  #})
  
  
}