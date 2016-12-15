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
  
  #Para la sigma, elegi la distribucion gamma
  output$apriorig <- renderPlot({
    x <- seq(-50, 50, length=100)
    plot(x, gdist(), type="l", lty=2, xlab="X",ylab="Densidad", main=paste('A Priori Sigma'))
  })
  
  
  likelihood <- function(param){
    b1= param[1]
    b0 = param[2]
    sigma2 = param[3]
    pred = b1* dataframe()$data[,input$independiente] + b0
    singlelikelihoods = dnorm(dataframe()$data[,input$dependiente], mean = pred, sd = sigma2**.5, log = T)
    sumll = sum(singlelikelihoods)
    return(sumll)
  }
  
  prior <- function(param){
    b1 = param[1]
    b0 = param[2]
    sigma2 = param[3]
    b1prior = dnorm(b1, mean=round(nreg()$betas[1,1],digits=2), sd=round(nreg()$betas[1,2]**.5,digits=2), log = T)
    b0prior = dnorm(b0, mean=round(nreg()$betas[2,1],digits=2), sd=round(nreg()$betas[2,2]**.5,digits=2), log = T)
    sigma2prior = dinvgamma(sigma2,14,round(25*nreg()$summary$sigma,digits=2),log = T)
    return(b1prior+b0prior+sigma2prior)
  }
  
  posterior <- function(param){
    return (likelihood(param) + prior(param))
  }
  
  #Metropolis
  
  proposalfunction <- function(param){
    return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
  }
  
  run_metropolis_MCMC <- function(startvalue, iterations){
    chain <- array(dim = c(iterations+1,3))
    chain[1,] <- startvalue
    for (i in 1:iterations){
      proposal <- proposalfunction(chain[i,])
      
      logprobab =posterior(proposal) - posterior(chain[i,])
      if (log(runif(1)) <= logprobab){
        chain[i+1,] = proposal
      }else{
        chain[i+1,] = chain[i,]
      }
    }
    return(chain)
  }
  
  mcmc <- reactive({
    startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
    chain = run_metropolis_MCMC(startvalue, input$simulaciones)
    data.frame(b1=chain[,1],b0=chain[,2],s2=chain[,3])
  })
  
  output$result <- renderDataTable({
    mcmc()
  })
  
  output$histograma <- renderPlot({
    burnIn = input$simulaciones*.20
    acceptance = 1-mean(duplicated(mcmc()[-(1:burnIn),]))
    par(mfrow = c(2,3))
    hist(mcmc()[-(1:burnIn),1],nclass=30,  main="a Posteriori de Alfa", xlab="Parametro", ylab="Frecuencia")
    abline(v = mean(mcmc()[-(1:burnIn),1]))
    hist(mcmc()[-(1:burnIn),2],nclass=30, main="a Posteriori de Beta", xlab="Parametro", ylab="Frecuencia")
    abline(v = mean(mcmc()[-(1:burnIn),2]))
    hist(mcmc()[-(1:burnIn),3],nclass=30, main="a Posteriori de Sigma^2", xlab="Parametro", ylab="Frecuencia")
    abline(v = mean(mcmc()[-(1:burnIn),3]) )
    plot(mcmc()[-(1:burnIn),1], type = "l", xlab="Iteraciones" , main = "Valores de las Cadenas de Alfa" )
    plot(mcmc()[-(1:burnIn),2], type = "l", xlab="Iteraciones" , main = "Valores de las Cadenas de Beta")
    plot(mcmc()[-(1:burnIn),3], type = "l", xlab="Iteraciones" , main = "Valores de las Cadenas de Sigma^2")
  })
  
  output$distribuciones<-renderPlot({
    
    burnIn = input$simulaciones*.20
    
    par(mfrow = c(1,3))
    
    d1 <- density(mcmc()[-(1:burnIn),1])
    d2 <- density(mcmc()[-(1:burnIn),2])
    d3 <- density(mcmc()[-(1:burnIn),3])
    
    plot(d1,main = "a Posteriori de Alfa")
    plot(d2,main = "a Posteriori de Beta")
    plot(d3,main = "a Posteriori de Sigma^2")
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