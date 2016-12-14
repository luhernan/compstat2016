# Module server function
ejemplo <- function(input, output, session) {
  
  fun1 <- reactive({
    texto <- paste("aux <- ", input$expresion1)
    eval(parse(text = texto))
    aux
  })
  
  
  fun2 <- reactive({
    switch(
      input$expresion2,
      "unif" = function(x)
        1 * (x > 0 && x < 1),
      "exp" = function(x)
        dexp(x) * (x > 0),
      "norm" = function(x)
        dnorm(x)
    )
  })
  
  
  output$Grafica <- renderPlot({
    x <- seq(input$xmin, input$xmax, length.out = 100)
    y1 <- sapply(x, fun1())
    y2 <- input$M * sapply(x, fun2())
    plot_limit = c(min(c(y1, y2)), max(c(y1, y2)))
    plot(
      x,
      y1,
      type = "l",
      col = "blue",
      main = "Grafica",
      ylim = plot_limit
    )
    lines(x, y2, col = "red")
    legend("topright",
           c("f", "M*g"),
           col = c("blue", "red"),
           lty = 1)
  })
  
  
  simulaciones <- reactive({
    num_aceptados <- 0
    num_intentos <- 0
    sim_Y <- switch(
      input$expresion2,
      "unif" = function()
        runif(1),
      "exp" = function()
        rexp(1),
      "norm" = function()
        rnorm(1)
    )
    valor_aceptados <- numeric(input$nsim)
    while (num_aceptados < input$nsim) {
      Y <- sim_Y()
      U <- runif(1)
      if (Y >= input$xmin &&
          Y <= input$xmax && U <= (fun1()(Y)) / (input$M * (fun2()(Y)))) {
        num_aceptados <- num_aceptados + 1
        valor_aceptados[num_aceptados] <- Y
      }
      num_intentos <- num_intentos + 1
    }
    list(valor = valor_aceptados,
         tasa_exito = input$nsim / num_intentos)
  })
  
  
  output$tasa_exito <- renderText({
    simulaciones()$tasa_exito
  })
  
  
  output$hist_sim <- renderPlot({
    hist(simulaciones()$valor,
         main = "Histograma de Simulaciones",
         breaks = input$nbins)
  })
}
  
  
  
  