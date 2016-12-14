require(MASS)
require("goftest")


tarea1 <- function(input, output, session) {
  observeEvent(input$btn, {
    output$summary <- renderPrint({
      verifica()
    })
  })
  
  verifica <- reactive({
    fit1 <- fitdistr(finv(), "exponential")
    
    #Prueba de Kolmogorov-Smirnoff
    ks_value <- ks.test(finv(), "pexp", fit1$estimate)$p.value
    if (ks_value > 0.05)
      output$res1 <-
      renderText("Segun la prueba de Kolmogorov-Smirnoff, SI se acepta la simulacion")
    else
      output$res1 <-
      renderText("NO se acepta la simulacion, segun la prueba de Kolmogorov-Smirnoff")
    
    #Prueba de Anderson-Darling
    ad_value <- ad.test(finv(), "pexp", fit1$estimate)$p.value
    if (ad_value > 0.05)
      output$res2 <-
      renderText("Segun la prueba de Anderson-Darling, SI se acepta la simulacion")
    else
      output$res2 <-
      renderText("NO se acepta la simulacion, segun la prueba de Anderson-Darling")
  })
  
  #Simulaciones
  finv <- reactive({
    res <- (-log(1 - runif(input$simulaciones))) / input$lambda
  })
  
  #Despliega el vector de simulaciones
  output$simulaciones <- renderText(finv())
  
  
  #Para descargar el vector de simulaciones como un archivo .CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("simulacion-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(finv(), file)
    }
  )
  
  output$histograma <- renderPlot({
    hist(
      main = "Histograma de Simulaciones" ,
      finv(),
      breaks = input$bins,
      xlab = "Simulaciones",
      ylab = "Frecuencia"
    )
  })
  
}