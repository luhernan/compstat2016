
Finv <- function(u, lambda){
  return (-log(1-u)/lambda)
}


Verifica <- function(u, lambda){
  X <- Finv(runif(u),lambda)
  
  #Se obtiene una estimacion de la distribucion exponencial simulada
  fit1 <- fitdistr(X, "exponential") 
  
  #Prueba de Kolmogorov-Smirnoff
  ks_value <- ks.test(X, "pexp",fit1$estimate)$p.value
  if(ks_value > 0.05) cat("Segun la prueba de Kolmogorov-Smirnoff, SI se acepta la simulacion ") else cat("NO se acepta la simulacion, segun la prueba de Kolmogorov-Smirnoff")
  
  #Prueba de Anderson-Darling
  ad_value <- ad.test(X, "pexp", fit1$estimate)$p.value
  if(ad_value > 0.05) cat("Segun la prueba de Anderson-Darling, SI se acepta la simulacion ") else cat("NO se acepta la simulacion, segun la prueba de Anderson-Darling")
}
