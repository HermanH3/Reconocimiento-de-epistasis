epist <- function(fen, alfa = 0.05){  #fen: vector para ingresar los fenotipos observados
  #                             alfa: margen de error para Chi-Cuadrado 
  
  t <- sum(fen)              #t: suma de los fenotipos ingresados
  
  fteo1 <- c(9, 3, 4)     
  fteo2 <- c(12, 3, 1)
  fteo3 <- c(9, 7)        #fteo: proporción fenotípica teórica
  fteo4 <- c(15, 1)
  fteo5 <- c(13, 3)
  largo <- length(fen)    #preguntando la longitud de los datos  
  
  if(largo == 1 | largo > 3){   
    r <- stop("No hay epistasis con datos iguales 
                  a uno o mayores que tres")
  }
  if(largo == 3){          #usando la longitud para crear el primer
    #                           condicional
    
    esp1 <- (t*fteo1)/16   #esp: vector de la proporción fenotípica 
    #                            esperada de los descendientes
    esp2 <- (t*fteo2)/16
    Resp1 <- (fen - esp1)^2 / esp1     #Resp: estadístico de Chi-
    #                                        Cuadrado de Pearson
    Resp2 <- (fen - esp2)^2 / esp2
    X2.1 <- sum(Resp1)              #X2.: sumatoria de lo anterior
    X2.2 <- sum(Resp2)
    X2.crit <- qchisq(p = alfa, df = largo - 1, lower.tail = FALSE)
    #                       X2.crit:  valor crítico para determinar la
    #                             aceptación o no de la hipótesis.
    if(X2.2 < X2.crit){
      r  <- "Epistasis simple dominante"
    }else{
      if(X2.1 < X2.crit){
      r <-  "Epistasis simple recesiva"
    }else{
      r <- stop("Corresponde a epistasis pero puede haber un caso 
                  de ligamiento genético")
    }
    }  
    }else{
    esp3 <- (t*fteo3)/16
    esp4 <- (t*fteo4)/16
    esp5 <- (t*fteo5)/16
    Resp3 <- (fen - esp3)^2 / esp3
    Resp4 <- (fen - esp4)^2 / esp4
    Resp5 <- (fen - esp5)^2 / esp5
    X2.3 <- sum(Resp3)
    X2.4 <- sum(Resp4)
    X2.5 <- sum(Resp5)
    X2.crit <- qchisq(p = alfa, df = largo - 1, lower.tail = FALSE)
    if(X2.3 < X2.crit){
      r <-  "Epistasis doble recesiva"
    }else{
      if(X2.4 < X2.crit){
        r <-  "Epistasis doble dominante"
      }else{
        if(X2.5 < X2.crit){
        r <-  "Epistasis doble dominante recesiva"
        }else{
        r <- stop("Corresponde a epistasis pero puede haber un caso 
                  de ligamiento genético") 
        }  
      }
   }
  }
  r
}
