
#***********************************************************
#*
#*
#*               F U N C I Ó N   P R I N C I P A L
#*                
#*                
#*                
#***********************************************************

G <- function(b = 0, lambda=0, P=-1, h=0, K=0, c=0, pi=0, pi_gorro=0, times = FALSE, Qdada = -1){
  if (P == -1){#la producción default tiende a infinito
    if(b != 0){#permitimos backorder
      print("SI backorder con producción infinita")
      if(Qdada==-1){
        Q_opt <- (sqrt(((2*K*lambda/h)-(((pi*lambda)^2)/(h*(h+pi_gorro))))))*(sqrt((h+pi_gorro)/pi_gorro))
      }else{
        Q_opt <- Qdada
      }
      I_barra <- ((Q_opt-b)^2)/(2*Q_opt)
      B_barra <- (b^2)/(2*Q_opt)
      InvMax <- Q_opt-b
      res <- (c*lambda + K*(lambda/Q_opt) + h*I_barra + pi*b*lambda/Q_opt + pi_gorro*B_barra)
    }else{#aquí no hay backorder
      print("NO backorder con producción infinita")
      if(Qdada==-1){
        Q_opt <- sqrt(2*K*lambda/h)
      }else{
        Q_opt <- Qdada
      }
      I_barra <- (1/2)*Q_opt
      B_barra <- 0
      InvMax = Q_opt
      res <- (c*lambda + K*(lambda/Q_opt) + h*I_barra)
    }
  }else{#aquí la producción está definida
    if(b != 0){#permitimos backorder
      print("SI backorder con producción definida")
      if(Qdada==-1){
        Q_opt <- sqrt((2*K*lambda)/(h*(1-lambda/P)))
      }else{
        Q_opt <- Qdada
      }
      I_barra <-  (((Q_opt*(1-lambda/P)-b)^2)/(2*Q_opt*(1-lambda/P)))
      B_barra <-  (((b)^2)/(2*Q_opt*(1-lambda/P)))
      InvMax <- Q_opt*(1-lambda/P) - b
      res <- (c*lambda + K*(lambda/Q_opt) + h*I_barra + pi*b*lambda/Q_opt + pi_gorro*B_barra)
    }else{#aquí no hay backorder
      print("NO backorder con producción definida")
      if(Qdada==-1){
        Q_opt <- sqrt((2*K*lambda)/(h*(1-lambda/P)))
      }else{
        Q_opt <- Qdada
      }
      I_barra <-  (1/2)*(Q_opt)*(1-lambda/P)
      B_barra <-  0
      InvMax <- Q_opt*(1-lambda/P)
      res <- (c*lambda + K*(lambda/Q_opt) + h*I_barra)
    }
  }
  if(times){
    T1 = b/(P - lambda)
    T2 = InvMax/(P - lambda)
    T3 = InvMax/lambda
    T4 = b/lambda
    Tp = T1 + T2
    Tl = T3 + T4
    Ttot = Tp + Tl
    print(paste("T1 = ", round(T1, 4), " - Es el tiempo para recuperarnos de las órdenes retrasadas."))
    print(paste("T2 = ", round(T2, 4), " - Es el tiempo para generar inventario."))
    print(paste("T3 = ", round(T3, 4), " - Es el tiempo para consumir el inventario."))
    print(paste("T4 = ", round(T4, 4), " -  Es el tiempo para generar órdenes retrasadas."))
    print(paste("Tp = ", round(Tp, 4), " -  Es el tiempo total para reabastecernos."))
    print(paste("Tl = ", round(Tl, 4), " -  Es el tiempo total para consumir."))
    print(paste("Tt = ", round(Ttot, 4), " - Es tiempo TOTAL de un periodo."))
    print("")
    print("")
  }
  
  print(paste("La Producción   Óptima es: ", round(Q_opt, 4)))
  print(paste("El Inventario Promedio es: ", round(I_barra, 4)))
  print(paste("El Backorder  Promedio es: ", round(B_barra, 4)))
  print(paste("El Inventario  Máximo  es: ", round(InvMax, 4)))
  print("")
  print(paste("Entonces el costo total es:",  round(res, 4)))
  return (res)
}


#***********************************************************
#*
#*
#*                M A I N  D E  P R U E B A S 
#*                
#*                
#*                
#***********************************************************


#***********************************************************
#*
#*                DECLARACIÓN DE VARIABLES 
#*                
#***********************************************************
# b = 0           #El backorder
# lambda = 640    #La demanda
# P = 4200        #La producción
# i = 1           #Interés por mantener cosas
# K = 400         #El costo por preparación "set-up"?
# c = 92          #Costo por poroducción de unidad física
# h = i*c         #Costo por matener unidad por unidad de tiempo 
# pi = 0          #Costo del backorder
# pi_gorro = 0    #Costo del backorder por unidad de tiempo





#***********************************************************
#*
#*                EJECUCIÓN DE LA FUNCIÓN
#*                
#***********************************************************


#Ejercicio #1
#Caso Original (Harriet)
G1 <- G(b=0, lambda = 10000, P=100000, h=0.2*40, K=5000, c=40)

#Caso 2 (Electronic Hardware Company)
G12 <- G(b=0, lambda = 6000, h = 0.2*44, K=0, c=44, Qdada=1000)


#Caso 3 (Metstamp Company)
G13 <- G(b=0, lambda = 4000, h=43.5*0.2, K=200, c=43.5)


#Ejercicio #2 (Toys International)
G2 <- G(b=0, lambda = 640, K = 400, h = 0.3, c = 92, P = 4200, times = TRUE)

#Caso del costo con setup de $4000
G21 <- G(b=0, lambda = 640, K = 4000, h = 0.3, c = 92, P = 4200, times = TRUE)
