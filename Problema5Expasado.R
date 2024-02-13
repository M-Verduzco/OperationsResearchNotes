#********************************************************************
#*
#*                 Soluciones: Problema 2 - Nepal
#*                           
#********************************************************************

c = 5
venta = 10
a = 1000
b = 2000
p = 0
salvation = 2.5
shipping =0

cu = venta-c + p
co = c-salvation+shipping

RC = (cu/(cu+co))

Q_opt = qunif(RC, a, b)

sobrantes_fun <- function(x){(Q_opt-x)*(dunif(x, a, b))} #Función general sobrantes
unidades_sobrantes <- integrate(sobrantes_fun, lower = a, upper = Q_opt)

faltantes_fun <- function(y){(y-Q_opt)*(dunif(y, a, b))} #Función general faltantes
unidades_faltantes <- integrate(faltantes_fun, lower = Q_opt, upper = b)

costo_promedio <- co*unidades_sobrantes$value + cu*unidades_faltantes$value

print(paste("Optimo de Comprar", Q_opt))
print(paste("Sobantes", unidades_sobrantes$value))
print(paste("Faltantes", unidades_faltantes$value))
print(paste("Costo", co*unidades_sobrantes$value + cu*unidades_faltantes$value))
print(paste("Ganancia",(venta-c)*Q_opt - co*unidades_sobrantes$value - cu*unidades_faltantes$value))

#*******************
venta = 10
c = 5
a = 1000
b = 2000
p = 0
salvation = 0
shipping =2.5

cu = venta-c + p
co = c-salvation+shipping

RC = (cu/(cu+co))

Q_opt = 1500

sobrantes_fun <- function(x){(Q_opt-x)*(dunif(x, a, b))} #Función general sobrantes
unidades_sobrantes <- integrate(sobrantes_fun, lower = a, upper = Q_opt)

faltantes_fun <- function(y){(y-Q_opt)*(dunif(y, a, b))} #Función general faltantes
unidades_faltantes <- integrate(faltantes_fun, lower = Q_opt, upper = b)

costo_promedio <- co*unidades_sobrantes$value + cu*unidades_faltantes$value

print(paste("Optimo de Comprar", Q_opt))
print(paste("Sobantes", unidades_sobrantes$value))
print(paste("Faltantes", unidades_faltantes$value))
print(paste("Costo", co*unidades_sobrantes$value + cu*unidades_faltantes$value))
print(paste("Ganancia",(venta-c)*Q_opt - co*unidades_sobrantes$value - cu*unidades_faltantes$value))




