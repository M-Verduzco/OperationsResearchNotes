
c <- 20#$/un
K <- 150#$
i = 0.3 #%/mes
pi <- 50#$/unidad

h <- i*c

a <- 100
b <- 200
lambda <- 6000 

# j = 0
nR = 0
Q0 = sqrt( (2*lambda*(K + pi*nR)) / h )
R0 <- a + ( 1- ((Q0*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R0)*(1/(b-a)) },
                                lower = R0,
                                upper = b)
nR0 <- expectedShortUnits$value


# j = 1
Q1 = sqrt( (2*lambda*(K + pi*nR0)) / h )
R1 <- a + ( 1- ((Q1*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R1)*(1/(b-a)) },
                                lower = R1,
                                upper = b)
nR1 <- expectedShortUnits$value

# j = 2
Q2 = sqrt( (2*lambda*(K + pi*nR1)) / h )
R2 <- a + ( 1- ((Q2*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R2)*(1/(b-a)) },
                                lower = R2,
                                upper = b)
nR2 <- expectedShortUnits$value

# j = 3
Q3 = sqrt( (2*lambda*(K + pi*nR2)) / h )
R3 <- a + ( 1- ((Q3*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R3)*(1/(b-a)) },
                                lower = R3,
                                upper = b)
nR3 <- expectedShortUnits$value

# j = 4
Q4 = sqrt( (2*lambda*(K + pi*nR3)) / h )
R4 <- a + ( 1- ((Q4*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R4)*(1/(b-a)) },
                                lower = R4,
                                upper = b)
nR4 <- expectedShortUnits$value

# j = 5
Q5 = sqrt( (2*lambda*(K + pi*nR4)) / h )
R5 <- a + ( 1- ((Q5*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R5)*(1/(b-a)) },
                                lower = R5,
                                upper = b)
nR5 <- expectedShortUnits$value

# j = 6
Q6 = sqrt( (2*lambda*(K + pi*nR5)) / h )
R6 <- a + ( 1- ((Q6*h)/(pi*lambda)) )*(b-a)

expectedShortUnits <- integrate(function(D){ (D-R6)*(1/(b-a)) },
                                lower = R6,
                                upper = b)
nR6 <- expectedShortUnits$value

Qopt <- Q6
Ropt <- R6
nRopt <- nR6


#safetyStock = R - lambdaTau
safetyStock <- Ropt - 150

#totalCost
totalCost <- lambda*c +
             K*(lambda/Qopt) +
             h*(0.5*Qopt + safetyStock) +
             pi*nRopt*(lambda/Qopt)
             
  
  
  
  
  
  
  

