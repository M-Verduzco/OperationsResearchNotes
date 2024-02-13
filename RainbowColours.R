K <- 10 #$
l <- 100 #units/month (mean)
t <- 1.5 #month
i <- 0.1/12 #%/month
p <- 10 #$/units
c <- 10 #$/unit


lt <- l*t
h <- i*c

#used when the l ~ Exp(lambda). Remember mean = 1/lambda (see exponential distribution)
lambda = 1/l


#j = 0
nR = 0
Q0 = sqrt( (2*l*(K + p*nR)) / h )
R0 <- -log(1- (1-((Q0*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R0)*(lambda)*exp(-(lambda)*y) }, lower = R0, upper = Inf)
nR0 <- expectedShortUnitsExp$value

#j = 1
Q1 = sqrt( (2*l*(K + p*nR0)) / h )
R1 <- -log(1- (1-((Q1*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R1)*(lambda)*exp(-(lambda)*y) }, lower = R1, upper = Inf)
nR1 <- expectedShortUnitsExp$value

#j = 2
Q2 = sqrt( (2*l*(K + p*nR1)) / h )
R2 <- -log(1- (1-((Q2*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R2)*(lambda)*exp(-(lambda)*y) }, lower = R2, upper = Inf)
nR2 <- expectedShortUnitsExp$value

#j = 3
Q3 = sqrt( (2*l*(K + p*nR2)) / h )
R2 <- -log(1- (1-((Q3*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R3)*(lambda)*exp(-(lambda)*y) }, lower = R3, upper = Inf)
nR3 <- expectedShortUnitsExp$value

#j = 4
Q4 = sqrt( (2*l*(K + p*nR3)) / h )
R4 <- -log(1- (1-((Q4*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R4)*(lambda)*exp(-(lambda)*y) }, lower = R4, upper = Inf)
nR4 <- expectedShortUnitsExp$value

#j = 5
Q5 = sqrt( (2*l*(K + p*nR4)) / h )
R5 <- -log(1- (1-((Q5*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R5)*(lambda)*exp(-(lambda)*y) }, lower = R5, upper = Inf)
nR5 <- expectedShortUnitsExp$value

#j = 6
Q6 = sqrt( (2*l*(K + p*nR5)) / h )
R6 <- -log(1- (1-((Q6*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R6)*(lambda)*exp(-(lambda)*y) }, lower = R6, upper = Inf)
nR6 <- expectedShortUnitsExp$value

#j = 7
Q7 = sqrt( (2*l*(K + p*nR6)) / h )
R7 <- -log(1- (1-((Q7*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R7)*(lambda)*exp(-(lambda)*y) }, lower = R7, upper = Inf)
nR7 <- expectedShortUnitsExp$value

#j = 8
Q8 = sqrt( (2*l*(K + p*nR7)) / h )
R8 <- -log(1- (1-((Q8*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R8)*(lambda)*exp(-(lambda)*y) }, lower = R8, upper = Inf)
nR8 <- expectedShortUnitsExp$value

#j = 9
Q9 = sqrt( (2*l*(K + p*nR8)) / h )
R9 <- -log(1- (1-((Q9*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R9)*(lambda)*exp(-(lambda)*y) }, lower = R9, upper = Inf)
nR9 <- expectedShortUnitsExp$value

#j = 10
Q10 = sqrt( (2*l*(K + p*nR9)) / h )
R10 <- -log(1- (1-((Q10*h)/(p*l))) )/lambda
expectedShortUnitsExp <- integrate(function(y) { (y - R10)*(lambda)*exp(-(lambda)*y) }, lower = R10, upper = Inf)
nR10 <- expectedShortUnitsExp$value

## Policy 
Qopt = Q10
Ropt = R10
nRopt = nR10

#### b) ---------

beta = 1-(nRopt/Qopt)























