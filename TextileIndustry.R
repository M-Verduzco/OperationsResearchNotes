
K <- 200
c = 5
i = 0.2
h = i*c
p = 4

lambda = 10000
lambdaTau = 400
sigmaTau = 30 

tau = lambdaTau/lambda

R = 400
#nR = L(z)*sigmaTau, z = (R-lambdaTau)/sigmaTau
z = (R - lambdaTau)/sigmaTau
lossFunctionNormal <- function(x){exp(-(x^2)/2)/(sqrt(2*pi)) - x*(1-pnorm(x))}
nR = lossFunctionNormal( z )*sigmaTau # L(z)*sigmaTau

Q = sqrt( (2*lambda*(K + p*nR)) / h )


#costSortage = pi*nR*(lambda/Q)
costSortage = p*nR*(lambda/Q)

# 1 - beta = nR/Q

unmetDemand = (nR/Q)*100

