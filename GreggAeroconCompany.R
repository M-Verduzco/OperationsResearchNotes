
K = 300
c = 40
i = 0.2/52
p = 2
tau = 2
lambda = 500
sigma = sqrt(2500)

lambdaTau = lambda*tau 
sigmaTau = sigma*sqrt(tau)
h = i*c

# if nR = 0
Q = sqrt(2*K*lambda/h)
R = qnorm( 1- ((Q*h)/(p*lambda)  ), lambdaTau, sigmaTau )

serviceLevel <- pnorm(R, lambdaTau, sigmaTau, TRUE)


lossFunctionNormal <- function(x){exp(-(x^2)/2)/(sqrt(2*pi)) - x*(1-pnorm(x))}
lossFunctionValue <- lossFunctionNormal(  qnorm( 1- ((Q*h)/(p*lambda))) ) #exactly the same as the below equations
#z = (R-lambdaTau)/sigmaTau
#lossFunctionValue <- lossFunctionNormal( z )

fillRate <- 1 - (lossFunctionValue*sigmaTau/Q)

# average inv 
#two options, the same result
#Option 1
avgInventory = 0.5*Q + (R - lambdaTau)

#Option 2
#z = (R-lambdaTau)/sigmaTau
#avgInventory = 0.5*Q + z*sigmaTau











