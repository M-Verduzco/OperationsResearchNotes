
K = 400
c = 500
i = 0.2
p = 5000

tau = 5/12
T = 2/12

lambda = 15
sigma = 5

lambdaTau = lambda*(tau + T)
sigmaTau = sigma*sqrt(tau + T)
h = i*c

#One
Q = lambdaTau
R = qnorm( 1- ((Q*h)/(p*lambda)), lambdaTau, sigmaTau )
S = R 

# z = (R-lambdaTau)/sigmaTau)
serviceLevel <- pnorm(R, lambdaTau, sigmaTau, TRUE)

lossFunctionNormal <- function(x){exp(-(x^2)/2)/(sqrt(2*pi)) - x*(1-pnorm(x))}
lossFunctionValue <- lossFunctionNormal(  qnorm( 1- ((R*h)/(p*lambda))) )
fillRate <- 1 - (lossFunctionValue*sigmaTau/lambdaTau)

# safety stock = z*sigmaTau
z = (R-lambdaTau)/sigmaTau
safetyStock = z*sigmaTau

#Option 2
#safetyStock = qnorm(serviceLevel)*sigmaTau









