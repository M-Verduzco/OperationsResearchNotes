# Determine the Q* with uniform distribution -------->>>>>>
purchase <- 2
sellingPrice <- 8
penalty <- 1

cu <- sellingPrice - purchase
co <- purchase + penalty

criticalRatio =cu/(co+cu)

a <- 200
b <- 400

Qopt = a + criticalRatio*(b-a) # Buscamos en su libro de estadística
Q = Qopt
# Expected unsold units (sobrantes) x = demand
expectedUnsoldUnits <- function(x) { (Q - x)*(1/(b-a)) }
unsoldUnits <- integrate(expectedUnsoldUnits, lower = a, upper = Qopt)


# Expected units of short units (faltantes) y = demand
expectedShortUnits <- function(y) { (y - Q)*(1/(b-a)) }
shortUnits <- integrate(expectedShortUnits,lower = Qopt, upper = b)

totalCost <- cu*shortUnits$value + co*unsoldUnits$value
totalCost

print(paste0("Average overage units: ", unsoldUnits$value))
print(paste0("Average underage units: ", shortUnits$value))

print(paste0("Average overage cost: ", co*unsoldUnits$value))
print(paste0("Average underage cost: ", cu*shortUnits$value))

ExpectedProfitOpt <- (sellingPrice - purchase)*Qopt - totalCost
print(paste0("Expected Profit : ", ExpectedProfitOpt))



#If Q = 300
Q = 300
# Expected unsold units (sobrantes) x = demand
expectedUnsoldUnits <- function(x) { (Q - x)*(1/(b-a)) }
unsoldUnits <- integrate(expectedUnsoldUnits, lower = a, upper = Q)


# Expected units of short units (faltantes) y = demand
expectedShortUnits <- function(y) { (y - Q)*(1/(b-a)) }
shortUnits <- integrate(expectedShortUnits,lower = Q, upper = b)

totalCost <- cu*shortUnits$value + co*unsoldUnits$value
totalCost

print(paste0("Average overage units: ", unsoldUnits$value))
print(paste0("Average underage units: ", shortUnits$value))

print(paste0("Average overage cost: ", co*unsoldUnits$value))
print(paste0("Average underage cost: ", cu*shortUnits$value))

ExpectedProfitQ <- (sellingPrice - purchase)*Q - totalCost
print(paste0("Expected Profit : ", ExpectedProfitQ))



