pHat <- 0.2 #inital guess for p
n <- 10
y <- 2

func <- function(n,y,pHat) {
    n*pHat - y*(1-pHat)^n - y
}

dfunc <- function(n,y,pHat) {
    n + y*n*(1-pHat)^(n-1)
}

err <- func(n,y,pHat) # calculate error

while (abs(err) > 0.0000001) {
    pHat <- pHat - err/(n + y*n*(1-pHat)^(n-1) )
    err <- func(n,y,pHat)
}