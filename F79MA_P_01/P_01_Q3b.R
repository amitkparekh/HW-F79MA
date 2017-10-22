pHat <- 0 #inital guess for p
n <- 10

func <- function(n,y,pHat) {
    n*pHat - y*(1-pHat)^n - y
}

dfunc <- function(n,y,pHat) {
    n + y*n*(1-pHat)^(n-1)
}

res <- 0

for (y in 1:n) {
    err <- func(n,y,pHat)
    
    while (abs(err)>0.0000001) {
        pHat <- pHat - err/dfunc(n,y,pHat)
        err <- func(n,y,pHat)
    }
    
    res[y] <- pHat
}

plot (1:n, res)