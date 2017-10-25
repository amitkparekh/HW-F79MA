f.mle <- function(x,n,p) { n*(p) - x*(1-p)^n - x }
df.mle <- function(x,n,p) { n + x*n*(1-p)^(n-1) }
f.mme <- function(x,n,p) { (x/n) }
df.mme <- function() { 1 }

T <- 0.0000001 #tolerance

mle <- function(y,n) {
    pHat0 <- 0.2 #inital guess for pHat
    pHat1 <- pHat0 - f.mle(y,n,pHat0)/df.mle(y,n,pHat0)
    
    while (abs(pHat1 - pHat0) > T) {
        pHat0 <- pHat1
        pHat1 <- pHat0 - f.mle(y,n,pHat0)/df.mle(y,n,pHat0)
    }
    pHat1
}

mme <- function(y,n) {
    pTilde0 <- 0.2 #initial guess for pTilde
    pTilde1 <- pTilde0 - f.mme(y,n,pTilde0)
    
    while (abs(pTilde1 - pTilde0) > T) {
        pTilde0 <- pTilde1
        pTilde1 <- pTilde0 - f.mme(y,n,pTilde0)
    }
    pTilde1
}

y <- 1; n <- 20
mle(y,n); mme(y,n)
