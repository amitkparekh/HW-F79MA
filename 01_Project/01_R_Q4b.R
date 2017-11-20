
k <- 1000 # number of total trials

tested <- 0 # initialise vector


m <- 0
no <- c(5, 10, 25, 40, 80, 160)

p <- 0.075 # probabilty of bumper failing
for (b in 1:length(no)) {
    n <- no[b]
    
    for (k in 1:k) {
        tests <- 0
        
        i <- 0
        
        while (i == 0) {
            x <- rbinom(n,1,p)
            if ( sum(x) >= 1 ) {
                i <- 1 
            }
            tests <- tests + n
        }
        tested[k] <- tests
    }
    
    m[b] <- mean(tested)
}; m

