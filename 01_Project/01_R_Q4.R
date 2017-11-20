# n <- 10 # number of tests in a set
# p <- 0.01 # probabilty of bumper failing

findTested <- function(j,p,n) {
    
    tested <- 0 # initialise vector
    
    for (k in 1:j) {
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
    tested
}

k <- 1000 # number of total trials

var <- list( c(0.01,10),
             c(0.05, 10),
             c(0.1, 10),
             c(0.01, 5), 
             c(0.01, 20), 
             c(0.01, 40))

a <- findTested(k, var[[1]][1], var[[1]][2])
b <- findTested(k, var[[2]][1], var[[2]][2])
c <- findTested(k, var[[3]][1], var[[3]][2])
d <- findTested(k, var[[4]][1], var[[4]][2])
e <- findTested(k, var[[5]][1], var[[5]][2])
f <- findTested(k, var[[6]][1], var[[6]][2])

list <- list(a,b,c,d,e,f)

par(mfrow=c(3,2))

dist.col <- c("indianred1", "orange", "mediumaquamarine", "deepskyblue2", "hotpink", "mediumpurple")

for (i in 1:6) {
    tested <- list[[i]]
    
    dist.xlab <- "Number of safety tests before first bumper fails"
    dist.ylab <- "Frequency"
    dist.title <- "Distribution of the number of high-impact safety tests \n carried out until one failure \n "
    dist.n <- var[[i]][2]
    dist.p <- var[[i]][1]
    dist.fig <- i
    dist.main <- paste("Figure ", (i+1), ": ", dist.title, "(n=", dist.n, ", p=", dist.p, ")", sep = "")
    
    if ((1/dist.p) <= 10) {
        w <- seq(0, max(tested)+(1/dist.p), by=(1/(dist.p)))
    } else {
        w <- seq(0, max(tested)+(1/dist.p), by=(1/(dist.p*2)))
    }
    
    hist(tested, xlab=dist.xlab, ylab=dist.ylab, main=dist.main, breaks=w, col=dist.col[i] )
}
