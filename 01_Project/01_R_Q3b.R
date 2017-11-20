# set functions of MLE and MME
f.mle <- function(x,n,p) { n*p - x*(1-p)^n - x }
df.mle <- function(x,n,p) { n + x*n*(1-p)^(n-1) }
f.mme <- function(x,n,p) { x/n }

pHat <- 0.2 # set inital guess for pHat
pTilde <- pHat # set initial guess for pTilde
T <- 0.000001 #set tolerance

no <- c(5,10,20) # number of trials
n <- no %>% length() %>% no[.] # set n as the largest number of trials

lst <- list(numeric(5), numeric(10), numeric(20)) # initialise objects in a list

# calculate mle for different values of n
mle <- 0
for (n in no) { 
    for (y in 1:n) {
        err <- f.mle(y,n,pHat) # calculate error
        
        while (abs(err) > T) {
            pHat <- pHat - err/df.mle(y,n,pHat)
            err <- f.mle(y,n,pHat)
        }
        
        mle[y] <- pHat
    }
    index <- which(no == n)
    
    lst[[index]] <- mle
}

# plot lines on graph

lines.col <- c("deepskyblue2", "mediumaquamarine", "indianred1")

plot (1:n, 
      lst[[3]],
      xaxt='n',
      yaxt='n',
      type='l', 
      col= lines.col[3],
      lwd=2,
      main = paste('Figure 1: Estimated probability for different numbers',
                   'of trials within a set of safety tests'),
      xlab = 'Number of trials',
      ylab = 'Probability')

for (i in 1:(length(no)-1)) {
    i <- i
    lines (1:(no[i]), (lst[[i]]), col=lines.col[i], lwd=2)
}

legend( "bottomright", 
        inset=.05, 
        legend=c("MLE when n=5", "MLE when n=10", "MLE when n=20"), 
        col= lines.col,
        lty=1,
        lwd=2,
        cex=0.8)

axis(1, at=seq(1, no[3],by=1)); axis(2, at=seq(0,1,by=0.1))

