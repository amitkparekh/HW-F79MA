# sample data
x <- c(
    0.324,
    0.177,
    0.163,
    0.317,
    0.326,
    0.174,
    0.321,
    0.115,
    0.108,
    0.133
)

n <- length(x)
xMin <- 0.1

# calculate beta
b <- 0 
for (i in 1:n) {
    b <- b + log(x[i])
}
b <- b - n*log(xMin)

# calculate alpha
a <- n 

trials <- 10000


# Plot posterior density

postDensity <- 0
for (i in 1:trials) {
    postDensity[i] <- rgamma(1, a, b)
}
plot(density(postDensity),
     main = "Fig 1: Distribution of the Posterior Density, for 10000 trials",
     xlab="Theta",
     ylab="Probability Density",
     col="indianred1",
     lwd=2)

summary(postDensity)


# Simulate posterior predictive density
y <- 0
for (i in 1:500) {
    y[i] <- 0
    
    while (y[i] < 0.1) {
        u <- runif(1, 0, 1)
        theta <- rgamma(1, n, b)
        y[i] <- xMin / ( (1-u)^(1/theta) )
    }
}


plot(density(y), 
    log="x", 
    type='l',
    main = "Fig 2: Posterior predictive density for the total claim amount for the next year",
    ylab = "Posterior predictive density",
    #xlab = "Total claim amount predicted for the next year",
    col="deepskyblue2",
    lwd=2)

summary(y)


# Estimate probability that y exceeds £1m

y <- 0
for (i in 1:10000) {
    y[i] <- 0
    
    while (y[i] < 0.1) {
        u <- runif(1, 0, 1)
        theta <- rgamma(1, n, b)
        y[i] <- xMin / ( (1-u)^(1/theta) )
    }
}

probest <- (y > 1) %>% sum() / 10000


# Calculate reserve value
reserve <- quantile(y, 0.99)
probRes <- (y > reserve) %>% sum() / trials

