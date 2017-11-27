# the setup ---------------------------------------------------------------

x <- c(
    0.324,
    0.177,
    0.163,
    0.317,
    0.174,
    0.321,
    0.115,
    0.108,
    0.133
)

n <- 10
a <- n
xMin <- 0.1

b <- 0 
for (i in 1:length(x)) {
    b <- b + log(x[i])
}; b <- b - n*log(xMin)

aGam <- factorial(a-1)

# Q3  ---------------------------------------------------------------------

trials <- 1000

dist <- 0
for (y in 1:trials) {
    dist[y] <- (b^a) / (aGam * y)
}

plot(dist, log="x", type='l')


# Q4 ----------------------------------------------------------------------

trials2 <- 10000

y <- 0
for (i in 1:trials2) {
    u <- runif(1, 0, 1)
    theta <- rgamma(1, n, b)
    y[i] <- xMin / ( (1-u)^(1/theta) )
}

probest <- (y > 1) %>% sum() / trials2


# Q5 ----------------------------------------------------------------------

reserve <- quantile(y, 0.99)

probRes <- (y > reserve) %>% sum() / trials2


