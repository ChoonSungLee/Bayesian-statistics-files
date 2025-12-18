data <- read.csv("example1.csv", header = FALSE)
N <- nrow(data)
x <- data$V1

library(rjags)
jags_model <- jags.model(file = "example1.jags", data = list("x" = x, "N" = N), n.chains = 4)
update(jags_model, 100)
samples <- coda.samples(jags_model, c("mu", "tau"), n.iter = 1000)

library(coda)
plot(samples)

summary(samples)
