
library(rstan)
options(mc.cores = parallel::detectCores())
parallel::detectCores()
Y <- rnorm(10, 1.5, 0.2)
Y
hist(Y)
fit <- stan('simple.stan', iter=200, chains=4, 
            data=list(Y=Y))

print(fit, probs = c(0.25, 0.5, 0.75))
plot(fit)

plot(fit, show_density=TRUE)

library(ggplot2)
mu <- extract(fit, 'mu') [[1]]

# ------ above; mc samling
# ------- below; Monte_carlo approximation

qplot(mu)
mean(mu)
var(mu)
sd(mu)

if (!require("shinystan")) install.packages("shinystan")
library(shinystan)
aFit <- as.shinystan(fit)
launch_shinystan(aFit)

