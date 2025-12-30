set.seed(123)
x <- rbinom(10, 10, 1/2)
x
x <- c(4, 6, 5, 7, 7, 2, 5, 7, 5, 5)

f_x_m0 <- function(x, n) {
  lambda <- rgamma(n, 1, 1)
  f <- dpois(x, lambda)
  return(mean(f))
}
f_x_m0(x, 10000)

f_x_m1 <- function(x, n) {
  p <- rbeta(n, 1, 1)
  f <- dbinom(x, 10, p) 
  return(mean(f))
}
f_x_m2 <- function(x, n) {
  p <- rbeta(n, 1, 1)
  f <- dnbinom(x, 2, p)
  return(mean(f))
}
f_x_m1(x, 10000)
f_x_m2(x, 10000)

pi_m <- rep(1/3, 3)
B_m0 <- f_x_m0(x, 10000) / f_x_m0(x, 10000)
B_m1 <- f_x_m1(x, 10000) / f_x_m0(x, 10000)
B_m2 <- f_x_m2(x, 10000) / f_x_m0(x, 10000)
pi_m_x <- pi_m * c(B_m0, B_m1, B_m2) / sum(pi_m * c(B_m0, B_m1, B_m2))
pi_m_x
