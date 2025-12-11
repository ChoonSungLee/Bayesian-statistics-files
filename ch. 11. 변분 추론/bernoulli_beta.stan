//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.

// bernoulli_beta.stan

data {
  int<lower=0> N;         // 데이터 개수
  int<lower=0,upper=1> x[N]; // 베르누이 관측값
}
parameters {
  real<lower=0,upper=1> p; // 성공 확률
}
model {
  p ~ beta(1, 1);          // prior
  x ~ bernoulli(p);        // likelihood
}
