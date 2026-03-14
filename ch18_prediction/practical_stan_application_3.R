library(rstan)

# (1) R에서 데이터 준비
data <- c(9.5, 10.1, 10.3, 9.8, 10.0,
          9.7, 10.2, 9.9, 9.6, 10.4)
n <- length(data)

# 분산이 알려져 있다고 가정
known_variance <- 4
prior_mean <- 10
prior_var <- 1

# Stan에 넘길 data 리스트 
data_list <- list(
  N = n,
  y = data,
  prior_mean = prior_mean,
  prior_var = prior_var,
  known_var = known_variance
)

model_code <- "
data {
  int<lower=1> N;
  real y[N];
  real prior_mean;
  real prior_var;
  real known_var;
}
parameters {
  real mu;
}
model {
  // prior
  mu ~ normal(prior_mean, sqrt(prior_var));
  // likelihood
  y ~ normal(mu, sqrt(known_var));
}
generated quantities {
  real y_new;
  y_new = normal_rng(mu, sqrt(known_var));
}
"

fit <- stan(model_code = model_code,
            data = data_list,
            iter = 2000, chains = 4)

# 결과 확인
print(fit, pars = c("mu", "y_new"))
head(as.matrix(fit))

# 예측분포
posterior_samples <- extract(fit)
# str(posterior_samples)

# mu 파라미터의 MCMC 평균, 신뢰구간
mean_mu <- mean(posterior_samples$mu)
ci_mu <- quantile(posterior_samples$mu, c(0.025, 0.975))

ynew_samples <- posterior_samples$y_new

mean(ynew_samples)
quantile(ynew_samples, probs=c(0.025, 0.5, 0.975))

# 예측분포 시각화
hist(ynew_samples, breaks=30, col="lightblue",
     main="Posterior Predictive Distribution for y_new",
     xlab="y_new")














