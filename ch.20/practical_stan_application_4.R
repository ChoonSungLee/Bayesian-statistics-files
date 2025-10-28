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
  real y_rep[N];
  
  for (i in 1:N)
  y_rep[i] = normal_rng(mu, sqrt(known_var));
}
"

fit <- stan(model_code = model_code,
            data = data_list,
            iter = 2000, chains = 4)

# 결과 확인
print(fit, pars = c("mu", "y_rep", "lp__"))
head(as.matrix(fit))

# 예측분포
posterior_samples <- extract(fit)
# y_rep는 dimensions: [iterations, N]
y_rep_matrix <- posterior_samples$y_rep
  
# 실제 데이터 통계량
T_obs_mean <- mean(data)           # 실제 데이터의 평균
T_obs_sd   <- sd(data)             # 실제 데이터의 표준편차(등)

# 복원 데이터 통계량
# 각 iteration별로 y_rep의 평균을 구한다
T_rep_means <- apply(y_rep_matrix, 1, mean)
T_rep_sds   <- apply(y_rep_matrix, 1, sd)

# 분포 비교
hist(T_rep_means, breaks=30, col="lightblue",
     main="Distribution of replicated means",
     xlab="Replicated data mean")
abline(v = T_obs_mean, col="red", lwd=2)

# 시각적으로, T_obs_mean이 T_rep_means 분포의 중앙 부근인지 확인.

# 혹은 p-value-like 지표를 간단히 볼 수도 있음.
mean(T_rep_means > T_obs_mean)  # T_obs가 어느 분위수인지











