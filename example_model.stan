
data {
  int<lower=0> N;       // 데이터 포인트 수
  real y[N];            // 관측된 데이터
}

parameters {
  real mu;              // 평균
  real<lower=0> sigma;  // 표준편차
}

model {
  mu ~ normal(0, 10);
  sigma ~ cauchy(0, 2);
  y ~ normal(mu, sigma);
}

generated quantities {
  real y_pred[N];             // 예측값
  real p_extreme_max;         // Bayesian p-value for 최대값
  real p_extreme_min;         // Bayesian p-value for 최소값
  int indicator_max[N];       // Indicator for max 초과
  int indicator_min[N];       // Indicator for min 초과
  real log_lik[N];            // Log likelihood 저장
  
  real y_max = max(y);        // 데이터의 최대값
  real y_min = min(y);        // 데이터의 최소값

  // 사후 예측 및 지표 변수 계산
  for (n in 1:N) {
    y_pred[n] = normal_rng(mu, sigma);
    indicator_max[n] = (y_pred[n] > y_max) ? 1 : 0;
    indicator_min[n] = (y_pred[n] < y_min) ? 1 : 0;

    // Log likelihood 계산
    log_lik[n] = normal_lpdf(y[n] | mu, sigma);
  }

  // Bayesian p-values 계산
  p_extreme_max = mean(to_array_1d(indicator_max));
  p_extreme_min = mean(to_array_1d(indicator_min));
}

