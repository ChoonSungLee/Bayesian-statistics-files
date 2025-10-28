data {
  int<lower=0> N;            // 학생 수
  int<lower=0> J;            // 학교 수
  int<lower=1, upper=J> school[N]; // 각 학생의 학교 ID
  vector[N] y;               // 시험 점수
}
parameters {
  real mu;                   // 전체 평균
  real<lower=0> tau;         // 학교 간 변이
  vector[J] alpha;           // 각 학교의 효과
  real<lower=0> sigma;       // 학생 개별 수준의 변이
}
model {
  alpha ~ normal(0, tau);            // 학교별 효과에 대한 분포
  y ~ normal(mu + alpha[school], sigma); // 관측된 시험 점수 모델링
}
generated quantities {
  vector[N] y_pred;                   // 예측 값
  for (n in 1:N)
    y_pred[n] = normal_rng(mu + alpha[school[n]], sigma); // 새로운 예측 값 생성
  
  real log_lik[N];                   // log-likelihood 계산
  for (n in 1:N)
    log_lik[n] = normal_lpdf(y[n] | mu + alpha[school[n]], sigma);
}