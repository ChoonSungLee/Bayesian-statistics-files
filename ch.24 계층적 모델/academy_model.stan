data {
  int<lower=1> N;                   // 총 학생 수
  int<lower=1> J;                   // 학원 수
  int<lower=1, upper=J> academy_id[N]; // 각 학생이 속한 학원 ID
  vector[N] score;                  // 각 학생의 점수
}

parameters {
  real mu;                         // 전체 학원 평균 점수의 평균 (초모수)
  real<lower=0> sigma_academy;     // 학원별 평균 점수들의 표준편차 (초모수)
  real<lower=0> sigma_student;     // 학생별 점수들의 표준편차 (개별 학생 오차)

  vector[J] theta_raw;             // 표준화된 학원별 평균 (랜덤 효과의 비표준화된 버전)
}

transformed parameters {
  vector[J] theta; // 학원별 평균 점수 (비표준화된 버전)
  theta = mu + sigma_academy * theta_raw; // 계층 구조: 학원 평균 = 전체평균 + 학원별 편차
}

model {
  // 상위 사전분포 (Hyperpriors)
  mu ~ normal(70, 10);              // 전체 평균에 대한 사전분포
  sigma_academy ~ cauchy(0, 2.5); // 학원간 표준편차에 대한 사전분포
  sigma_student ~ cauchy(0, 2.5); // 학생간 표준편차에 대한 사전분포

  // 랜덤 효과의 계층 구조
  theta_raw ~ normal(0, 1);       // 표준화된 학원별 평균은 표준정규분포를 따른다

  // 가능도 (Likelihood)
  for (i in 1:N) {
    score[i] ~ normal(theta[academy_id[i]], sigma_student); // 각 학생 점수는 해당 학원 평균을 따른다
  }
}

generated quantities {
  // 예측된 각 학원의 평균 점수 (사후 예측 분포)
  vector[J] academy_predicted_means;
  for (j in 1:J) {
    academy_predicted_means[j] = normal_rng(mu, sigma_academy); // 전체 분포에서 새로운 학원의 평균을 예측
  }
}










