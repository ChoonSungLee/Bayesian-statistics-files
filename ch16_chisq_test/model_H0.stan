// model_H0.stan
// 귀무 모델: 색상 비율이 0.5, 0.3, 0.2로 고정된 모델
data {
  int<lower=1> K;           // 카테고리 개수 (3)
  int<lower=0> y[K];        // 관측 데이터 (95, 55, 50)
  vector<lower=0>[K] p0;    // 귀무가설의 고정 비율 (0.5, 0.3, 0.2)
}
parameters {
  // 추정할 파라미터가 없음 (No parameters to estimate)
}
model {
  // 고정된 확률 p0을 갖는 다항분포 모델
  y ~ multinomial(p0);
}