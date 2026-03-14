// model_H1.stan
// 대립 모델: 색상 비율을 데이터로부터 추정하는 모델
data {
  int<lower=1> K;       // 카테고리 개수 (3)
  int<lower=0> y[K];    // 관측 데이터 (95, 55, 50)
}
parameters {
  simplex[K] theta;     // 추정할 확률 벡터 (합이 1이 됨)
}
model {
  // Prior for theta (Dirichlet(1,1,1)은 uniform prior)
  theta ~ dirichlet(rep_vector(1.0, K));
  
  // Likelihood
  y ~ multinomial(theta);
}