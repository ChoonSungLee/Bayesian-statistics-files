data {
  int n; 
  int N[n]; // n개의 원소를 갖는 integer로 이루어진 변수 N 선언 
  int X[n]; // 종양에 걸린 쥐의 마릿수
}

parameters {
  real<lower=0, upper=1> theta[n];
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  for (i in 1:n) {
    X[i] ~ binomial(N[i], theta[i]);
    theta[i] ~ beta(alpha, beta); 
  }
  alpha ~ gamma(0.001, 0.001);
  beta ~ gamma(0.001, 0.001);
}
