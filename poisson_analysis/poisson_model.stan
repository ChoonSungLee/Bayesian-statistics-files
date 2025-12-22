// 파일명: poisson_model.stan
// 역할: 포아송 모델의 구조를 정의합니다. (데이터, 모수, 사전/사후분포)

// 1. 데이터 블록: R로부터 받을 데이터의 종류와 형태를 정의합니다.
data {
  int<lower=0> N;      // 데이터의 총 개수 (양의 정수)
  int<lower=0> y[N];   // 관측된 데이터 (N개의 양의 정수로 이루어진 배열)
}

// 2. 파라미터 블록: 추정하고자 하는 미지의 모수를 정의합니다.
parameters {
  real<lower=0> lambda; // 평균 발생률 lambda (양수)
}

// 3. 모델 블록: 모수와 데이터의 확률적 관계를 정의합니다.
model {
  // 사전분포(Prior): lambda는 exponential(0.2) 분포를 따른다고 가정합니다.
  lambda ~ exponential(0.2);
  
  // 가능도(Likelihood): 데이터 y는 평균이 lambda인 포아송 분포를 따른다고 가정합니다.
  y ~ poisson(lambda);
}

// 4. 생성된 수량 블록: PPC 등 추가 분석을 위해 사후분포로부터 데이터를 생성합니다.
generated quantities {
  int<lower=0> y_rep[N]; // 시뮬레이션으로 생성될 복제 데이터
  for (i in 1:N) {
    // 추정된 lambda의 각 사후 샘플로부터 새로운 y값을 랜덤으로 뽑습니다.
    y_rep[i] = poisson_rng(lambda);
  }
}

