# 파일명: poisson_Bayes_rstan.R
# 역할: 'rstan' 패키지를 사용하여 포아송 데이터를 분석합니다.
# 실행 전 필요사항: 
# 1. 'create_poisson_data.R' 스크립트가 먼저 실행되어야 합니다.
# 2. 'poisson_model.stan' 파일이 R 프로젝트 폴더 안에 있어야 합니다.

# -------------------------------------------------------------------------
# 1. 환경 설정 및 데이터 불러오기
# -------------------------------------------------------------------------

# 분석에 필요한 패키지를 로드합니다. (필요 시 자동 설치)
if (!require("rstan")) install.packages("rstan")
if (!require("here")) install.packages("here")
if (!require("bayesplot")) install.packages("bayesplot")

library(rstan)
library(here)
library(bayesplot)

# '01_create_poisson_data.R'에서 생성한 .rds 파일을 읽어옵니다.
df_pois <- readRDS(here("data", "df_pois.rds"))

# -------------------------------------------------------------------------
# 2. 베이즈 포아송 모델 적합 (rstan 사용)
# -------------------------------------------------------------------------

# Stan에 전달할 데이터를 R의 리스트(list) 형태로 준비합니다.
stan_data <- list(N = nrow(df_pois), 
                  y = df_pois$y)

# stan() 함수를 호출하여 MCMC 샘플링을 실행합니다.
# file 인자에 here() 함수를 사용해 .stan 파일의 경로를 안전하게 지정합니다.
fit_rstan <- stan(
  file = here("poisson_model.stan"),
  data = stan_data,
  seed = 123
)

# -------------------------------------------------------------------------
# 3. 결과 확인 및 해석
# -------------------------------------------------------------------------

# 적합된 모델의 전체 요약 확인
print(fit_rstan)

# 추정하고자 했던 'lambda' 모수의 사후분포 요약만 따로 확인
print("--- 'lambda' 파라미터의 사후분포 요약 ---")
print(fit_rstan, pars = c("lambda"))
print("---------------------------------------")


# -------------------------------------------------------------------------
# 4. 사후 예측 검증 (Posterior Predictive Check, PPC)
# -------------------------------------------------------------------------

# 'generated quantities' 블록에서 생성된 y_rep 샘플들을 추출합니다.
yrep_rstan <- rstan::extract(fit_rstan)$y_rep

# PPC 시각화 1: 밀도 곡선 겹쳐 그리기
# 실제 데이터(굵은 선)와 예측 데이터(얇은 선)의 분포가 얼마나 비슷한지 확인합니다.
ppc_dens_overlay(y = df_pois$y, yrep = yrep_rstan[1:100, ])

# PPC 시각화 2: 히스토그램 비교
ppc_hist(y = df_pois$y, yrep = yrep_rstan[1:8, ])

# PPC 시각화 3: 특정 데이터 포인트에 대한 예측 분포 확인
# 예시로 10번째 관측값(y[10])에 대한 4000개의 예측값을 히스토그램으로 그립니다.
# 이를 통해 모델이 특정 관측값을 얼마나 잘 예측하는지 확인할 수 있습니다.

# 운영체제에 따른 폰트 설정

# 1. 현재 사용 중인 운영체제 확인
#    Sys.info()['sysname']이 'Darwin'이면 macOS를 의미합니다.
if (Sys.info()['sysname'] == 'Darwin') {
  
  # 2. macOS 사용자일 경우,
  #    그래픽 장치의 기본 폰트(family)를 'AppleGothic'으로 설정합니다.
  par(family = "AppleGothic")
  
}

hist(yrep_rstan[, 10], 
     breaks = 30,
     main = "10번째 관측값(y[10])에 대한 사후 예측 분포",
     xlab = "y[10]의 예측값",
     ylab = "빈도")

# 실제 10번째 관측값이 어디에 위치하는지 빨간색 세로선으로 표시합니다.
abline(v = df_pois$y[10], col = "red", lwd = 2)

# 범례 추가
legend("topright", legend = "실제 관측값", col = "red", lty = 1, lwd = 2)



