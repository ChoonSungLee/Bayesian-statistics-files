
# 파일명: poisson_Bayes_stanglm.R
# 실행 전 필요사항: 'create_poisson_data.R' 스크립트가 먼저 실행되어야 합니다.
  
# -------------------------------------------------------------------------
# 1. 환경 설정 및 데이터 불러오기
# -------------------------------------------------------------------------

# 분석에 필요한 패키지를 로드합니다. (필요 시 자동 설치)
if (!require("rstanarm")) install.packages("rstanarm")
if (!require("here")) install.packages("here")
if (!require("bayesplot")) install.packages("bayesplot")

library(rstanarm)
library(here)
library(bayesplot)

# here()와 readRDS()를 사용해 'data' 폴더에 저장된 데이터 파일을 불러옵니다.
# '01_create_poisson_data.R'에서 생성한 df_pois.rds 파일을 읽어옵니다.
df_pois <- readRDS(here("data", "df_pois.rds"))

# 불러온 데이터의 구조와 앞부분 6행을 확인합니다.
print("--- 불러온 데이터 확인 ---")
print(head(df_pois))
print(dim(df_pois))
print("--------------------------")


# -------------------------------------------------------------------------
# 2. 베이즈 포아송 모델 적합 (Fitting)
# -------------------------------------------------------------------------

# stan_glm() 함수를 사용하여 베이지안 모델을 데이터에 적합시킵니다.
fit_poisson <- stan_glm(
  y ~ 1,
  family = poisson(link = "log"),
  data = df_pois,
  seed = 123,
  refresh = 0 # MCMC 샘플링 진행 과정 출력을 생략합니다.
)

# -------------------------------------------------------------------------
# 3. 결과 확인 및 해석
# -------------------------------------------------------------------------

# 간단한 결과 확인
print(fit_poisson)

# 파라미터의 사후분포에 대한 상세한 요약 (평균, 중앙값, 신뢰구간 등)
print("--- 베이지안 포아송 모델 결과 요약 ---")
print(summary(fit_poisson))
print("---------------------------------")

# -------------------------------------------------------------------------
# 4. 사후 예측 검증 (Posterior Predictive Check, PPC)
# -------------------------------------------------------------------------

# (1) 먼저, 분포의 전체적인 경향성을 보기 위해 충분히 많은 수(500개)의
#     예측 데이터셋을 생성합니다.
yrep_all <- posterior_predict(fit_poisson, draws = 500)

# 생성된 객체의 차원을 확인하여 500개의 시뮬레이션이 생성되었는지 봅니다.
# 결과: 500 100 (500개의 행, 100개의 열)
print("--- 전체 예측 데이터셋 차원 ---")
print(dim(yrep_all))
print("----------------------------")


# (2) 다음으로, 시각적 비교를 위해 대표적인 몇 개(8개)의 예측 데이터셋만
#     따로 생성합니다.
yrep_for_hist <- posterior_predict(fit_poisson, draws = 8)


# (3) ppc_hist() 함수에 시각화용 객체(yrep_for_hist)를 사용하여
#     실제 데이터(y)의 분포와 예측 데이터의 분포를 비교합니다.
ppc_hist(y = df_pois$y, yrep = yrep_for_hist)





