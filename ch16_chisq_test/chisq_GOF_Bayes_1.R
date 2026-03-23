
# 1. 기본 정보 설정 (빈도주의 결과 활용)
observed <- c(95, 55, 50)
n <- sum(observed)    # 전체 표본 크기: 200
K <- length(observed) # 범주의 개수: 3
chi_sq_stat <- 3.1667 # 앞서 계산한 카이제곱 통계량

# 2. BIC 차이를 이용한 베이즈 팩터 근사 계산
delta_bic <- (K - 1) * log(n) - chi_sq_stat
bf_01_approx <- exp(delta_bic / 2)

print(paste("BIC 기반 BF01 근사치:", round(bf_01_approx, 2)))
# 결과: "BIC 기반 BF01 근사치: 41.04"
