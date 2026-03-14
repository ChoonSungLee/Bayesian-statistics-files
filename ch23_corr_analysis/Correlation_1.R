
set.seed(123)    # 재현성 위해 시드 고정

N <- 30          # 표본 크기
true_r <- 0.5    # 실제 상관계수 설정

# 1) 두 변수: (X, Y)가 ~ bivariate normal
#    상관 true_r, 평균/표준편차 임의
library(MASS)
Sigma <- matrix(c(1, true_r, 
                  true_r, 1), 2, 2)   # 공분산행렬
mu <- c(0, 0)                         # 평균벡터(0,0)
sim_data <- MASS::mvrnorm(N, mu, Sigma)

# X를 'Hours', Y를 'Score'라 하자
Hours <- sim_data[,1]*5 + 10   # 적당히 변환(평균 ~10)
Score <- sim_data[,2]*10 + 70  # 적당히 변환(평균 ~70)

my_data <- data.frame(Hours, Score)
head(my_data)

# 빈도주의 상관분석
# 귀무가설(H0): 상관계수 rho = 0
# 대립가설(H1): rho != 0
freq_result <- cor.test(my_data$Hours, my_data$Score)
freq_result



# 베이즈 상관분석
# BayesFactor 패키지 사용: correlationBF
if(!requireNamespace("BayesFactor", quietly = TRUE)){
  install.packages("BayesFactor")
}
library(BayesFactor)

bayes_result <- correlationBF(x = my_data$Hours, 
                              y = my_data$Score, 
                              rscale = "medium") 
# rscale: 효과 크기 스케일 (small, medium, large)

bayes_result

# ---------------------------------------------------------
# [추가] 상관계수의 사후분포 추정 및 시각화
# ---------------------------------------------------------

# 1. 사후분포로부터 샘플 추출 (MCMC 샘플링)
# iterations: 추출할 샘플 수
samples <- posterior(bayes_result, iterations = 10000)

# 2. 샘플 구조 확인
# rho(상관계수) 열이 생성됨을 확인
head(samples)
summary(samples)

# 3. 사후분포 시각화 (상관계수 rho의 분포)
# rho 열만 선택하여 히스토그램 작성
rho_samples <- samples[, "rho"]

# 히스토그램 그릴 때 폰트 지정
# Windows는 "malgun", macOS는 "AppleGothic"
par(family = "AppleGothic") 

hist(rho_samples, breaks = 50, col = "skyblue", border = "white",
     main = "상관계수(rho)의 사후분포",
     xlab = "상관계수 (rho)", 
     ylab = "밀도", 
     freq = FALSE)

# 4. 사후 요약치 추가 (평균 및 95% 신뢰구간)
abline(v = mean(rho_samples), col = "red", lwd = 2, lty = 2) # 사후 평균
abline(v = quantile(rho_samples, c(0.025, 0.975)), col = "blue", lty = 3) # 95% HDI

