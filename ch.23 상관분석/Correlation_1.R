
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

