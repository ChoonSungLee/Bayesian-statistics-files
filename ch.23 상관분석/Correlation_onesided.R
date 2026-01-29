
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

## ---------------------------
## 1) 양측 검정(기존)
## H0: rho = 0  vs  H1: rho ≠ 0
## ---------------------------
res_twosided <- cor.test(my_data$Hours, my_data$Score,
                         method = "pearson", alternative = "two.sided")
res_twosided

## ---------------------------
## 2) 단측 검정: 양(+)의 상관 있는지?
## H0: rho ≤ 0  vs  H1: rho > 0
## ---------------------------
res_greater <- cor.test(my_data$Hours, my_data$Score,
                        method = "pearson", alternative = "greater")
res_greater
# 참고: one-sided 95% CI는 (하한, 1) 형태로 출력됩니다.
# conf.level을 바꾸고 싶으면 conf.level=0.95 등으로 지정

## ---------------------------
## 3) 단측 검정: 음(–)의 상관 있는지?
## H0: rho ≥ 0  vs  H1: rho < 0
## ---------------------------
res_less <- cor.test(my_data$Hours, my_data$Score,
                     method = "pearson", alternative = "less")
res_less
# 참고: one-sided 95% CI는 (-1, 상한) 형태로 출력됩니다.

## ---------------------------
## (선택) t-통계량과 p값을 수식으로 직접 확인
## ---------------------------
r  <- cor(my_data$Hours, my_data$Score, use = "complete.obs")
n  <- sum(complete.cases(my_data$Hours, my_data$Score))
t0 <- r * sqrt((n - 2) / (1 - r^2))
df <- n - 2

p_two     <- 2 * pt(abs(t0), df = df, lower.tail = FALSE)   # 양측
p_greater <-     pt(t0,        df = df, lower.tail = FALSE) # H1: rho > 0
p_less    <-     pt(t0,        df = df, lower.tail = TRUE ) # H1: rho < 0

list(r = r, t = t0, df = df,
     p_two_sided = p_two, p_greater = p_greater, p_less = p_less)
