# 예제 데이터(10개 생성: 실제론 랜덤이므로 실행할 때마다 달라집니다)
set.seed(123)
x <- rnorm(10, mean = 2, sd = 1)  # 실제 모평균 2 가정하에 난수 생성

# 기초 통계량
n    <- length(x)    # 표본크기
xbar <- mean(x)      # 표본평균
s    <- sd(x)        # 표본 표준편차

# 가설: H0: mu = 0  vs.  H1: mu != 0
mu0  <- 0

# 유의수준 alpha = 0.05 (양측 검정)
alpha <- 0.05

#t-statistic 계산
t_stat <- (xbar - mu0) / (s / sqrt(n))
t_stat

# 양측 검정이므로 절댓값 기준으로 꼬리 양쪽 합
p_value <- 2 * (1 - pt(abs(t_stat), df = n - 1))
p_value

# 검정 결론
if (p_value < alpha) {
  cat("p-value =", p_value, "< alpha ⇒ 귀무가설 기각 (유의차 있음)\n")
} else {
  cat("p-value =", p_value, ">= alpha ⇒ 귀무가설 채택 (유의차 없음)\n")
}

#t-critical 구하기
t_crit <- qt(1 - alpha/2, df = n - 1)
t_crit

#둘의 비교
cat("Observed t-statistic =", t_stat, "\n")
cat("Critical value =", t_crit, "\n")

if (abs(t_stat) > t_crit) {
  cat("결론: |t_stat| > t_crit ⇒ 귀무가설 기각\n")
} else {
  cat("결론: |t_stat| <= t_crit ⇒ 귀무가설 채택\n")
}

