#################################################################
# 연습 문제: 태양 폭발 진실게임 (xkcd.com/1132)
# 빈도주의적 접근과 베이즈주의적 접근의 비교
#################################################################

# --- 1. 문제 상황 설정 ---

# 탐지기가 거짓말을 할 확률 (주사위 두 개가 모두 6이 나올 확률)
p_lie <- 1/36

# 탐지기가 진실을 말할 확률
p_true <- 1 - p_lie

# 태양이 폭발했을 사건(H1)의 사전 확률 (매우 낮음)
prior_H1 <- 1e-8

# 태양이 폭발하지 않았을 사건(H0)의 사전 확률
prior_H0 <- 1 - prior_H1

# 빈도주의 검정을 위한 유의수준 알파
alpha <- 0.05

# 관측된 데이터
observed_data <- "YES"

cat("--- 문제 설정 완료 ---\n")
cat("탐지기의 거짓말 확률:", round(p_lie, 4), "\n")
cat("태양 폭발의 사전 확률:", prior_H1, "\n\n")


# --- 2. 빈도주의적 접근 (Frequentist Approach) ---

cat("--- 2. 빈도주의적 접근 ---\n")

# 가설 설정
# H0: 태양은 폭발하지 않았다.
# H1: 태양은 폭발했다.

# p-값 계산: H0가 사실일 때(태양이 안전할 때), "YES"라는 데이터가 관측될 확률
# 즉, 탐지기가 거짓말을 할 확률
p_value <- p_lie

cat("계산된 p-값:", round(p_value, 4), "\n")

# 결론 도출
cat("결론: ")
if (p_value < alpha) {
  cat("p-값이 유의수준", alpha, "보다 작으므로 귀무가설(H0)을 기각한다.\n")
  cat(">> 최종 결론: 태양은 폭발했다.\n\n")
} else {
  cat("p-값이 유의수준", alpha, "보다 크므로 귀무가설(H0)을 기각하지 못한다.\n")
  cat(">> 최종 결론: 태양이 폭발했다는 증거가 부족하다.\n\n")
}


# --- 3. 베이즈주의적 접근 (Bayesian Approach) ---

cat("--- 3. 베이즈주의적 접근 ---\n")

# 가능도(Likelihood) 설정
# P(데이터="YES" | H1): 태양이 폭발했을 때 "YES"라고 할 확률 (진실)
likelihood_H1 <- p_true

# P(데이터="YES" | H0): 태양이 안전할 때 "YES"라고 할 확률 (거짓말)
likelihood_H0 <- p_lie

cat("가능도 P(YES|H1):", round(likelihood_H1, 4), "\n")
cat("가능도 P(YES|H0):", round(likelihood_H0, 4), "\n")

# 주변 가능도(Marginal Likelihood) 계산: P(데이터="YES")
# P(YES) = P(YES|H1)*P(H1) + P(YES|H0)*P(H0)
marginal_likelihood <- (likelihood_H1 * prior_H1) + (likelihood_H0 * prior_H0)

# 사후 확률(Posterior Probability) 계산: P(H1 | 데이터="YES")
posterior_H1 <- (likelihood_H1 * prior_H1) / marginal_likelihood

cat("태양 폭발의 사후 확률 P(H1|YES):", format(posterior_H1, scientific = TRUE, digits = 3), "\n")

# 베이즈 인자(Bayes Factor) B10 계산 (참고용)
# 데이터 자체의 증거력: H1이 H0보다 데이터를 몇 배나 더 잘 설명하는가
bayes_factor_B10 <- likelihood_H1 / likelihood_H0
cat("베이즈 인자 (B10):", round(bayes_factor_B10, 2), "\n")
cat(" -> 데이터 자체는 H1(폭발)을 H0(안전)보다 35배 더 강하게 지지함을 의미\n")

# 결론 도출
cat("결론: 사후 확률이 극도로 낮으므로, 탐지기 신호에도 불구하고 태양이 폭발했다는 주장은 믿기 어렵다.\n")
cat(">> 최종 결론: 태양은 폭발하지 않았고, 탐지기가 오류를 일으켰을 가능성이 매우 높다.\n")

