# -------------------------------------------------------------------
# Frequentist_Prostate.R
# 전립선암 3개 치료군 비교 (빈도주의 방식)
# -------------------------------------------------------------------

# 1. 데이터 준비 (Data Frame 생성)
# 빈도주의 함수들은 보통 '성공 횟수'와 '전체 횟수'를 벡터로 받거나 
# 테이블(Matrix) 형태를 선호합니다.

groups <- c("Surgery", "IMRT", "Proton")
successes <- c(42, 40, 38)
totals <- c(50, 50, 50)
failures <- totals - successes

# 데이터 확인
data_matrix <- rbind(successes, failures)
colnames(data_matrix) <- groups
print("데이터 행렬:")
print(data_matrix)

# -------------------------------------------------------------------
# 2. 점 추정 (MLE Point Estimation)
# -------------------------------------------------------------------
# MLE는 단순히 (성공 수 / 전체 수)입니다.
mle_estimates <- successes / totals
cat("\n[MLE 추정치 - 성공률]\n")
for(i in 1:3) {
  cat(sprintf("%s: %.2f (%.1f%%)\n", groups[i], mle_estimates[i], mle_estimates[i]*100))
}

# -------------------------------------------------------------------
# 3. 95% 신뢰구간 (Confidence Interval)
# -------------------------------------------------------------------
# 베이지안의 '신용구간(Credible Interval)'에 대응되는 개념입니다.
# binom.test 함수를 사용해 각 그룹별 CI를 구합니다.

cat("\n[95% 신뢰구간]\n")
for(i in 1:3) {
  test_res <- binom.test(successes[i], totals[i])
  cat(sprintf("%s: [%.3f, %.3f]\n", 
              groups[i], test_res$conf.int[1], test_res$conf.int[2]))
}

# -------------------------------------------------------------------
# 4. 
# -------------------------------------------------------------------
# 귀무가설(H0): 세 치료법의 성공률은 모두 같다. (p1 = p2 = p3)
# 대립가설(H1): 적어도 하나의 치료법은 성공률이 다르다.

# (1) 카이제곱 검정 (Chi-squared Test)
chisq_res <- prop.test(successes, totals)
cat("\n[전체 비교 - 카이제곱 검정]\n")
print(chisq_res)

# 해석: p-value가 0.05보다 크면 "통계적으로 유의한 차이가 없다"고 결론.

# (2) 사후 검정 (Pairwise Comparison)
# 만약 차이가 있다면, 구체적으로 누가 다른지 확인 (수술 vs IMRT 등)
# 다중 비교(Multiple Comparison) 문제를 보정하기 위해 'bonferroni' 등을 사용
pairwise_res <- pairwise.prop.test(successes, totals, p.adjust.method = "bonferroni")
cat("\n[짝지어 비교하기 (Pairwise Comparison)]\n")
print(pairwise_res)

# -------------------------------------------------------------------
# 5. 로지스틱 회귀 (GLM) - 선생님께서 언급하신 'Formula' 방식
# -------------------------------------------------------------------
# 빈도주의에서 회귀분석을 하려면 glm() 함수를 씁니다.
# 데이터셋을 Long Format으로 만들어야 합니다.

# 데이터 변환 (0: 실패, 1: 성공)
long_data <- data.frame(
  Treatment = factor(rep(groups, each=50), levels=groups),
  Result = c(
    rep(1, 42), rep(0, 8),  # Surgery
    rep(1, 40), rep(0, 10), # IMRT
    rep(1, 38), rep(0, 12)  # Proton
  )
)

# GLM 적합 (MLE 사용)
# Formula: 결과 ~ 치료법
fit_glm <- glm(Result ~ Treatment, data = long_data, family = binomial)

cat("\n[로지스틱 회귀분석 결과 (GLM with MLE)]\n")
summary(fit_glm)

# 해석: 
# (Intercept)는 기준 그룹(Surgery)의 로그 오즈
# TreatmentIMRT는 (IMRT - Surgery)의 차이
# Pr(>|z|) (p-value)가 0.05보다 작아야 유의미한 차이.

