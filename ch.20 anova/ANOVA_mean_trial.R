# 본문 설명 중  F=4 일 때 p 값 구하는 코드
F_value <- 4               # F 통계량
df1 <- 2                   # 분자 자유도 (그룹 간 자유도)
df2 <- 10                  # 분모 자유도 (오차 자유도)

# p-값 계산 (우측 꼬리 확률)
p_value <- 1 - pf(F_value, df1, df2)

# 결과 출력
cat("F =", F_value, "에서 p-값은:", p_value, "\n")



# 다음은 빈도주의 코드이다. 
# 데이터 생성
scores <- c(85, 90, 88, 78, 82, 80, 92, 95, 94)
methods <- factor(rep(c("A", "B", "C"), each = 3))

# 데이터 프레임 생성
data <- data.frame(Method = methods, Score = scores)

# 데이터 확인
print(data)

# 일원분산분석 (One-Way ANOVA)
anova_result <- aov(Score ~ Method, data = data)

# ANOVA 결과 출력
summary(anova_result)

# 사후 분석 (Tukey's HSD Test)
tukey_result <- TukeyHSD(anova_result)

# 사후 분석 결과 출력
print(tukey_result)

# 사후 분석 결과 시각화
plot(tukey_result)



# 다음은 베이즈 코드이다. 
# 필요한 패키지 설치 및 로드
if (!requireNamespace("brms", quietly = TRUE)) install.packages("brms")
library(brms)
if (!requireNamespace("arm", quietly = TRUE)) install.packages("arm")

# 데이터 생성 (길이 맞춤)
scores <- c(85, 90, 88, 78, 82, 80, 92, 95, 94)  # 9개의 값
methods <- factor(rep(c("A", "B", "C"), each = 3))  # 9개의 그룹 레이블
data <- data.frame(Method = methods, Score = scores)

# 데이터 확인
print(data)

# 베이즈 일원분산분석 (Bayesian One-Way ANOVA)
bayes_model <- brm(
  Score ~ Method,           # 반응 변수 ~ 독립 변수
  data = data,              # 데이터
  family = gaussian(),      # 정규분포 가정
  prior = c(
    prior(normal(0, 10), class = "b"),         # 각 그룹 효과에 대한 사전 분포
    prior(cauchy(0, 5), class = "sigma")      # 잔차(오차)에 대한 사전 분포
  ),
  iter = 2000,              # 샘플 반복 수
  chains = 4,               # MCMC 체인 수
  cores = 4                 # 사용 CPU 코어 수
)

# 결과 확인
summary(bayes_model)

# 사후 분석 (베이즈 다중비교, emmeans 활용)
if (!requireNamespace("emmeans", quietly = TRUE)) install.packages("emmeans")
library(emmeans)

# 그룹 간 비교
posthoc <- emmeans(bayes_model, pairwise ~ Method)
summary(posthoc)

# --- 시각화 추가 코드 ---
if (!requireNamespace("bayesplot", quietly = TRUE)) 
  install.packages("bayesplot")
library(bayesplot)
library(ggplot2)

# 1. 각 그룹(A, B, C)에 대한 사후 평균 추출
# 새로운 데이터 프레임 생성 (Method A, B, C)
new_data <- data.frame(Method = c("A", "B", "C"))

# fitted() 함수를 사용하여 각 그룹의 평균에 대한 MCMC 샘플을 추출
# summary = FALSE로 설정해야 요약값이 아닌 전체 샘플을 가져옴
post_samples <- fitted(bayes_model, newdata = new_data, summary = FALSE)

# 그래프용 이름 설정
colnames(post_samples) <- c("Group A", "Group B", "Group C")

# 2. 산 모양의 분포(Ridgeline plot) 그리기
# mcmc_areas 함수는 분포의 모양(높이)과 95% 신뢰구간(색칠된 부분)을 보여줌
plot_obj <- mcmc_areas(post_samples, 
                       prob = 0.95,        # 95% 구간을 진하게 표시
                       point_est = "mean", # 평균 지점에 선 표시
                       area_method = "equal height") + 
  ggtitle("Posterior Distributions by Group") +
  theme_minimal()

# 그래프 출력
print(plot_obj)

# 3. 파일로 저장 (figs 폴더가 있다고 가정)
# 원고에 삽입하기 위해 이미지 파일로 저장합니다.
ggsave("figs/bayes_posterior.png", plot = plot_obj, width = 6, height = 4, dpi = 300)







