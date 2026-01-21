# ==========================================================
# 파일명: Frequentist_analysis.R
# 역할: '간단한 예제의 빈도주의 접근법' 메인 분석 스크립트
# ==========================================================

# --- 1. 필수 패키지 로드 ---
# (빈도주의 기본 통계는 R 기본 함수만 사용하므로 here만 로드합니다)
library(here)

# --- 2. 데이터 로드 ---
# here::here()를 사용해 'data' 폴더의 CSV 파일을 불러옵니다.
# (이 스크립트를 실행하기 전에 'prepare_data.R'가 1회 실행되었어야 함)
data <- read.csv(here::here("data", "example1.csv"), header = FALSE)


# --- 3. 빈도주의 분석 (텍스트 본문 내용) ---

# 컬럼 이름 지정 (텍스트 5행)
colnames(data) <- c("value")
print("> head(data)")
print(head(data))
cat("\n") # 줄바꿈

# 표본평균과 표본분산 계산
sample_mean <- mean(data$value)
sample_variance <- var(data$value)
cat("--- 표본 통계량 ---\n")
cat("sample_mean:", sample_mean, "\n")
cat("sample_variance:", sample_variance, "\n")
cat("\n") # 줄바꿈

# <모평균에 대한 신뢰구간>
n <- length(data$value)
confidence_level <- 0.95
alpha <- 1 - confidence_level

# t-임계값 계산
t_critical <- qt(1 - alpha / 2, df = n - 1)

sample_std <- sqrt(sample_variance)
standard_error <- sample_std / sqrt(n)

margin_of_error <- t_critical * standard_error
confidence_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)
cat("--- 모평균 95% 신뢰구간 ---\n")
cat("95% Confidence interval for the population mean:", confidence_interval, "\n")
cat("\n") # 줄바꿈

# <모분산의 신뢰구간>
chi2_lower <- qchisq(alpha / 2, df = n - 1)
chi2_upper <- qchisq(1 - alpha / 2, df = n - 1)

lower_bound <- (n - 1) * sample_variance / chi2_upper
upper_bound <- (n - 1) * sample_variance / chi2_lower
variance_confidence_interval <- c(lower_bound, upper_bound)
cat("--- 모분산 95% 신뢰구간 ---\n")
cat("95% confidence interval for the population variance:", variance_confidence_interval, "\n")
cat("\n") # 줄바꿈

print("===== 빈도주의 분석 완료 =====")

