# ==========================================================
# 회귀분석의 t-검정 (빈도주의 분석 스크립트)
# ==========================================================

# --- 1. 데이터 및 패키지 준비 ---

# 필요한 패키지 로드 (최초 1회 설치 필요)
if (!require("here")) install.packages("here")
library(here)

# here() 함수를 이용해 'data' 폴더에 있는 데이터 파일을 불러옵니다.
load(here("data", "lm_data.Rdata"))

# 불러온 데이터의 구조와 앞부분을 확인합니다.
print("--- 불러온 데이터 확인 ---")
head(lm_data)
dim(lm_data)
print("--------------------------")


# --- 2. 선형 회귀 모델 적합 및 결과 확인 ---

# lm() 함수를 사용하여 단순 선형회귀 모델을 적합합니다.
model <- lm(weight ~ height, data = lm_data)

# summary() 함수로 t-검정 결과를 포함한 상세 분석 결과를 확인합니다.
print("--- OLS 회귀분석 결과 ---")
summary(model)
print("--------------------------")


# --- 3. 모델 통계량 수동 계산 및 진단 (학습용) ---

# 예측값과 잔차를 데이터프레임에 추가
lm_data$predicted <- predict(model)
lm_data$residuals <- resid(model)

# 잔차 제곱합(RSS) 계산
RSS <- sum(lm_data$residuals^2)

# 평균제곱오차(MSE) 계산
n <- nrow(lm_data)
k <- length(coef(model)) # 파라미터 개수 (절편 포함, k=2)
mean_squared_error <- RSS / (n - k)

print("--- 수동 계산 결과 ---")
print(paste("Residual Sum of Squares (RSS):", round(RSS, 2)))
print(paste("Mean Squared Error (MSE):", round(mean_squared_error, 2)))
print("--------------------------")

# 잔차 패턴을 시각적으로 확인하기 위한 잔차도(Residual Plot)
plot(lm_data$predicted, lm_data$residuals,
     xlab = "Fitted values (예측값)", ylab = "Residuals (잔차)",
     main = "Residual Plot", pch = 16)
abline(h = 0, col = "red", lwd = 2)


# 운영체제에 따른 폰트 설정
# (macOS 사용자)
if (Sys.info()['sysname'] == 'Darwin') {
  par(family = "AppleGothic")
}
# (Windows 사용자)
# windowsFonts(malgun = windowsFont("맑은 고딕"))
# par(family = "malgun")


# 잔차 패턴을 시각적으로 확인하기 위한 잔차도(Residual Plot)
# 운영체제에 따른 폰트 설정
# (macOS 사용자)
if (Sys.info()['sysname'] == 'Darwin') {
  par(family = "AppleGothic")
}
# (Windows 사용자)
# windowsFonts(malgun = windowsFont("맑은 고딕"))
# par(family = "malgun")


# 잔차 패턴을 시각적으로 확인하기 위한 잔차도(Residual Plot)
plot(lm_data$predicted, lm_data$residuals,
     xlab = "Fitted values (예측값)", ylab = "Residuals (잔차)",
     main = "Residual Plot", pch = 16,
     cex.lab = 1.2, cex.main = 1.5) # 라벨과 제목 크기 조절

abline(h = 0, col = "red", lwd = 2)

# 폰트 설정을 원래대로 되돌리고 싶다면
# par(family = "sans")
