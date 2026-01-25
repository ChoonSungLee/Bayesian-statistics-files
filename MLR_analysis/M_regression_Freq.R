# ==========================================================
# 다중 회귀분석 (빈도주의 분석 스크립트)
# ==========================================================

# --- 1. 데이터 및 패키지 준비 ---

# 필요한 패키지 로드 (최초 1회 설치 필요)
if (!require("here")) install.packages("here")
if (!require("car")) install.packages("car") 
library(here)
library(car)    

# here() 함수를 이용해 'data' 폴더에 있는 데이터 파일을 불러옵니다.
load(here("data", "mlr_data.Rdata"))

# 불러온 데이터의 구조와 앞부분을 확인합니다.
print("--- 불러온 데이터 확인 ---")
head(mlr_data)
dim(mlr_data)
print("--------------------------")

# 다중 회귀모델 
model <- lm(y ~ x1 + x2)

# 모델 요약
summary_model <- summary(model)
print(summary_model)

# ANOVA 테이블
anova_table <- anova(model)
print("ANOVA Table:")
print(anova_table)

#정규성 가정
# Q-Q Plot
qqnorm(residuals(model))  # 잔차의 정규 Q-Q 플롯
qqline(residuals(model), col = "red")  # 기준선 추가

# Shapiro-Wilk Test
shapiro.test(residuals(model))

# Kolmogorov-Smirnov Test
ks.test(residuals(model), "pnorm", mean = mean(residuals(model)), sd = sd(residuals(model)))


#시각화-1
# x1과 y의 관계
plot(x1, y, main = "Scatter Plot of x1 vs y",
     xlab = "x1", ylab = "y")
abline(lm(y ~ x1), col = "blue")

# x2과 y의 관계
plot(x2, y, main = "Scatter Plot of x2 vs y",
     xlab = "x2", ylab = "y")
abline(lm(y ~ x2), col = "blue")

# 독립성 및 등분산성 가정을 시각적으로 확인하기 위한 잔차 플롯
plot(fitted(model), residuals(model),
     xlab = "Fitted values (예측값)", 
     ylab = "Residuals (잔차)",
     main = "Residual Plot",
     pch = 16) # 점 모양을 채워진 원으로 지정
# y=0 기준선 추가
abline(h = 0, col = "red", lwd = 2)

# 실제값 vs 예측값
plot(y, fitted(model),
     main = "Actual vs Predicted",
     xlab = "Actual Values", ylab = "Predicted Values")
abline(0, 1, col = "red")  # y = x 선 추가

# 다중 산점도
pairs(~ y + x1 + x2, main = "Scatterplot Matrix")

# 상관 관계
cor(x1, x2)

#시각화-2
plot(model)


# 다중공선성 
# VIF 확인

vif(model)






