#Revised code
# 1. 데이터 생성
# 기온(X)과 아이스크림 판매량(Y) 데이터를 벡터로 만든 후, 데이터 프레임으로 결합합니다.
temperature <- c(20, 22, 25, 28, 30)
ice_cream_sales <- c(30, 35, 45, 50, 55)
my_data <- data.frame(temperature, ice_cream_sales)

# 2. 데이터 시각화: 산점도 그리기 (macOS 폰트 설정)
# plot() 함수에 family = "AppleGothic"을 추가합니다.
# main: 제목, xlab/ylab: x/y축 라벨, pch: 점 모양, col: 점 색상
plot(my_data$temperature, my_data$ice_cream_sales,
     main = "기온과 아이스크림 판매량의 관계",
     xlab = "기온 (°C)",
     ylab = "아이스크림 판매량 (개)",
     pch = 19,
     col = "blue",
     family = "AppleGothic") # <-- 이 부분이 변경되었습니다.

# 3. 회귀선 추가 (이 부분은 Windows와 동일합니다)
abline(lm(ice_cream_sales ~ temperature, data = my_data), 
       col = "red",
       lwd = 2)

# 4. 상관분석 수행
#cor.test() 함수로 상관분석을 실행하고 결과를 확인합니다.
result <- cor.test(my_data$temperature, my_data$ice_cream_sales)
print(result)





#원래 코드
# 1. 데이터 생성
# 기온(X)과 아이스크림 판매량(Y) 데이터를 벡터로 만든 후, 데이터 프레임으로 결합합니다.
#temperature <- c(20, 22, 25, 28, 30)
#ice_cream_sales <- c(30, 35, 45, 50, 55)
#my_data <- data.frame(temperature, ice_cream_sales)

# 2. 데이터 시각화 (산점도)
# plot() 함수를 사용하여 산점도를 그립니다.
# main: 제목, xlab/ylab: x/y축 라벨, pch: 점 모양, col: 점 색상
#plot(my_data$temperature, my_data$ice_cream_sales,
#main = "기온과 아이스크림 판매량의 관계",
#xlab = "기온 (°C)",
#ylab = "아이스크림 판매량 (개)",
#pch = 19,
#col = "blue")

# 3. 상관분석 수행
# cor.test() 함수로 상관분석을 실행하고 결과를 확인합니다.
#result <- cor.test(my_data$temperature, my_data$ice_cream_sales)
#print(result)
