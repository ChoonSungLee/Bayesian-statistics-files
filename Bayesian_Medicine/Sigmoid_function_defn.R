# 시그모이드 함수 정의
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

# 테스트
print(paste("x가 -5일 때:", round(sigmoid(-5), 4))) # 0에 가까움
print(paste("x가  0일 때:", round(sigmoid(0), 4)))  # 딱 0.5
print(paste("x가 +5일 때:", round(sigmoid(5), 4)))  # 1에 가까움
