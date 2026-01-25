# 파일명: create_data.R
# 역할: 분석에 사용할 데이터를 생성하고 저장 (최초 1회 실행)

# 'here' 패키지 설치 및 로드
if (!require("here")) install.packages("here")
library(here)

# 데이터 생성
set.seed(123)
height <- rnorm(100, mean = 170, sd = 10)
weight <- 0.5 * height + rnorm(100, mean = 0, sd = 5)
lm_data <- data.frame(height, weight)

# 데이터를 저장할 'data' 폴더 생성
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

# here()를 사용해 Rdata 파일로 저장
save(lm_data, file = here("data", "lm_data.Rdata"))

print(paste("데이터 파일이 '", here("data", "lm_data.Rdata"), "' 경로에 저장되었습니다.", sep=""))
