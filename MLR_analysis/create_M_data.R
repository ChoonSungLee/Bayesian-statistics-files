# 파일명: create_data_M.R
# 역할: 분석에 사용할 데이터를 생성하고 저장 (최초 1회 실행)

# 'here' 패키지 설치 및 로드
if (!require("here")) install.packages("here")
library(here)

# 1. 데이터 생성
set.seed(123)
n <- 100
x1 <- rnorm(n, mean = 10, sd = 2)  # 독립변수 1
x2 <- rnorm(n, mean = 20, sd = 5)  # 독립변수 2
y <- 3 + 2 * x1 - 1.5 * x2 + rnorm(n, mean = 0, sd = 3)  # 종속변수

# 2. 생성된 변수들을 하나의 데이터프레임으로 묶기
# 이 데이터프레임의 이름을 'mlr_data'로 지정합니다.
mlr_data <- data.frame(y, x1, x2)

# 3. 데이터를 저장할 'data' 폴더 생성
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

# 4. here()를 사용해 'mlr_data' 객체를 Rdata 파일로 저장
save(mlr_data, file = here("data", "mlr_data.Rdata"))

# 5. 확인 메시지 출력
print(paste("데이터 파일이 '", here("data", "mlr_data.Rdata"), "' 경로에 저장되었습니다.", sep=""))

# 6. 생성된 데이터 샘플 확인
head(mlr_data)
