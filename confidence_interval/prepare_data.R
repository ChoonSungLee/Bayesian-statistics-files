# ==========================================================
# 파일명: prepare_data.R
# 역할: 분석에 사용할 원본 데이터 파일들을 'data' 폴더로 복사 (최초 1회 실행)
# ==========================================================

# 1. 'here' 패키지 설치 및 로드
if (!require("here")) install.packages("here")
library(here)

# 2. 데이터를 저장할 'data' 폴더 생성
# here::here("data")는 프로젝트 최상위 폴더 기준 'data' 폴더 경로를 의미
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"))
}

# 3. 원본 데이터 파일 경로 지정
# (프로젝트 최상위 폴더에 파일이 있다고 가정)
source_csv_file <- here::here("example1.csv")
source_jags_file <- here::here("example1.jags")

# 4. 복사할 대상 폴더 경로 지정
target_csv_file <- here::here("data", "example1.csv")
target_jags_file <- here::here("data", "example1.jags")

# 5. 파일 복사 실행
# (단, 대상 폴더에 파일이 없을 경우에만 복사)
if (file.exists(source_csv_file) && !file.exists(target_csv_file)) {
  file.copy(from = source_csv_file, to = target_csv_file)
  print(paste("파일 복사 완료:", target_csv_file))
} else {
  print(paste("파일이 이미 있거나 원본이 없습니다:", source_csv_file))
}

if (file.exists(source_jags_file) && !file.exists(target_jags_file)) {
  file.copy(from = source_jags_file, to = target_jags_file)
  print(paste("파일 복사 완료:", target_jags_file))
} else {
  print(paste("파일이 이미 있거나 원본이 없습니다:", source_jags_file))
}

print(paste("데이터 준비가 완료되었습니다. 'data' 폴더를 확인하세요."))
