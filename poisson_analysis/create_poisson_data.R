# 파일명: create_poisson_data.R
# 역할: 분석에 사용할 데이터를 생성하고 저장합니다. (프로젝트에서 최초 1회만 실행)

# 'here' 패키지가 없으면 설치하고 로드합니다.
if (!require("here")) install.packages("here")
library(here)

# 1. 재현성을 위해 시드(seed) 설정
set.seed(123)

# 2. 포아송 데이터 생성
N <- 100
lambda_true <- 5
y <- rpois(N, lambda_true)
hist(y)
y

# 3. 데이터를 데이터프레임(data.frame) 형태로 변환
df_pois <- data.frame(y)

# 4. 데이터를 저장할 'data' 폴더가 없으면 생성
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

# 5. here()와 saveRDS()를 사용해 R 객체를 파일로 저장
# saveRDS()는 단일 R 객체를 저장하는 데 권장되는 방식입니다.
saveRDS(df_pois, file = here("data", "df_pois.rds"))

# 6. 저장 완료 메시지를 콘솔에 출력
print(paste("데이터 파일이 '", here("data", "df_pois.rds"), "' 경로에 저장되었습니다.", sep=""))

# 7. 생성된 데이터의 처음 6행을 확인
print("생성된 데이터 샘플:")
print(head(df_pois))

# 8. 참고: 나중에 다른 스크립트에서 이 데이터를 불러올 때는 아래와 같이 사용합니다.
# my_poisson_data <- readRDS(here("data", "df_pois.rds"))
