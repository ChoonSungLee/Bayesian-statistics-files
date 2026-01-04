# 재현성을 위한 시드 설정
set.seed(42)

# 데이터 생성
# A그룹: 위약 (평균 감소량 5, 표준편차 3)
group_A <- rnorm(10, mean = 5, sd = 3)

# B그룹: 신약 (평균 감소량 12, 표준편차 3)
group_B <- rnorm(10, mean = 12, sd = 3)

# --- Student's t-test 수행 ---
# var.equal = TRUE 옵션이 Student's t-검정을 의미합니다.
# (이 옵션이 없으면 R은 기본적으로 Welch's t-test를 수행합니다)
t_result <- t.test(group_A, group_B, var.equal = TRUE)

# 결과 출력
print(t_result)
