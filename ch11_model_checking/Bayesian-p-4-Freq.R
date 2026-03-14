# 유의성 검증이다. 
# 이항모델에서 p 값을 구한다. 

# 데이터
n <- 100           # 총 실험 대상 수
successes <- 65    # 성공한 사례 수
p_null <- 0.5      # 귀무가설 성공률

# 검정 통계량: 관측된 성공률
observed_proportion <- successes / n

# 이항 검정 수행
binom_test <- binom.test(successes, n, p = p_null, alternative = "two.sided")

# 결과 출력
print(binom_test)
