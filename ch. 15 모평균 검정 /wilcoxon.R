
# 데이터 생성
set.seed(123)

# A반: 정규분포
group_A <- rnorm(30, mean = 70, sd = 10)

# B반: 이상치 포함한 비정규분포
group_B <- c(rnorm(25, mean = 75, sd = 10), 100, 100, 100, 100, 100)

# 그룹 확인
boxplot(group_A, group_B, names = c("A반", "B반"),
        main = "두 그룹 점수 분포", col = c("skyblue", "lightpink"))

# 정규성 테스트 (Shapiro-Wilk)
shapiro.test(group_A)  # 정규분포일 가능성 높음
shapiro.test(group_B)  # 정규분포 아님 (p-value 낮음)

# --- Levene 검정 수행 (var.test 대체) ---

# 1. Levene 검정에 필요한 'car' 패키지를 설치
if (!require("car")) install.packages("car")
library(car)

# 2. leveneTest() 함수에 맞게 데이터를 재구성
#    두 그룹을 하나의 열로 합치고, 그룹을 구별하는 열을 새로 만듬.
values <- c(group_A, group_B)
groups <- factor(c(rep("A반", length(group_A)), rep("B반", length(group_B))))
my_data <- data.frame(values, groups)

# 3. Levene 검정을 수행.
#    center = median 옵션은 B반의 이상치(outlier)에 더 안정적인 결과를 제공.
leveneTest(values ~ groups, data = my_data, center = median)


# 📊 1. 모수 검정: t-test (정규성/등분산 가정 필요)
t.test(group_A, group_B, var.equal = TRUE)  # Student's t-test (등분산 가정)

t.test(group_A, group_B, var.equal = FALSE)  # Welch t-test (등분산 가정 X)

# 📊 2. 비모수 검정: Wilcoxon rank-sum test
wilcox.test(group_A, group_B)

# 결과 저장
t_result <- t.test(group_A, group_B, var.equal = FALSE)
w_result <- wilcox.test(group_A, group_B)

# 결과 비교 출력
cat("📌 Welch's t-test 결과:\n")
cat("t =", t_result$statistic, ", p-value =", t_result$p.value, "\n\n")

cat("📌 Wilcoxon rank-sum test 결과:\n")
cat("W =", w_result$statistic, ", p-value =", w_result$p.value, "\n")
