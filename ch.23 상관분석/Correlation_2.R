# (1) 패키지 설치 & 로드
if(!requireNamespace("UsingR", quietly = TRUE)){
  install.packages("UsingR")
}
library(UsingR)

# (2) galton 데이터 불러오기
data(galton)

# (3) 구조 확인
str(galton)
# 'data.frame': 928 obs. of  2 variables:
#  $ child : num  61.7 61.7 61.7 ...
#  $ parent: num  70.5 68.5 65.5 ...

head(galton)  # 몇 개만 미리 보기

# 빈도주의 Pearson 상관분석
freq_result <- cor.test(galton$child, galton$parent, method="pearson")
freq_result

  

# 베이즈 상관분석
# (1) BayesFactor 패키지 설치 & 로드
if(!requireNamespace("BayesFactor", quietly = TRUE)){
  install.packages("BayesFactor")
}
library(BayesFactor)

# (2) 베이즈 상관 분석
bf_result <- correlationBF(x = galton$child, 
                           y = galton$parent,
                           rscale = "medium")  
bf_result
