# 0. 필요한 패키지 설치 및 로드
if (!require("lme4")) install.packages(c("lme4"))
if (!require("rstan")) install.packages(c("rstan"))
if (!require("rstanarm")) install.packages(c("rstanarm"))
if (!require("bayesplot")) install.packages(c("bayesplot"))
if (!require("ggplot2")) install.packages(c("ggplot2"))
if (!require("dplyr")) install.packages(c("dplyr"))

library(lme4) # 포뮬러 방식 (빈도주의)
library(rstan) # Stan을 통한 베이즈 모델
library(rstanarm) # Stan 기반의 포뮬러 방식 (베이즈)
library(bayesplot) # 베이즈 결과 시각화
library(ggplot2) # 일반 시각화
library(dplyr) # 데이터 전처리

# Stan 병렬 처리를 위한 옵션 설정 (선택 사항)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 1. 데이터 생성: 여러 학원의 학생 점수

set.seed(123) # 재현성을 위한 시드 설정

n_academies <- 10 # 학원 수
students_per_academy_min <- 10 # 최소 학생 수
students_per_academy_max <- 30 # 최대 학생 수

# 각 학원의 실제 평균 점수 (숨겨진 모수)
# 이 평균들은 전체 평균(70)을 중심으로 분포한다고 가정
academy_true_means <- rnorm(n_academies, mean = 70, sd = 5)

# 데이터를 저장할 빈 리스트
academy_data_list <- list()

for (i in 1:n_academies) {
  academy_name <- paste0("Academy_", LETTERS[i])
  n_students <- sample(students_per_academy_min:students_per_academy_max, 1) # 학원별 학생 수 다르게 설정
  
  # 각 학원의 실제 평균을 중심으로 학생들의 점수 생성
  # 점수는 0-100점 사이로 제한
  scores <- round(pmax(0, pmin(100, rnorm(n_students, mean = academy_true_means[i], sd = 8))))
  
  academy_data_list[[i]] <- data.frame(
    Academy = academy_name,
    Student_ID = 1:n_students,
    Score = scores
  )
}

# 모든 학원 데이터를 하나의 데이터프레임으로 결합
exam_data <- do.call(rbind, academy_data_list)

# 데이터 확인
head(exam_data)
tail(exam_data)
summary(exam_data)
table(exam_data$Academy) # 학원별 학생 수 확인
as.data.frame(table(exam_data$Academy))

# Stan 모델 파일 경로 (실제 파일 경로에 맞게 수정)
stan_file <- "academy_model.stan" # 따로 작성한 Stan 코드를 이 이름으로 저장하세요.

# 데이터를 Stan 모델에 전달하기 위한 리스트 생성
stan_data <- list(
  N = nrow(exam_data),
  J = n_academies,
  academy_id = as.integer(as.factor(exam_data$Academy)), # 학원 이름을 숫자로 변환
  score = exam_data$Score
)

# Stan 모델 적합
# 시간이 다소 소요될 수 있습니다. (chains, iter 조정 가능)
fit_stan <- stan(file = stan_file, data = stan_data,
                 chains = 4, iter = 2000, warmup = 1000, seed = 42)

# 결과 요약 확인
print(fit_stan, pars = c("mu", "sigma_academy", "sigma_student", "theta"),
      probs = c(0.025, 0.5, 0.975))

# 학원별 평균 점수 추정치 (theta) 확인
# 특히 데이터가 적은 학원의 추정치가 전체 평균(mu) 쪽으로 '수축'되었는지 확인
academy_names <- levels(as.factor(exam_data$Academy))
theta_summary <- summary(fit_stan, pars = "theta")$summary
theta_df <- data.frame(Academy = academy_names, Mean_Score = theta_summary[, "mean"],
                       Lower_CI = theta_summary[, "2.5%"], Upper_CI = theta_summary[, "97.5%"])
print(theta_df)

# 사후 분포 시각화
mcmc_dens(fit_stan, pars = c("mu", "sigma_academy", "sigma_student"), facet_args = list(scales = "free"))
mcmc_trace(fit_stan, pars = c("mu", "sigma_academy", "sigma_student"), facet_args = list(nrow = 3))

# 특정 학원들의 theta 사후분포 시각화
mcmc_dens(fit_stan, pars = c("theta[1]", "theta[2]", "theta[3]"), facet_args = list(scales = "free"))
mcmc_trace(fit_stan, pars = c("theta[1]", "theta[2]", "theta[3]"), facet_args = list(nrow = 3))

# (선택 사항) 학원별 추정 평균과 실제 평균 비교 (가상 데이터에서만 가능)
true_means_df <- data.frame(Academy = academy_names, True_Mean = academy_true_means)
merged_results <- left_join(theta_df, true_means_df, by = "Academy")
print(merged_results)


# rstanarm을 이용한 베이즈 계층 모델 적합
# lmer()와 유사한 포뮬러 문법 사용
fit_rstanarm <- stan_lmer(Score ~ (1 | Academy), data = exam_data,
                          chains = 4, iter = 2000, warmup = 1000, seed = 42)

# 결과 요약 확인
print(fit_rstanarm, digits = 3)

# 학원별 랜덤 절편 확인 (ranef 함수)
# 각 학원이 전체 평균에서 얼마나 벗어나는지 보여줍니다.
academy_intercepts <- ranef(fit_rstanarm)
print(academy_intercepts)

# 학원별 최종 추정 평균 점수 확인 (coef 함수)
# 각 학원의 (전역 절편 + 해당 학원의 랜덤 절편)
academy_mean_estimates <- coef(fit_rstanarm)$Academy
print(academy_mean_estimates)

# 사후 분포 시각화
plot(fit_rstanarm, plot_priors = FALSE, prob = 0.95) # 기본적인 사후 분포 플롯

# bayesplot을 사용한 특정 파라미터 시각화
mcmc_dens(fit_rstanarm, pars = c("(Intercept)", "Sigma[Academy:(Intercept),(Intercept)]", "sigma"), facet_args = list(scales = "free"))
mcmc_trace(fit_rstanarm, pars = c("(Intercept)", "Sigma[Academy:(Intercept),(Intercept)]", "sigma"), facet_args = list(nrow = 3))










