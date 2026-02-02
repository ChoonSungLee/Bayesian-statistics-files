if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# -----------------------------------------------------------
# 1. 데이터 생성 (두 개의 가우시안 분포에서 데이터 시뮬레이션)
# -----------------------------------------------------------
set.seed(123) # 재현성을 위해 시드 설정

# 실제 파라미터 (우리가 추정하고자 하는 값)
true_pi <- c(0.4, 0.6) # 혼합 가중치
true_mu <- c(2, 8)     # 평균
true_sigma <- c(1, 1)  # 표준편차 (분산은 sigma^2)

# 데이터 포인트 개수
n_points <- 500

# 데이터 생성
data <- c(rnorm(n_points * true_pi[1], mean = true_mu[1], sd = true_sigma[1]),
          rnorm(n_points * true_pi[2], mean = true_mu[2], sd = true_sigma[2]))

# 데이터 분포 시각화
hist(data, breaks = 30, freq = FALSE,
     main = "Generated Data Distribution (Gaussian Mixture)",
     xlab = "Value", ylab = "Density")
lines(density(data), col = "blue", lwd = 2)


# -----------------------------------------------------------
# 2. EM 알고리즘 구현 함수
# -----------------------------------------------------------

# GMM EM 알고리즘 함수
# data: 관측 데이터 (1차원)
# K: 클러스터(가우시안 분포) 개수
# max_iter: 최대 반복 횟수
# tol: 수렴 임계값

gmm_em <- function(data, K = 2, max_iter = 100, tol = 1e-6) {
  
  n <- length(data)
  
  # 1. 파라미터 초기화 (랜덤 초기화)
  # 초기 혼합 가중치 (pi)
  pi_k <- rep(1/K, K)
  # 초기 평균 (mu) - 데이터 범위 내에서 랜덤하게 선택
  mu_k <- sample(data, K)
  # 초기 분산 (sigma_sq) - 전체 데이터 분산으로 초기화하거나 작게 초기화
  sigma_sq_k <- rep(var(data) / K, K) # 또는 rep(1, K) 등으로 작게 초기화
  
  # 수렴을 위한 로그 우도 저장
  log_likelihood_history <- numeric(max_iter)
  elbo_history <- numeric(max_iter) # ELBO 값도 저장
  
  for (iter in 1:max_iter) {
    
    # === E-STEP (Expectation Step) ===
    # q(z) = p(z | x, theta_t) 계산
    # 각 데이터 포인트 i가 각 클러스터 k에 속할 사후 확률 (responsibility) 계산
    
    # responsibility (gamma_ik) = p(z_ik=1 | x_i, theta_t)
    # p(x_i | z_ik=1, theta_t) * p(z_ik=1) / sum_j (p(x_i | z_ij=1, theta_t) * p(z_ij=1))
    
    # 1. p(x_i | z_ik=1, theta_t) 즉, 각 가우시안 분포의 PDF 값 계산
    # (n x K) 행렬
    cluster_densities <- matrix(0, nrow = n, ncol = K)
    for (k in 1:K) {
      cluster_densities[, k] <- dnorm(data, mean = mu_k[k], sd = sqrt(sigma_sq_k[k]))
    }
    
    # 2. 분자: p(x_i | z_ik=1, theta_t) * p(z_ik=1) = density * pi_k
    weighted_densities <- sweep(cluster_densities, 2, pi_k, "*")
    
    # 3. 분모: sum_j (p(x_i | z_ij=1, theta_t) * p(z_ij=1))
    # 각 데이터 포인트에 대한 전체 주변 확률 p(x_i)
    marginal_likelihood_per_point <- rowSums(weighted_densities)
    
    # NaN 방지 (매우 작은 값으로 인해 0이 되는 경우)
    marginal_likelihood_per_point[marginal_likelihood_per_point == 0] <- 1e-300
    
    # 4. responsibility (gamma_ik) 계산
    # (n x K) 행렬
    gamma_ik <- sweep(weighted_densities, 1, marginal_likelihood_per_point, "/")
    
    # ELBO 계산 (E-step 완료 후)
    # ELBO = sum_i sum_k gamma_ik * log(pi_k * N(x_i | mu_k, sigma_k^2) / gamma_ik)
    # 이는 sum_i sum_k gamma_ik * log(pi_k) + sum_i sum_k gamma_ik * log(N(x_i | mu_k, sigma_k^2)) - sum_i sum_k gamma_ik * log(gamma_ik)
    
    # N(x_i | mu_k, sigma_k^2)는 cluster_densities
    # pi_k는 pi_k
    
    # 로그 우도 계산 (ELBO의 가장 간단한 형태)
    current_log_likelihood <- sum(log(marginal_likelihood_per_point))
    log_likelihood_history[iter] <- current_log_likelihood
    
    # ELBO의 각 항을 계산 (시각화 목적)
    # 항 1: E_q[log p(x,z|theta)] = sum_i sum_k gamma_ik * log p(x_i, z_k | theta_k)
    # log p(x_i, z_k | theta_k) = log(pi_k) + log(dnorm(x_i | mu_k, sigma_k^2))
    term1 <- 0
    for(k in 1:K){
      term1 <- term1 + sum(gamma_ik[,k] * (log(pi_k[k]) + dnorm(data, mean = mu_k[k], sd = sqrt(sigma_sq_k[k]), log = TRUE)))
    }
    
    # 항 2: - E_q[log q(z)] = - sum_i sum_k gamma_ik * log(gamma_ik)
    # log(gamma_ik)는 gamma_ik가 0일 때 -Inf가 되므로 처리 필요 (0 * -Inf = 0)
    term2 <- 0
    gamma_log_gamma <- gamma_ik * log(gamma_ik)
    gamma_log_gamma[is.na(gamma_log_gamma)] <- 0 # NaN (0 * log(0)) 처리
    term2 <- -sum(gamma_log_gamma)
    
    current_elbo <- term1 + term2
    elbo_history[iter] <- current_elbo
    
    
    # === M-STEP (Maximization Step) ===
    # theta_(t+1) = argmax_theta Q(theta, theta_t)
    
    # 1. 새로운 혼합 가중치 (pi_k) 업데이트
    # N_k = sum_i gamma_ik (각 클러스터에 할당된 효과적인 데이터 포인트 수)
    N_k <- colSums(gamma_ik)
    pi_k <- N_k / n
    
    # 2. 새로운 평균 (mu_k) 업데이트
    mu_k <- colSums(gamma_ik * data) / N_k
    
    # 3. 새로운 분산 (sigma_sq_k) 업데이트
    for (k in 1:K) {
      sigma_sq_k[k] <- sum(gamma_ik[, k] * (data - mu_k[k])^2) / N_k[k]
    }
    
    # 수렴 확인 (로그 우도 변화량이 임계값보다 작으면 중단)
    if (iter > 1 && abs(log_likelihood_history[iter] - log_likelihood_history[iter-1]) < tol) {
      cat(sprintf("Converged at iteration %d\n", iter))
      log_likelihood_history <- log_likelihood_history[1:iter]
      elbo_history <- elbo_history[1:iter]
      break
    }
  }
  
  return(list(pi = pi_k, mu = mu_k, sigma_sq = sigma_sq_k,
              log_likelihood_history = log_likelihood_history,
              elbo_history = elbo_history,
              gamma_ik = gamma_ik))
}

# -----------------------------------------------------------
# 3. EM 알고리즘 실행 및 결과 시각화
# -----------------------------------------------------------

# EM 알고리즘 실행
results <- gmm_em(data, K = 2)

cat("\n--- EM Algorithm Results ---\n")
cat("Estimated Pi (mixture weights): ", round(results$pi, 3), "\n")
cat("Estimated Mu (means): ", round(results$mu, 3), "\n")
cat("Estimated Sigma^2 (variances): ", round(results$sigma_sq, 3), "\n")
cat("Final Log-Likelihood: ", round(tail(results$log_likelihood_history, 1), 3), "\n")
cat("Final ELBO: ", round(tail(results$elbo_history, 1), 3), "\n")


# ELBO와 Log-Likelihood 변화 시각화
plot_df <- data.frame(
  Iteration = 1:length(results$elbo_history),
  ELBO = results$elbo_history,
  LogLikelihood = results$log_likelihood_history
)

ggplot(plot_df, aes(x = Iteration)) +
  geom_line(aes(y = ELBO, color = "ELBO"), size = 1.2) +
  geom_line(aes(y = LogLikelihood, color = "Log-Likelihood"), linetype = "dashed", size = 1.2) +
  scale_color_manual(values = c("ELBO" = "darkgreen", "Log-Likelihood" = "darkred")) +
  labs(title = "ELBO and Log-Likelihood Convergence in EM",
       y = "Value", color = "Metric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
EM
