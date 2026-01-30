library(tidyverse)
library(rstan)

n <- c(20, 20, 20, 20, 20, 20, 20, 19, 19, 19, 19, 18, 18, 
       17, 20, 20, 20, 20, 19, 19, 18, 18, 27, 25, 24, 23, 
       20, 20, 20, 20, 20, 20, 10, 49, 19, 46, 17, 49, 47, 
       20, 20, 13, 48, 50, 20, 20, 20, 20, 20, 20, 20, 48, 
       19, 19, 19, 22, 46, 49, 20, 20, 23, 19, 22, 20, 20, 
       20, 52, 46, 47, 24, 14)
x <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 
       1, 1, 1, 1, 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 5, 
       2, 5, 2, 7, 7, 3, 3, 2, 9, 10, 4, 4, 4, 4, 4, 4, 4, 
       10, 4, 4, 4, 5, 11, 12, 5, 5, 6, 5, 6, 6, 6, 6, 16, 
       15, 15, 9, 4)
rats <- data.frame(n = n, x = x)
sum(rats$x) #답으로 267이 나온다.
sum(rats$n) #답으로 1739가 나온다

# 첫번째 모델
prior_par = c(1,1) #여기서 par는 parameter의 약자이다. param으로 써도 된다. 
post_par1 = c(5, 11)

#그림을 그리는 두 가지 방법이 있다. 간단한 방법은 ggplot의 내장함수를 사용하는 것이고, 복잡한 방법은 직접 코드로 그리는 것이다. 

#credible interval을 그리는 방법의 코드가 아래 두 줄에 있다. 
#googling; qbeta(p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
upper_ci1 = qbeta(0.025, post_par1[1], post_par1[2], lower.tail=FALSE)
lower_ci1 = qbeta(0.025, post_par1[1], post_par1[2], lower.tail=TRUE)

#간단한 방법
ggplot(data.frame(theta=c(0,1)), aes(x=theta)) + #theta=c(0,1)은 x축이 0~1이라는 뜻이다. 
  stat_function(fun=dbeta, args=prior_par, aes(col='prior')) + #stat_function이라는 함수는 ggplot에서 'fun' 함수를 그려준다. 
  #이때 fun의 parameter를 args로 지정할 수 있다. 
  # 즉 이 코드는 ggplot 위에 parameter가 prio-par인 dbeta함수를 그려주는 것이다. dbeta는 beta 분포의 pdf이다. 
  stat_function(fun=dbeta, args=post_par1, aes(col='posterior')) + 
  geom_vline(xintercept = upper_ci1, col='blue', lty=2) +
  geom_vline(xintercept = lower_ci1, col='blue', lty=2)



# 두번째 모델
prior_par = c(1,1)
post_par2 = c(1+sum(rats$x), 1+sum(rats$n)-sum(rats$x)) #sum(rats$x)=267, 1+sum(rats$n)-sum(rats$x)=1473

#간단한 방법 사용
upper_ci2 = qbeta(0.025, post_par2[1], post_par2[2], lower.tail=FALSE)
lower_ci2 = qbeta(0.025, post_par2[1], post_par2[2], lower.tail=TRUE)

ggplot(data.frame(theta=c(0,1)), aes(x=theta)) +  
  stat_function(fun=dbeta, args=prior_par, aes(col='prior')) +  
  
  stat_function(fun=dbeta, args=post_par2, aes(col='posterior'), n=1000) + #n=1000을 추가하면 곡선이 부드러워진다. 
  geom_vline(xintercept = upper_ci2, col='blue', lty=2) +
  geom_vline(xintercept = lower_ci2, col='blue', lty=2)


# qq plot
pbar <- sum(rats$x) / sum(rats$n)
mutate(rats, p = (x+0.5) / (n+1)) %>%  #n이 작을 때 근사의 정확도를 높이기 위해
  #mutate 함수는 data frame에 열을 추가한다. 
  transmute(z = (p - pbar)/sqrt(p * (1-p) / n)) %>% #transmute는 mutate와 같은 기능을 하는데 열만 출력해준다. 
  ggplot(aes(sample = z)) +
  geom_qq() + #geom_qq-qqplot
  geom_qq_line(col = "red", linetype = 2) + #geom_qq_line; 이상적인 상황에서의 직선을 추가해준다. 
  ylab("z")


#세번째 모델
hyper_est <- mutate(rats, p = (x+0.5) / (n+1)) %>% #hyper-estimation
  summarise(mean_p = mean(p), var_p = var(p)) #열에 함수를 적용하는 함수, p열에 mean과 variance를 적용,현재 열은 x, n, p의 세개가 있다. 
hyper_est

true_val <- as.numeric(hyper_est)
fn = function(hyper) {
  m = hyper[1] / (hyper[1] + hyper[2])
  v = hyper[1] * hyper[2] / ((hyper[1]+hyper[2])^2 * (hyper[1]+hyper[2]+1))
  return(crossprod(c(m, v) - true_val))
}

soln <- optim(c(1, 1), fn)
soln
alpha_est <- soln$par[1]
beta_est <- soln$par[2]

# 세번째 모델의 사후분포
prior_par = c(alpha_est,beta_est)
post_par3 = prior_par + c(rats$x[71], rats$n[71]-rats$x[71])
#첫번째 방법과 동일하나, prior에 기존의 정보를 반영

#간단한 방법 사용
upper_ci3 = qbeta(0.025, post_par3[1], post_par3[2], lower.tail=FALSE)
lower_ci3 = qbeta(0.025, post_par3[1], post_par3[2], lower.tail=TRUE)

ggplot(data.frame(theta=c(0,1)), aes(x=theta)) +  
  stat_function(fun=dbeta, args=prior_par, aes(col='prior')) +  
  
  stat_function(fun=dbeta, args=post_par3, aes(col='posterior'), n=1000) + #n=1000을 추가하면 곡선이 부드러워진다. 
  geom_vline(xintercept = upper_ci3, col='blue', lty=2) +
  geom_vline(xintercept = lower_ci3, col='blue', lty=2)



# 네번째 모델
# stan model code
rats_model_code <- "
data {
  int N;
  int x[N];
  int n[N];
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0, upper=1> theta[N];
}
model {
  for (i in 1:N) {
    x[i] ~ binomial(n[i], theta[i]);
    theta[i] ~ beta(alpha, beta);
  }
  
  alpha ~ gamma(0.001, 0.001);
  beta ~ gamma(0.001, 0.001);
}
"

# stan model code를 이용한 fitting
rats_model <- stan(model_code=rats_model_code,
                   data=list(
                     N=length(rats$x),
                     n=rats$n,
                     x=rats$x
                   ),
                   chains = 4,
                   cores = 4)


# stan file을 이용한 fitting
rats_model <- stan(file="hierarchical_rat_model.stan",
                   data=list(
                     n=length(rats$x),
                     N=rats$n,
                     X=rats$x
                   ),
                   chains = 4,
                   cores = 4)


# fitting 결과
print(rats_model)


# 다음과 같이 사후밀도함수, 시계열 그림, 자기상관계수 그림을 그릴 수 있다.
# 이 그림은 세 개의 모수 $\theta_{71},~ \alpha,~ \beta$의 사후밀도함수의 그림을 보여 준다. 각 모수들의 사후 밀도 함수의 모양과, 사후 확률이 어디에 모여 있는지 보여준다. $\theta_{71}$ 은 대략 0에서 0.5사이에 거의 모든 사후 확률이 분포되어 있음을 보여 준다.
plot(rats_model, plotfun = "dens", pars = c("theta[71]", "alpha", "beta"))


# 이 그림은 각 모수의 마르코프 체인의 시계열 그림을 보여 준다. 시계열 그림에 뚜렷한 경향성이 보이지 않으므로 마르코프 체인의 수렴에는 문제가 없다고 보여진다.
plot(rats_model, plotfun = "trace", pars = c("theta[71]", "alpha", "beta"))


# 이 그림은 각 모수의 자기 상관 계수 그림을 보여준다. 세 개의 모수 모두 시차가 6 이상이 되면 자기 상관계수가 거의 0에 가까워진다는 것을 보여준다. 이 역시 마르코프 체인의 수렴에는 문제가 없다는 것을 보여준다.
plot(rats_model, plotfun = "ac", pars = c("theta[71]", "alpha", "beta"))



# 결론;
# 계층 모형을 사용하지 않으면 쥐의 종양 자료 분석에서 봤듯이 71개의 데이터를 너무 적은 수의 모수로 표현하거나(두 번째 모형) 혹은 너무 많은 수의 모수로 표현하여(첫 번째 모형) 예측력이 떨어지게 된다. 계층 모형은 많은 수의 모수를 쓰지만 이 모수들이 하나의 분포를 따른다는 가정으로 모수의 자유도를 제한한다.
# 계층 모형은 주변 그룹의 정보를 이용(borrowing information from the neighbors)하여 보다 정확한 추정을 하는 장점이 있다. 한 그룹의 자료만으로 그 그룹의 모수를 추정하는 것이 아니라 비슷한 주변 그룹의 정보도 이용하여 그룹의 모수들을 추정한다.
# 계층 모형은 다층 모형(multilevel model)이라 불리기도 한다. 계층 모형에서 초모수를 추정해서 대입한 후에 분석하는 방법을 경험적 베이즈 모형(empirical Bayes method)이라 한다. 계층 모형이 표본 조사에서 사용되면 소지역 추정(small area estimation)이라 하기도 한다.






















