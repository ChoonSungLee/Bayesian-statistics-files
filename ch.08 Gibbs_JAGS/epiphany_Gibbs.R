set.seed(123)  # 결과를 재현 가능하도록 설정
Y = c(14, 1, 1, 1, 5)  # 관측치
n = 22  # 관측치의 합
a = c(0.25, 0.25, 0.25, 0.25)  # a_1, a_2, a_3, a_4
b = c(0.125, 0, 0, 0.375)  # b_1, b_2, b_3, b_4
c = 0.5  # c
alpha = c(1, 1, 1)  # Dirichlet 사전분포의 파라미터
m = 1000; N = 3000

THETA= matrix(0, m+N, 3); X=c(1:9)*0

#Dirichlet generator
rDirichlet=function(n,p,a){
  x=matrix(0,n,p)
  for(i in 1:p) x[,i]=rgamma(n,a[i])
  x=x/apply(x,1,sum) #rowSums(x)
}

#initialize

theta= 0.5; eta= 0.5; THETA[1,]= c(theta, eta, 1-theta-eta)

X[1]= 0.5*Y[1]; X[3]= 0.5*Y[2]; X[5]= 0.5*Y[3]; X[7]= 0.5*Y[4]; X[9]= Y[5]


# Begin simulation
for (i in 2:(m + N)) {
  p = 3
  aa = c(X[1] + X[3] + alpha[1], X[5] + X[7] + alpha[2], X[9] + alpha[3])
  THETA[i, ] = rDirichlet(1, p, aa)
  theta = THETA[i, 1]
  eta = THETA[i, 2]
  
  # 수정 사항: X 값 업데이트
  X[1] = rbinom(1, Y[1], a[1] * theta / (a[1] * theta + b[1]))
  X[3] = rbinom(1, Y[2], a[2] * theta / (a[2] * theta + b[2]))
  X[5] = rbinom(1, Y[3], a[3] * eta / (a[3] * eta + b[3]))
  X[7] = rbinom(1, Y[4], a[4] * eta / (a[4] * eta + b[4]))
  X[9] = Y[5]  # X[9] 값 설정
}


theta.sim = THETA[(m + 1):(m + N), 1]
eta.sim = THETA[(m + 1):(m + N), 2]

# 결과 확인
print(head(theta.sim))
print(head(eta.sim))

# 시각화
par(mfrow=c(1,1))
plot(theta.sim, eta.sim, xlab=expression(theta), ylab=expression(eta))

par(mfrow=c(1,2))
plot(density(theta.sim), type="l", xlab=expression(theta), ylab="posterior", main=" ")
plot(density(eta.sim), type="l", xlab=expression(eta), main="")