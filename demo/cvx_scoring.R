
##############  cvx application 1  ######################


# library(CVXR)
# p <- c(9.89900e-01, 9.99899e-03, 9.99899e-05, 9.99899e-07)
# p <- p/sum(p)
# q <- rep(1/4, 4)
# kldiv <- sum(p*log(p/q))
# epsilon <- 1e-30
# lambda <- Variable(length(p))
# objective <- Minimize(sum(abs(lambda-1)))
# constraint1 <- sum(p_norm(p - lambda*q, 1)) - epsilon < 0
# problem <- Problem(objective, constraints = list(constraint1, constraint2))
# result <- solve(problem)
# 
# phat <- result$getValue(lambda)*q
# phat <- phat/sum(phat)

library(Logolas)
data("N_Glycosyl_sequences")
eps <- 1e-20
pseudo_dat <- N_Glycosyl_sequences+eps
pseudo_dat2 <- apply(pseudo_dat, 2, function(x) return(x/sum(x)))

bg <- apply(pseudo_dat2, 1, median)
bg2 <- bg/sum(bg)

lambda_mat <- matrix(0, nrow(pseudo_dat2), ncol(pseudo_dat2))
phat_mat <- matrix(0, nrow(pseudo_dat2), ncol(pseudo_dat2))

for(m in 1:ncol(pseudo_dat2)){
  pp <- pseudo_dat2[,m]
  qq <- bg2
  kldiv <- sum(pp*log(pp/qq)) 
  epsilon <- 1e-04
  llambda <- Variable(length(pp))
  objective <- Minimize(sum(p_norm(llambda,1)))
  constraint1 <- sum(p*llambda) - kldiv - log_sum_exp(llambda + log(q)) + epsilon > 0
  problem <- Problem(objective, constraints = list(constraint1))
  result <- solve(problem, solver="SCS")
  phat <- exp(result$getValue(llambda))*q
  phat <- phat/sum(phat)
  lambda_mat[,m] <- result$getValue(llambda)
  phat_mat[,m] <- phat
}


round(lambda_mat, 2)




library(CVXR)
p <- rep(1/6, 6)
p <- p/sum(p)
q <- c(7.590720e-10, 3.232363e-67, 9.657371e-01, 1.141712e-39, 3.426289e-02, 5.617568e-45)
q <- q/sum(q)
kldiv <- sum(p*log(p/q)) 
epsilon <- 1e-04
llambda <- Variable(length(p))
objective <- Minimize(sum(p_norm(llambda,1)))
constraint1 <- sum(p*llambda) - kldiv - log_sum_exp(llambda + log(q)) + epsilon > 0
problem <- Problem(objective, constraints = list(constraint1))
result <- solve(problem)
phat <- exp(result$getValue(llambda))*q
phat <- phat/sum(phat)

