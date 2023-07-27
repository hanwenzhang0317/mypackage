myprior <- function (N,p,Sigma.hat) {

  kappa.1 <- 1
  kappa.2 <- 100

  K = 1 + N*p

  A.prior = matrix(0,K,N)
  A.prior[2:(N+1),] = diag(N)
  V.prior = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
  S.prior = diag(diag(Sigma.hat))
  nu.prior = N+1

  return (list(A.prior=A.prior,
               V.prior = V.prior,
               S.prior = S.prior,
               nu.prior = nu.prior))
}
