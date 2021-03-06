
# @title Estimate mixture proportions of a mixture model by EM algorithm
#
# @description Given the individual component likelihoods for a mixture model, 
# estimates the mixture proportions by an EM algorithm.
#
# @details Fits a k component mixture model \deqn{f(x|\pi)= \sum_k \pi_k f_k(x)} 
# to independent
# and identically distributed data \eqn{x_1,\dots,x_n}.
# Estimates mixture proportions \eqn{\pi} by maximum likelihood, or by maximum a
# posteriori (MAP) estimation for a Dirichlet prior on \eqn{\pi}
# (if a prior is specified).  Uses the SQUAREM package to accelerate convergence
# of EM. Used by the ash main function; there is no need for a user to call this
# function separately, but it is exported for convenience.
#
#
# @param matrix_lik, a n by k matrix with (j,k)th element equal to \eqn{f_k(x_j)}.
# @param prior, a k vector of the parameters of the Dirichlet prior on 
# \eqn{\pi}. Recommended to be rep(1,k)
# @param pi_init, the initial value of \eqn{\pi} to use. If not specified 
#  defaults to (1/k,...,1/k).
# @param control A list of control parameters for the SQUAREM algorithm, 
# default value is set to be control.default=list(K = 1, method=3, square=TRUE,
# step.min0=1, step.max0=1, mstep=4, kr=1, objfn.inc=1,tol=1.e-07, maxiter=5000,
# trace=FALSE).
#
# @return A list, including the estimates (pihat), the log likelihood for 
# each interation (B)
# and a flag to indicate convergence
#
#' @import SQUAREM
mixEM = function(matrix_lik,prior,pi_init=NULL,control=list()){
  control = set_control_squarem(control,nrow(matrix_lik))
  k=dim(matrix_lik)[2]
  if(is.null(pi_init)){
    pi_init = rep(1/k,k)# Use as starting point for pi
  }
  res = SQUAREM::squarem(par=pi_init,fixptfn=fixpoint, 
                         objfn=negpenloglik,matrix_lik=matrix_lik, 
                         prior=prior, control=control)
  return(list(pihat = normalize(pmax(0,res$par)), B=res$value.objfn,
              niter = res$iter, converged=res$convergence, control=control))
}


normalize = function(x){return(x/sum(x))}

fixpoint = function(pi, matrix_lik, prior){
  pi = normalize(pmax(0,pi)) #avoid occasional problems with negative pis 
                             # due to rounding
  m  = t(pi * t(matrix_lik)) # matrix_lik is n by k; so this is also n by k
  m.rowsum = rowSums(m)
  classprob = m/m.rowsum #an n by k matrix
  pinew = normalize(colSums(classprob) + prior - 1)
  return(pinew)
}

negpenloglik = function(pi,matrix_lik,prior){return(-penloglik(pi,
                                                    matrix_lik,prior))}

penloglik = function(pi, matrix_lik, prior){
  pi = normalize(pmax(0,pi))
  m  = t(pi * t(matrix_lik)) # matrix_lik is n by k; so this is also n by k
  m.rowsum = rowSums(m)
  loglik = sum(log(m.rowsum))
  subset = (prior != 1.0)
  priordens = sum((prior-1)[subset]*log(pi[subset]))
  return(loglik+priordens)
}



# @title Estimate mixture proportions of a mixture model by EM algorithm
# (weighted version)
#
# @description Given the individual component likelihoods for a mixture model,
# and a set of weights, estimates the mixture proportions by an EM algorithm.
#
# @details Fits a k component mixture model \deqn{f(x|\pi)= \sum_k \pi_k f_k(x)}
# to independent and identically distributed data \eqn{x_1,\dots,x_n} 
# with weights \eqn{w_1,\dots,w_n}.
# Estimates mixture proportions \eqn{\pi} by maximum likelihood, or by 
# maximum a posteriori (MAP) estimation for a Dirichlet prior on \eqn{\pi}
# (if a prior is specified).  Here the log-likelihood for the weighted data 
# is defined as \eqn{l(\pi) = \sum_j w_j log f(x_j | \pi)}. Uses the SQUAREM 
# package to accelerate convergence of EM. Used by the ash main function; 
# there is no need for a user to call this
# function separately, but it is exported for convenience.
#
#
# @param matrix_lik, a n by k matrix with (j,k)th element equal to 
#                   \eqn{f_k(x_j)}.
# @param prior, a k vector of the parameters of the Dirichlet prior on 
#   \eqn{\pi}. Recommended to be rep(1,k)
# @param pi_init, the initial value of \eqn{\pi} to use. If not 
#   specified defaults to (1/k,...,1/k).
# @param weights, an n vector of weights
# @param control A list of control parameters for the SQUAREM algorithm,
#   default value is set to be control.default=list(K = 1, method=3, 
#   squarem=TRUE, step.min0=1, step.max0=1, mstep=4, kr=1, objfn.inc=1,
#   tol=1.e-07, maxiter=5000, trace=FALSE).
#
# @return A list, including the estimates (pihat), the log likelihood for each
#  interation (B)
# and a flag to indicate convergence
#
#' @import SQUAREM
w_mixEM = function(matrix_lik,prior, pi_init=NULL, weights=NULL,control=list()){
  control = set_control_squarem(control,nrow(matrix_lik))
  k=dim(matrix_lik)[2]
  if(is.null(pi_init)){
    pi_init = rep(1/k,k)# Use as starting point for pi
  }
  if(is.null(weights)){
    weights <- rep(1/dim(matrix_lik)[1], dim(matrix_lik)[1])
  }
  res = SQUAREM::squarem(par=pi_init,fixptfn=w_fixpoint, objfn=w_negpenloglik,
                         matrix_lik=matrix_lik, prior=prior, w=weights,
                         control=control)
  return(list(pihat = normalize(pmax(0,res$par)), B=res$value.objfn,
              niter = res$iter, converged=res$convergence, control=control))
}

w_fixpoint = function(pi, matrix_lik, prior, w){
  pi = normalize(pmax(0,pi)) #avoid occasional problems with negative pis due 
                             #to rounding
  m  = t(pi * t(matrix_lik)) # matrix_lik is n by k; so this is also n by k
  m.rowsum = rowSums(m)
  classprob = m/m.rowsum #an n by k matrix
  pinew = normalize(colSums(w*classprob) + prior - 1)
  return(pinew)
}

w_negpenloglik = function(pi,matrix_lik,prior, w)
    {return(-w_penloglik(pi,matrix_lik,prior,w))}

w_penloglik = function(pi, matrix_lik, prior, w){
  pi = normalize(pmax(0,pi))
  m  = t(pi * t(matrix_lik)) # matrix_lik is n by k; so this is also n by k
  m.rowsum = rowSums(m)
  loglik = sum(w*log(m.rowsum))
  subset = (prior != 1.0)
  priordens = sum((prior-1)[subset]*log(pi[subset]))
  return(loglik+priordens)
}

# sets up a default for squarem, and modifies it with other provided values
set_control_squarem=function(control,nobs){
  control.default=list(K = 1, method=3, square=TRUE, step.min0=1,
                       step.max0=1, mstep=4, kr=1, objfn.inc=1,tol=1.e-07,
                       maxiter=5000, trace=FALSE)
  if (nobs > 50000) control.default$trace = TRUE
  control.default$tol = min(0.1/nobs,1.e-7) # set default convergence criteria
  ##to be more stringent for larger samples
  namc=names(control)
  if (!all(namc %in% names(control.default)))
    stop("unknown names in control: ", 
         namc[!(namc %in% names(control.default))])
  control=utils::modifyList(control.default, control)
  return(control)
}




