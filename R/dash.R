#' @title Dirichlet adaptive shrinkage of compositional data using dash
#'
#' @description Given a matrix of compositional counts data, with samples along
#' the rows and the categories of composition along columns, performs Bayesian
#' adaptive shrinkage of the compositions to produce refined composition probs.
#'
#' @details The dash function provides a number of ways to perform
#' Empirical Bayes shrinkage estimation on compositional data (counts).
#'
#' The inputs to dash is a matrix of compositional counts with samples along
#' rows and categories along columns. The method assumes that the compositional
#' counts data is generated from an underlying composition probability vector,
#' which follows a mixture of Dirichlet distributions centered at the
#' user defined mode (which defaults to means for all categories being equal).
#'
#' We assume that the component Dirichlet distributions in the mixture have varying
#' degrees of concentration, varying from Inf (which is same as saying a point mass
#' at the mode), and then from high to low values of concentration and even concentration
#' values less than 1, which would represent spikes at the corners of the simplex.
#'
#' The grades of memberships/ mixture proportions in different Dirichlet
#' components are estimated and post-hoc measures - posterior mean, posterior weights,
#' posterior center and corner probabilities etc are computed. The posterior mean
#' is considered as the shrunk compositional probability.
#'
#'
#' @param comp_data, a n by m matrix where n represents the sample and m
#'                   represents the category of composition.
#' @param concentration a vector of concentration scales for different Dirichlet
#'                   compositions. Defaults to NULL, in which case, we append
#'                   concentration values of Inf, 100, 50, 20, 10, 5, 2, 1, 0.5
#'                   and 0.1.
#' @param mode An user defined mode/mean for the Dirichlet components. Defaults
#'             to equal means for all components.
#' @param weight A list of three components which assigns weights to the center
#'             Dirichlet component (determined by defined position of concentrations).
#'             Defaults to center weight of 10 and null and corner weights of 1.
#' @param def_positions A list of three components which assigns concentration
#'                values to center, null and corner components of Dirichlet
#'                distributions. The default concentrations for center are Inf,
#'                null are 1 and corner as 0.5.
#' @param optmethod The method for performing optimization of the mixture
#'               proportions or grades of memberships for the different
#'               Dirichlet compositions. Can be either of "mixEM", "w_mixEM"
#'               or weighted mixEM and "mixIP" for interior point convex
#'               optimization.
#' @param sample_weights The weights of the samples for performing the optimization.
#'               Defaults to NULL, in which case the weight is same for each sample.
#' @param verbose if TRUE, outputs messages tracking progress of the method
#' @param fdr_bound The concentration bound used to detect false discoveries
#'                of deviations from the mode.
#' @param bf A boolean (TRUE/FALSE) variable denoting whether log bayes factor
#'           (with respect to category with smallest representation) is used in
#'           optimization or the loglikelihood. Defaults to FALSE.
#' @param pi_init An initial starting value for the mixture proportions. Defaults
#'                to same proportion for all categories.
#' @param control A list of control parameters for the SQUAREM/IP algorithm,
#'              default value is set to be control.default=list(K = 1, method=3,
#'               square=TRUE, step.min0=1, step.max0=1, mstep=4, kr=1,
#'               objfn.inc=1,tol=1.e-07, maxiter=5000, trace=FALSE).
#' @param reportcov A boolean indicating whether the user wants to return
#'                  the covariance and correlation structure of the posterior.
#'                  Defaults to FALSE.
#'
#' @return A list, including the following,
#'         \code{fitted_pi}: The fitted values of mixture proportions for Dirichlet components
#'         \code{concentration}: The concentration scales of the Dirichlet compositions
#'         \code{prior}: Prior strengths of Dirichlet components
#'         \code{posterior_weights}: Posterior weights of each sample on each category posterior component
#'         \code{posmean}: Posterior means of compositional probability from dash fit of each sample
#'         \code{datamean}: Original compositional probability of each sample
#'         \code{poscov}: Posterior covariance structure for each sample (if \code{reportcov} TRUE)
#'         \code{poscor}: Posterior correlation structure for each sample (if \code{reportcov} TRUE)
#'         \code{center_prob_local}: Posterior probability on Inf concentration Dirichlet component
#'         \code{center_prob}: Posterior probability on Dirichlet components with concentration less than \code{fdr_bound}
#'         \code{corner_prob}: Posterior probability on Dirichlet components with concentration less than 1
#'
#' @examples
#' mat <- rbind(c(5, 0, 2, 0),
#'              c(1, 1, 0, 1),
#'              c(100, 100, 50, 100),
#'              c(20, 50, 100, 10),
#'              c(10, 10, 200, 20),
#'              c(50, 54, 58, 53),
#'              c(1,1,1,3),
#'              c(2, 4, 1, 1))
#' out <- dash(xmat, optmethod = "mixEM", verbose=TRUE)
#' out <- dash(xmat, optmethod = "w_mixEM", verbose=TRUE)
#'
#' @export
#'


dash <- function(comp_data,
                 concentration = NULL,
                 mode = NULL,
                 weight = list("center" = 10, "null" = 1, "corner" = 1),
                 def_positions = list("center" = Inf, "null" = 1, "corner" = 0.5),
                 optmethod = c("mixEM", "w_mixEM", "mixIP"),
                 sample_weights = NULL,
                 verbose = FALSE,
                 fdr_bound = 50,
                 bf = TRUE,
                 pi_init = NULL,
                 control = list(),
                 reportcov = FALSE){

  ## check if the mode has same length as columns of composition data

  if(verbose){
    cat("Checking inputs and processing the data \n")
  }

  if(is.null(mode)){
    mode <- rep(1, dim(comp_data)[2])
  }else{
    mode <- mode/min(mode[mode!=0])
  }

  if(!is.null(sample_weights)){
    if(length(sample_weights) != dim(comp_data)[1]){
      stop("The length of the user-defined sample weights must match with number of rows
           in the comp_data")
    }
  }


  if(!is.null(pi_init)){
    if(length(pi_init) != dim(comp_data)[2]){
      stop("The length of the user-defined pi_init must match with number of columns
           in the comp_data")
    }
  }


  if(!is.null(mode)){
    if(length(mode) != dim(comp_data)[2]){
      stop("The length of the user-defined mode must match with number of columns
           in the comp_data")
    }
  }

  ## add prior concentrations - adding an Inf and 1 concentration to the mix
  ## if not provided by the user

  if(is.null(concentration)){
    concentration <- c(Inf, 100, 50, 20, 10, 5, 2, 1, 0.5, 0.1)
  }else{
    concentration <- unique(c(def_positions$center, concentration, def_positions$null, def_positions$corner))
  }


  conc <- concentration
  conc[conc == Inf] <- 10^5
  conc_mat <- t(sapply(conc,function(x) return(x*(mode+1e-04))))

  prior <- array(1, length(concentration))
  prior[match(c(def_positions$center, def_positions$null, def_positions$corner), concentration)] <- c(weight$center, weight$null, weight$corner)


  #############  define the matrix likelihoods   ###########################

  if(verbose){
    cat("Fitting the dash shrinkage \n")
  }

  matrix_log_lik <- matrix(0, dim(comp_data)[1], dim(conc_mat)[1])

  for(n in 1:dim(comp_data)[1]){
    x <- comp_data[n,]
    for(k in 2:dim(conc_mat)[1]){
     # numero <- sum(x)*beta(sum(conc_mat[k,]), sum(x))
      lognumero <- log(sum(x)) - LaplacesDemon::ddirichlet(rep(1,2), alpha = c(sum(conc_mat[k,]), sum(x)), log=TRUE)
      if(lognumero == -Inf | lognumero == Inf ){
        matrix_log_lik[n, k] <- lognumero
      }else{
        index1 <- which(x > 0)
        logdeno <- sum(log(x[index1]) -  sapply(1:length(index1), function(mm) return(LaplacesDemon::ddirichlet(rep(1,2), alpha = c(conc_mat[k, index1[mm]], x[index1[mm]]), log=TRUE))))
        matrix_log_lik[n,k] <- lognumero - logdeno
       # tmp1 <- exp(log(x[index1]) -  sapply(1:length(index1), function(mm) return(LaplacesDemon::ddirichlet(rep(1,2), alpha = c(conc_mat[k, index1[mm]], x[index1[mm]]), log=TRUE))))
      #  tmp2 <- x[index1] * beta(conc_mat[k, index1], x[index1])
       # deno <- prod(x[index1] * beta(conc_mat[k, index1], x[index1]))
       # matrix_lik[n,k] <- numero/deno
      }
    }
    matrix_log_lik[n,1] <- logfac(sum(x)) - sum(sapply(x, function(y) return(logfac(y)))) + sum(x*log((conc_mat[1,]+1e-04)/sum(conc_mat[1,]+1e-04)))
  }

  if(!bf){
    matrix_lik <- exp(matrix_log_lik - max(matrix_log_lik[matrix_log_lik != Inf & matrix_log_lik != -Inf ]))
  }else{
    matrix_lik <- exp(matrix_log_lik - apply(matrix_log_lik, 1, function(x) return(max(x))) %*% t(rep(1, dim(matrix_log_lik)[2])))
  }

  ############################  mixEM optimization ############################

  if(optmethod == "mixEM"){
    fit=do.call("mixEM",args = list(matrix_lik= matrix_lik, prior=prior, pi_init=pi_init, control=control))
  }else if (optmethod == "w_mixEM"){
    fit=do.call("w_mixEM",args = list(matrix_lik= matrix_lik, prior=prior, pi_init=pi_init, control=control, weights=sample_weights))
  }else if (optmethod == "mixIP"){
    fit=do.call("mixIP",args = list(matrix_lik= matrix_lik, prior=prior, pi_init=pi_init, control=control))
  }else{
    message("optmethod npt provided correctly: switching to mixEM")
    fit=do.call("mixEM",args = list(matrix_lik= matrix_lik, prior=prior, pi_init=pi_init, control=control))
  }


  if(verbose){
    cat("Preparing output from fitted model  \n")
  }

  ll <- list()
  ll$fitted_pi <- fit$pihat
  ll$concentration <- concentration
  ll$prior <- prior

  ######################    posterior weights      #########################

  pi_complete <- rep(1, dim(matrix_lik)[1]) %*% t(fit$pihat)
  matrix_lik_adj <- matrix_lik*pi_complete
  posterior_weights <- t(apply(matrix_lik_adj, 1, function(x) return(x/sum(x))))
  ll$posterior_weights <- posterior_weights

  ########################    posterior means      ############################

  conc_mat[conc_mat == Inf] <- 10^5
  posmean_comp <- array(0, c(dim(comp_data)[1], dim(comp_data)[2], dim(conc_mat)[1]))
  for(n in 1:dim(comp_data)[1]){
    for(k in 1:dim(conc_mat)[1]){
      temp <-  comp_data[n,]+ conc_mat[k,]
      posmean_comp[n,,k] <- (temp+1e-08)/sum(temp+1e-08)
    }
  }

  posmean <- matrix(0, dim(comp_data)[1], dim(comp_data)[2])

  for(n in 1:dim(comp_data)[1]){
    posmean[n,] <- posmean_comp[n,,]%*%posterior_weights[n,]
  }

  ll$posmean <- posmean
  ll$datamean <- t(apply(comp_data, 1, function(x) return(x/sum(x))))

  ######################## posterior cov/corr structure  ############################

  if(reportcov){
    poscov_comp <- array(0, c(dim(comp_data)[1], dim(comp_data)[2], dim(comp_data)[2], dim(conc_mat)[1]))
    for(n in 1:dim(comp_data)[1]){
      for(k in 1:dim(conc_mat)[1]){
        temp <-  comp_data[n,]+ conc_mat[k,]
        alpha_0 <- sum(temp)
        posvar <- (alpha_0* temp)/((alpha_0)^2*(alpha_0 + 1))
        poscov_comp[n,,,k] <- diag(posvar) -(temp %*% t(temp))/((alpha_0)^2*(alpha_0 + 1))
      }
    }

    poscov <- array(0, c(dim(comp_data)[2], dim(comp_data)[2], dim(comp_data)[1]))
    poscor <- array(0, c(dim(comp_data)[2], dim(comp_data)[2], dim(comp_data)[1]))

    for(n in 1:dim(comp_data)[1]){
      dsum <- matrix(0, dim(comp_data)[2], dim(comp_data)[2])
      for(k in 1:dim(conc_mat)[1]){
        dsum <- dsum + poscov_comp[n,,,k]*posterior_weights[n,k]
      }
      poscov[,,n] <- dsum
      poscor[,,n] <- cov2cor(dsum)
    }

    ll$poscov <- poscov
    ll$poscor <- poscor
  }

  ######################   FDR + corner enrichment  #########################

  ll$center_prob_local <- posterior_weights[,which(concentration == Inf)]
  ll$center_prob <- rowSums(posterior_weights[, which(concentration > fdr_bound)])
  ll$corner_prob <- rowSums(posterior_weights[, which(concentration < 1)])

  return(ll)
}


logfac = function(x){
  if(x < 1 && x > 0 || x < 0){
    stop("x cannot be less than 0 or a fraction")
  }else if(x < 10){
    out <- log(factorial(x))
  }else if (x == 0){
    out <- 0
  }else{
    out <- sum(log(1:x))
  }
  return(out)
}


