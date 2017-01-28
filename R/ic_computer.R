#' @title Information criterion computer
#'
#' @description Computes information criterion based on Renyi entropy
#' for determining the size of the logos in the logo plot. Similar but allows
#' for more general entropy criteria to the pwm2ic() function in seqLogo package.
#' By tuning the alpha parameter input, one can vary the entropy from Shannon
#' (alpha=1) to collision (alpha=2) to min-entropy (alpha=infty)
#'
#' @param mat A matrix with symbols or logo names along the rows and
#' the sites/positions/groups along the columns.
#'
#' @param alpha The tuning parameter of the Renyi entropy used in computing
#' the information criterion. Default is alpha=1, for which it uses Shannon
#' entropy (in the limit).
#'
#' @return A vector of same length as the number of columns in the data, with
#'         each entry representing information contained in that column,
#'         which determines the height of the bar for the logo plot for that
#'         particular column (site/position/block).
#'
#' @export
#' @examples
#'
#' counts_mat <- rbind(c(0, 10, 100, 60, 20),
#'                    c(40, 30, 30, 35, 20),
#'                    c(100, 0, 15, 25, 75),
#'                    c(10, 30, 20, 50, 70)
#'               )

#' colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
#' rownames(counts_mat) <- c("P1", "P2", "P3", "P4")
#' ic_computer(counts_mat, alpha=2)

ic_computer <-function(mat, alpha, hist=FALSE) {
  if(!hist){
    mat <- apply(mat, 2, function(x) return(x/sum(x)))
    npos<-ncol(mat)
    ic <-numeric(length=npos)
    for (i in 1:npos) {
      if(alpha == 1){
        ic[i] <- log(nrow(mat), base=2) + sum(sapply(mat[, i], function(x) {
          if (x > 0) { x*log2(x) } else { 0 }
        }))
      }
      else if(alpha == Inf){
        ic[i] <- log(nrow(mat), base=2) + log(max(mat[,i]))
      }
      else if(alpha <= 0){
        stop("alpha value must be greater than 0")
      }
      else{
        ic[i] <- log(nrow(mat), base=2) - (1/(1-alpha))* log (sum(mat[,i]^{alpha}))
      }
    }
    return(ic)
  }else{
    mat <- mat/sum(mat)
    ic <- colSums(mat)
    return(ic)
  }
}
