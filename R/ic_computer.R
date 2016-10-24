#' @title Information criterion computer
#'
#' @description Computes information criterion required in determining the size
#' of the logos in the logo plot. Similar to the pwm2ic() function in seqLogo
#' package
#'
#' @param mat A matrix with symbols or logo names along the rows and
#' the sites/positions/groups along the columns.
#'
#' @return A vector of same length as the number of columns in the data, with each
#'         entry determining the height of the bar for the logo plot for that
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
#' rownames(counts_mat) <- c("Kushal", "Gao", "Hussein",
#'                          "Joyce")
#' ic_computer(counts_mat)

ic_computer <-function(mat) {
  npos<-ncol(mat)
  ic <-numeric(length=npos)
  for (i in 1:npos) {
    ic[i]<- log(nrow(mat), base=2) + sum(sapply(mat[, i], function(x) {
      if (x > 0) { x*log2(x) } else { 0 }
    }))
  }
  return(ic)
}
