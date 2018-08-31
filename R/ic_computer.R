# @title Information criterion computer
#
# @description Computes information criterion based on Renyi entropy
# for determining the size of the logos in the logo plot. Similar but allows
# for more general entropy criteria to the pwm2ic() function in seqLogo package.
# By tuning the alpha parameter input, one can vary the entropy from Shannon
# (alpha=1) to collision (alpha=2) to min-entropy (alpha=infty)
#
# @param mat A matrix with symbols or logo names along the rows and
# the sites/positions/groups along the columns.
#
# @param alpha The tuning parameter of the Renyi entropy used in computing
# the information criterion. Default is alpha=1, for which it uses Shannon
# entropy (in the limit).
#
# @param hist if hist is FALSE (default). information criterion is used to 
# decide on the heights
# of the logo plots. If TRUE, one uses the relative proportion of the values in
# the different columns of the matrix to determine the height of the bars.
#
# @param bg The background probability, which defaults to NULL, in which case
# equal probability is assigned to each symbol. The user can however specify a
# vector (equal to in length to the number of symbols) which specifies the
# background probability for each symbol and assumes this background probability
# to be the same across the columns (sites), or a matrix, whose each cell 
# specifies the background probability of the symbols for each position.
#
# @return A vector of same length as the number of columns in the data, with
#         each entry representing information contained in that column,
#         which determines the height of the bar for the logo plot for that
#         particular column (site/position/block).
#
# @examples
#
# counts_mat <- rbind(c(0, 10, 100, 60, 20),
#                    c(40, 30, 30, 35, 20),
#                    c(100, 0, 15, 25, 75),
#                    c(10, 30, 20, 50, 70)
#               )

# colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
# rownames(counts_mat) <- c("P1", "P2", "P3", "P4")
# ic_computer(counts_mat, alpha=2)

ic_computer <-function(mat, alpha, pseudocount=NULL, hist=FALSE, bg = NULL) {

  if (is.vector(bg)==TRUE){
    if(length(bg) != dim(mat)[1]){
      stop("If background prob (bg) is a vector, the length of bg must equal 
           the number of symbols for the logo plot")
    }else if(length(which(is.na(mat))) > 0){
      stop("For NA in table, a vector bg is not allowed")
    }else{
      bgmat <- bg %*% t(rep(1, dim(mat)[2]))
      bgmat[which(is.na(mat))] <- NA
      bgmat <- apply(bgmat, 2, function(x) return(x/sum(x[!is.na(x)])))
    }
  }else if (is.matrix(bg)==TRUE){
    if(dim(bg)[1] != dim(mat)[1] | dim(bg)[2] != dim(mat)[2]){
      stop("If background prob (bg) is a matrix, its dimensions must match that
           of the table")
    }else{
      bgmat <- bg
      bgmat[which(is.na(mat))] <- NA
      bgmat <- apply(bgmat, 2, function(x) return(x/sum(x[!is.na(x)])))
    }
  }else {
    message ("using a background with equal probability for all symbols")
    bgmat <- matrix(1/dim(mat)[1], dim(mat)[1], dim(mat)[2])
    bgmat[which(is.na(mat))] <- NA
    bgmat <- apply(bgmat, 2, function(x) return(x/sum(x[!is.na(x)])))
  }

  if(is.null(pseudocount)){pseudocount <- 0.1}
  pseudocount2 <- pseudocount*max(max(abs(mat), na.rm=TRUE), max(abs(bgmat), na.rm=TRUE))
  mat <- pseudocount_adjust(mat, pseudocount2)
  mat <- apply(mat,2,normalize_ic_computer)
  bgmat <- pseudocount_adjust(bgmat, pseudocount2)
  bgmat <- apply(bgmat,2,normalize_ic_computer)

  if(!hist){
    mat <- apply(mat, 2, function(x) return(x/sum(x[!is.na(x)])))
    npos<-ncol(mat)
    ic <-numeric(length=npos)
    for (i in 1:npos) {
      if(alpha == 1){
        if(is.null(bg)){
          tmp <- mat[,i]
          tmp <- tmp[!is.na(tmp)]
          ic[i] <- log(length(which(tmp!=0.00)), base=2) + 
              sum(sapply(tmp, function(x) {
            if (x > 0) { x*log2(x) } else { 0 }
          }))
        }else{
          tmp <- mat[!is.na(mat[,i]), i]
          bgtmp <- bgmat[!is.na(mat[,i]), i]
          ic[i] <- sum(sapply(1:length(tmp), function(x) {
            if (x > 0) { tmp[x]*log2(tmp[x]) } else { 0 }
          })) - sum(sapply(1:length(tmp), function(x) {
            if (x > 0) { tmp[x]*log2(bgtmp[x]) } else { 0 }
          }))
        }
      }
      else if(alpha == Inf){
        tmp <- mat[!is.na(mat[,i]), i]
        ic[i] <- log(length(which(tmp!=0.00)), base=2) + log(max(tmp))
      }
      else if(alpha <= 0){
        stop("alpha value must be greater than 0")
      }
      else{
        if(is.null(bg)){
          tmp <- mat[!is.na(mat[,i]), i]
          ic[i] <- log(length(which(tmp !=0.00)), base=2) - 
              (1/(1-alpha))* log (sum(tmp^{alpha}), base=2)
        }else{
          tmp <- mat[!is.na(mat[,i]), i]
          bgtmp <- bgmat[!is.na(mat[,i]), i]
          ic[i] <- abs((log(length(which(tmp !=0.00)), base=2) - 
                            (1/(1-alpha))* log2(sum(tmp^{alpha}))) -
                         (log(length(which(tmp !=0.00)), base=2) - 
                              (1/(1-alpha))* log2(sum(bgtmp^{alpha}))))
        }
      }
    }
    return(ic)
  }else{
    mat <- mat/sum(mat[!is.na(mat)])
    ic <- colSums(mat, na.rm = TRUE)
    return(ic)
  }
}

normalize_ic_computer <- function(x){return(x/sum(x[!is.na(x)]))}
