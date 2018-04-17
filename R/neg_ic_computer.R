# @title Negative scaling output for nlogomaker function
#
# @description a scaling function for \code{nlogomaker()} to separately scale the
# positive and the negative axes heights in the enrichment logo plots.
#
# @param table The input table (data frame or matrix) of counts across different
# logos or symbols (specified along the rows) ans across different sites or
# positions or groups (specified along the columns).
#
# @param hist Whether to use the hist method or the information criterion
# method to determine the heights of the logos.
#
# @param alpha The Renyi entropy tuning parameter which is used in case of
# scaling of the bar heights by information criterion. The default tuning
# parameter value is 1, which corresponds to Shannon entropy.
#
# @return Returns the enrichment heights in the positive and the negative scale.
#   \code{pos}: scaling on the positive scale
#   \code{neg}: scaling on the negative scale
#   \code{scales}: the scaling factor for each site positive and negative heights
#   \code{ic_pos}: height of the positive bar
#   \code{ic_neg}: height of the negative bar
#
#
# @examples
#
# m = matrix(rep(0,48),4,12)
# m[1,] = c(0,0,2.5,7,0,0,0,0,0,0,1,0)
# m[2,] = c(4,6,3,1,0,0,0,0,0,5,0,5)
# m[3,] = c(0,0,0,0,0,1,8,0,0,1,1,2)
# m[4,] = c(4,2,2.5,0,8,7,0,8,8,2,6,1)
# rownames(m) = c("A", "C", "G", "T")
# colnames(m) = 1:12
# m=m/8
# ic2 <- neg_ic_computer(m)
#
# ic2$ic_pos
# ic2$ic_neg
# ic2$scales

neg_ic_computer <- function (table,
                             hist = FALSE,
                             alpha = 1){

  table <- apply(table+0.0001,2,normalize)

  if (class(table) == "data.frame"){
    table <- as.matrix(table)
  }else if (class(table) != "matrix"){
    stop("the table must be of class matrix or data.frame")
  }

  table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x[!is.na(x)])))
  npos <- ncol(table_mat_norm)
  chars <- as.character(rownames(table_mat_norm))

  table_mat_adj <- apply(table_mat_norm, 2, function(x)
  {
    indices <- which(is.na(x))
    if(length(indices) == 0){
      y = x
      z <- y - median(y)
      return(z)
    }else{
      y <- x[!is.na(x)]
      z <- y - median(y)
      zext <- array(0, length(x))
      zext[indices] <- 0
      zext[-indices] <- z
      return(zext)
    }
  })

  table_mat_pos <- table_mat_adj
  table_mat_pos[table_mat_pos<= 0] = 0
  table_mat_pos_norm  <- apply(table_mat_pos, 2, function(x) return(x/sum(x)))
  table_mat_pos_norm[table_mat_pos_norm == "NaN"] = 0

  table_mat_neg <- table_mat_adj
  table_mat_neg[table_mat_neg >= 0] = 0
  table_mat_neg_norm  <- apply(table_mat_neg, 2, function(x) return(x/sum(x)))
  table_mat_neg_norm[table_mat_neg_norm == "NaN"] = 0

  table_mat_norm <- replace(table_mat_norm, is.na(table_mat_norm), 0)

  ic <- ic_computer(table_mat_norm, alpha, hist=hist)
  tab_neg <- apply(table_mat_adj, 2, function(x) {
    y = x[x < 0]
    if(length(y) == 0){
      return(0)
    }else{
      return(abs(sum(y)))
    }
  })

  tab_pos <- apply(table_mat_adj, 2, function(x) {
    y = x[x > 0]
    if(length(y) == 0){
      return(0)
    }else{
      return(abs(sum(y)))
    }
  })

  pos_neg_scaling <- apply(rbind(tab_pos, tab_neg), 2, function(x) return(x/sum(x)))

  scaled_tab_mat_pos_norm <- table_mat_pos_norm %*% diag(pos_neg_scaling[1,])
  scaled_tab_mat_neg_norm <- table_mat_neg_norm %*% diag(pos_neg_scaling[2,])

  ic_tab_mat_pos_norm <- table_mat_pos_norm %*% diag(pos_neg_scaling[1,]*ic)
  ic_tab_mat_neg_norm <- table_mat_neg_norm %*% diag(pos_neg_scaling[2,]*ic)

  ll <- list("pos" = scaled_tab_mat_pos_norm,
             "neg" = scaled_tab_mat_neg_norm,
             "scales" = pos_neg_scaling,
             "ic_pos" = ic_tab_mat_pos_norm,
             "ic_neg" = ic_tab_mat_neg_norm)
  return(ll)
}

normalize = function(x){return(x/sum(x))}

