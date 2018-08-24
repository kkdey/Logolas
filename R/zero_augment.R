#' @title PFM/PWM Table Zero Augmentation
#'
#' @description Building the zero-augmented positional weight matrix from a PWM
#' (positional weight matrix) or a PFM (positional frequency matrix) with 0 entries.
#' 
#' @param table A table (PFM/PWM) with the symbols along the rows and the logo stack
#' positions along the columns, possibly containing 0 values.
#' 
#' @return Returns a new table that augments the zeros in the input \code{table}.
#' @return eps A small pertutbation value to be added to 0. Default is NULL.
#'             If \code{table} is a counts matrix, the default changes to 0.5, 
#'             if \code{table} is a positional weight matrix, the default becomes
#'             0.001.
#' @examples 
#' data("mutation_sig")
#' zero_augment(mutation_sig)
#' @export 

zero_augment <- function(table, eps = NULL){
  fl_table <- floor(table)
  diff <- table[!is.na(table)] - fl_table[!is.na(fl_table)]
  if(sum(abs(diff)) == 0){
    message("The table contains counts")
    if(is.null(eps)){eps <- 0.5}
    table1 <- table + eps
  }else{
    if(is.null(eps)){eps <- 1e-03}
    table1 <- table + eps
  }
  return(table1)
}
