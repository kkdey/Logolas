#' @title PFM/PWM Pseudo-count adjustment to get rid of 0 entries.
#'
#' @description Building a pseudo-count adjusted matrix from a PWM
#' (positional weight matrix) or a PFM (positional frequency matrix) with 0 entries.
#' 
#' @param table A table (PFM/PWM) with the symbols along the rows and the logo stack
#' positions along the columns, possibly containing 0 values.
#' @param pseudocount A small pseudocount to be added mainly to bypass 0 entries. 
#'                     Default is NULL. If \code{table} is a counts matrix, 
#'                     the default changes to 0.5, if \code{table} is a 
#'                     positional weight matrix, the default becomes 0.001 times
#'                     the minimum non-zero value of the table.
#' @return Returns a new table that pseudo-count adjusts the 0 values.
#' @examples 
#' data("mutation_sig")
#' pseudocount_adjust(mutation_sig)
#' @export 

pseudocount_adjust <- function(table, pseudocount = NULL){
  fl_table <- floor(table)
  diff <- table[!is.na(table)] - fl_table[!is.na(fl_table)]
  max_tab <- max(abs(table[table > 1e-10]), na.rm = TRUE)
  if(sum(abs(diff)) == 0){
    message("The table contains counts")
    if(is.null(pseudocount)){pseudocount <- 5e-01*max_tab}
    table1 <- table + pseudocount
  }else{
    if(is.null(pseudocount)){pseudocount <- 5e-01*max_tab}
    table1 <- table + pseudocount
  }
  return(table1)
}
