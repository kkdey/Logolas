#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- fourletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
#' out <- fourletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")



fourletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  x <- c(0.3, 0.15, 0.55, 0.55, 0.70, 0.70, 0.80, 0.80, 0.70, 0.70, 0.55, 0.55, 0.32, 0.45)
  y <- c(1, 0.25, 0.25, 0, 0, 0.25, 0.25, 0.40, 0.40, 0.55, 0.55, 0.40, 0.40, 1)


  id <- rep(1, length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



