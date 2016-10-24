#' @title Logo maker for a given English alphanumeric with common punctuations
#'
#' @description Plots logo for a given english symbol or name that contains
#' English alphabets, numbers or punctuations like dots, dashes, etc. This is
#' the skeleton used by the \code{logomaker} function of the package to create
#' distinct logos for distinct alphanumeric symbols.
#'
#'
#' @param name A English name, or alphanumeric, containing English alphabets,
#' numbers, dots, dashes, arroww, colons, semicolons, comma among punctuations.
#' @param colfill The color used for the symbol
#' @param plot  binary, if FALSE, returns only the co-ordinates of the symbol in the
#' [0,1] X [0,1] grid, along with block id labels and their corresponding colors.
#' If TRUE, plots the symbol with specified color in a new grid window.
#'
#' @return Along with symbol plot, if plot is TRUE, returns a list with the following items.
#'         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{id}{id vector representing blocks in the logo co-ordinates}
#'         \item{fill}{a vector equal to the number of distinct ids or blocks in
#'                    the logo, whose elements correspond to colors of these blocks}
#'
#' @import grid
#'
#' @examples
#'
#' makemylogo("KUSHAL")
#' makemylogo("MATTHEW", colfill="red")
#' makemylogo("Q-BIO.QM;A,DD>R::C,123456789:D0O", plot=TRUE)
#'
#' @export


makemylogo <- function(name, colfill="orange", plot=FALSE){

  sapply(list.files(pattern="letter.R", full.names = TRUE), source)

  split_string <- strsplit(name, "")[[1]]
  split_string[grep("[-]", split_string)] <- "dash"
  split_string[grep("[.]", split_string)] <- "dot"
  split_string[grep("[>]", split_string)] <- "rightarrow"
  split_string[grep("[<]", split_string)] <- "leftarrow"
  split_string[grep("[,]", split_string)] <- "comma"
  split_string[grep("[:]", split_string)] <- "colon"
  split_string[grep("[;]", split_string)] <- "semicolon"
  split_string[grep("[0]", split_string)] <- "zero"
  split_string[grep("[1]", split_string)] <- "one"
  split_string[grep("[2]", split_string)] <- "two"
  split_string[grep("[3]", split_string)] <- "three"
  split_string[grep("[4]", split_string)] <- "four"
  split_string[grep("[5]", split_string)] <- "five"
  split_string[grep("[6]", split_string)] <- "six"
  split_string[grep("[7]", split_string)] <- "seven"
  split_string[grep("[8]", split_string)] <- "eight"
  split_string[grep("[9]", split_string)] <- "nine"

  chars <- paste0(split_string, "letter")
  xpool <- numeric()
  ypool <- numeric()
  idpool <- numeric()
  fillpool <- numeric()

  counter <- 0
  for(m in 1:length(chars)){
    fun <- get(chars[m])
    out <- fun(colfill=colfill)
    xpool <- c(xpool, (m-1)*(1/length(chars)) + (1/length(chars))*out$x);
    ypool <- c(ypool, out$y)
    idpool <- c(idpool, out$id + counter)
    fillpool <- c(fillpool, out$fill)
    counter <- counter + max(out$id);

  }

  if(plot){

    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    lwd <- 10
    grid::grid.polygon(xpool, ypool,
                 default.unit="native",
                 id=idpool,
                 gp=grid::gpar(fill=fillpool,
                               lwd=lwd))
    }

  ll <- list("x"=xpool,
             "y"=ypool,
             "id"=idpool,
             "fill"=fillpool)
  return(ll)
}


