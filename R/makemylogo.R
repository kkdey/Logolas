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
#'
#' @param colfill The color used for the symbol
#'
#' @param plot  binary, if FALSE, returns only the co-ordinates of the symbol in the
#' [0,1] X [0,1] grid, along with block id labels and their corresponding colors.
#' If TRUE, plots the symbol with specified color in a new grid window.
#'
#' @param addlogos Vector of additional logos/symbols defined by user
#'
#' @param addlogos_text Vector of the names given to the additional
#' logos/symbols defined by user.
#'
#' @return Along with symbol plot, if plot is TRUE, returns a list with the
#' following items.
#'         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{id}{id vector representing blocks in the logo co-ordinates}
#'         \item{fill}{a vector equal to the number of distinct ids or blocks in
#'                    the logo, whose elements correspond to colors of these
#'                    blocks}
#'
#' @import grid
#'
#' @examples
#'
#' makemylogo("KUSHAL")
#' makemylogo("MATTHEW", colfill="red")
#' makemylogo("Q-BIO.QM;A,DD>R::C,123456789:D0O", plot=TRUE)
#' lambdaletter <- function(colfill="green"){
#'
#'     x <- c(0.15, 0.5, 0.85, 0.75, 0.5, 0.25)
#'     y <- c(0, 1, 0, 0, 0.8, 0)
#'
#'     fill <- colfill
#'     id <- rep(1, length(x))
#'     ll <- list("x"= x,
#'           "y"= y,
#'           "id" = id,
#'           "fill" = fill)
#'            return(ll)}
#' makemylogo("AC>EF/lambda/W35",
#'            addlogos="lambda",
#'            addlogos_text="lambda",
#'            plot=TRUE)
#' @export


makemylogo <- function(name,
                       colfill="orange",
                       plot=FALSE,
                       addlogos = NULL,
                       addlogos_text = NULL){

  split_string <- strsplit(as.character(name), "")[[1]]

  if(!is.null(addlogos)){
  slash_index <- grep("/", split_string)

  groups <- length(slash_index)/2;

  sym <- array(0, groups)
  sym_slash_index <- numeric()
  sym_slash_top_index <- array(0, groups)
  counter  <- 1

  for(l in seq_len(groups)){
    sym[l] <- paste0(as.character(split_string[(slash_index[counter]+1) : (slash_index[counter+1]-1)]),
                     collapse="")
    sym_slash_index <- c(sym_slash_index, slash_index[counter] : slash_index[counter+1])
    sym_slash_top_index[l] <- slash_index[counter]
    counter <- counter + 2
  }


  split_string_alphanumeric <- split_string[-sym_slash_index]

  inds_alphanumeric <- (seq_along(split_string))[-sym_slash_index]

  split_string_mod <- c(split_string_alphanumeric, sym)
  split_string_inds <- c(inds_alphanumeric, sym_slash_top_index)

  split_string <- split_string_mod[order(split_string_inds)]
  }

 # split_string <- strsplit(name, "")[[1]]
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

  if(!is.null(addlogos)){

    if(length(addlogos) != length(addlogos_text)){
      stop("length of the logo name and the logo text name must match")
    }

    for(num in seq_along(addlogos)){
      split_string[grep(as.character(addlogos[num]), split_string)] <- as.character(addlogos_text[num])
    }
  }

  chars <- paste0(split_string, "letter")
  xpool <- numeric()
  ypool <- numeric()
  idpool <- numeric()
  fillpool <- numeric()

  counter <- 0
  for(m in seq_along(chars)){
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


