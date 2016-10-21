
library(grid)
files <- list.files(pattern="letter")
sapply(list.files(pattern="letter", full.names = TRUE), source)


makemylogo <- function(name, colfill="orange", plot=FALSE){

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

    grid.newpage()
    pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    lwd <- 10
    grid.polygon(xpool, ypool,
                 default.unit="native",
                 id=idpool,
                 gp=gpar(fill=fillpool,
                         lwd=lwd))
    }

  ll <- list("x"=xpool,
             "y"=ypool,
             "id"=idpool,
             "fill"=fillpool)
  return(ll)
}

## makemylogo("KUSHAL")
## makemylogo("MATTHEW", colfill="red")
## makemylogo("PURABI", colfill="blue")
makemylogo("Q-BIO.QM;A,DD>R::C,123456789:D0O", plot=TRUE)

