
library(grid)
files <- list.files(pattern="letter")
sapply(list.files(pattern="letter", full.names = TRUE), source)


makemylogo <- function(name, colfill="orange"){
  
  split_string <- strsplit(name, "")[[1]]
  split_string[grep("[-]", split_string)] <- "dash"
  split_string[grep("[.]", split_string)] <- "dot"
  split_string[grep("[>]", split_string)] <- "rightarrow"
  split_string[grep("[<]", split_string)] <- "leftarrow"
  split_string[grep("[,]", split_string)] <- "comma"
  split_string[grep("[:]", split_string)] <- "colon"
  split_string[grep("[;]", split_string)] <- "semicolon"
  
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
  
  grid.newpage()
  pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                        clip=TRUE))
  lwd <- 10
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=idpool,
               gp=gpar(fill=fillpool, 
                       lwd=lwd))
  
  ll <- list("x"=xpool, 
             "y"=ypool,
             "id"=idpool,
             "fill"=fillpool)
  return(ll)
}

makemylogo("KUSHAL")
makemylogo("MATTHEW", colfill="red")
makemylogo("PURABI", colfill="blue")
makemylogo("Q-BIO.QM;A,DD>R::C")

