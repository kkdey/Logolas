
library(grid)
files <- list.files(pattern="letter")
sapply(list.files(pattern="letter", full.names = TRUE), source)


makemylogo <- function(name, colfill="orange"){
  
  chars <- paste0(strsplit(name, "")[[1]], "letter")
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

