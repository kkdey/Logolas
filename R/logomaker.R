

########################  Logo maker in R  ###########################

source("makemylogo.R")

addLetter <- function(letters, letter, col, x.pos, y.pos,ht,wt){
  letter <- as.character(toupper(letter))
  out <- makemylogo(letter, colfill = col)
  x <- x.pos + out$x * wt
  y <- y.pos + out$y * ht
  
  letter <- list("x"=x,
                 "y"=y,
                 "id"=out$id,
                 "fill"=out$fill)
  
  letters$x <- c(letters$x,letter$x)
  letters$y <- c(letters$y,letter$y)
  
  lastID <- ifelse(is.null(letters$id),0,max(letters$id))
  letters$id <- c(letters$id,lastID+letter$id)
  letters$fill <- c(letters$fill,letter$fill)
  letters
}

pwm2ic<-function(pwm) {
  npos<-ncol(pwm)
  ic<-numeric(length=npos)
  for (i in 1:npos) {
    ic[i]<- log(nrow(pwm), base=2) + sum(sapply(pwm[, i], function(x) { 
      if (x > 0) { x*log2(x) } else { 0 }
    }))
  }    
  return(ic)
}



logomaker <- function( table,
                       ic=NULL,
                       cols,
                       frame_width=NULL,
                       ic.scale=TRUE, 
                       xaxis=TRUE, 
                       yaxis=TRUE, 
                       xaxis_fontsize=10,
                       xlab_fontsize=15,
                       y_fontsize=15,
                       start=0.0001,
                       yscale_change=TRUE,
                       pop_name = NULL,
                       xlab = "X",
                       ylab = "Information content"){
  
  if(length(cols) != dim(table)[1]){
    stop("the number of colors must match the number of symbols")
  }
  if(is.null(frame_width)){
    message("frame width not provided, taken to be 1")
    wt <- rep(1,dim(table)[2])
  }
  if(!is.null(frame_width)){
      if(length(frame_width)==1){
            wt <- rep(frame_width, dim(table)[2])
      }else{
            wt <- frame_width
      }
  }
  
  if (class(table) == "data.frame"){
    table <- as.matrix(table)
  }else if (class(table) != "matrix"){
    stop("the table must be of class matrix or data.frame")
  }
  
  table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x)))
  npos <- ncol(table_mat_norm)
  chars <- as.character(rownames(table_mat_norm))
  
  if(is.null(ic)){
    ic <- pwm2ic(table_mat_norm)
  }
  
  letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
  npos <- ncol(table_mat_norm)
  
  
  if (ic.scale){
    if(yscale_change){
      if(max(ic)<1){ylim <- 1
      facs <- ic + 1 - max(ic)}
      if(max(ic)>1){ylim <- 2
      facs <- ic}
    }else{
      ylim <- 2
      facs <- ic
    }
    ylab <- ylab
  }else{
    ylim <- 1
    ylab <- "Probability"
    facs <- rep(1, npos)
  }
  
  x.pos <- 0 
  
  for (j in 1:npos){
    
    column <- table_mat_norm[,j]
    hts <- 0.99*column*facs[j]
    letterOrder <- order(hts)
    
    y.pos <- 0    
    for (i in 1:length(chars)){
      letter <- chars[letterOrder[i]]
      col <- cols[letterOrder[i]]
      ht <- hts[letterOrder[i]]
      if (ht>0) letters <- addLetter(letters, letter, col, x.pos,y.pos,ht,wt[j])
      y.pos <- y.pos + ht + start
    }
    x.pos <- x.pos + wt[j]
    
  }
  
  xlim <- cumsum(wt) - wt/2;
  # xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
  low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
  ylim_scale <- seq(0, ylim, length.out=6);
  ic_lim_scale <- seq(0, max(ic), length.out=6)
  
  
  grid.newpage()
  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)
  leftMargin = ifelse(yaxis, 0.1 + y_fontsize/3.5, 3)
  pushViewport(plotViewport(c(bottomMargin,leftMargin,max(ylim)+0.5,max(xlim)*wt+0.5)))
  # pushViewport(viewport(layout = grid.layout(2, 2),
  #              x = bottomMargin,
  #              y = leftMargin,
  #              width = max(xlim/2)+0.5,
  #              height = max(ylim/2)+0.5))
  pushViewport(dataViewport(0:ncol(table_mat_norm),0:ylim,name="vp1"))
  grid.polygon(x=unit(letters$x,"native"), y=unit(letters$y,"native"),
               id=letters$id, gp=gpar(fill=letters$fill,col="transparent"))
  grid.polygon(x=unit(letters$x,"native"), y=unit(letters$y,"native"),
               id=letters$id,
               gp=gpar(fill=letters$fill,col="transparent"))
  

  for(n in 1:length(xlim)){
    grid.lines(x = unit(low_xlim[n], "native"),
               y = unit(c(0, ylim), "native"),
               gp=gpar(col="grey80"))
  }
  
  if(is.null(pop_name)){
    grid.text("Logo plot", y = unit(1, "npc") + unit(1.5, "lines"),
              gp = gpar(fontsize = 16))
  }else{
    grid.text(paste0("Logo plot of ", pop_name), y = unit(1, "npc") + unit(1.5, "lines"),
              gp = gpar(fontsize = 16))
  }
  
  if (xaxis){
    grid.xaxis(at=wt*seq(0.5,ncol(table_mat_norm)-0.5),
               label=colnames(table_mat_norm), 
               gp=gpar(fontsize=xaxis_fontsize))
    grid.text(xlab, y=unit(-3,"lines"), gp=gpar(fontsize=xaxis_fontsize))
  }
  if (yaxis){
    if(yscale_change==TRUE){
      grid.yaxis(at = ylim_scale,
                 label = round(ic_lim_scale,1),
                 gp=gpar(fontsize=y_fontsize))
    }else{
      grid.yaxis(gp=gpar(fontsize=y_fontsize))
    }
    grid.text(ylab,x=unit(-3,"lines"),rot=90, 
              gp=gpar(fontsize=y_fontsize))
  }
  popViewport()
  popViewport()
  par(ask=FALSE)
}

################   examples   ##########################

counts_mat <- rbind(c(0, 10, 100, 60, 20), 
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70)
)

colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
rownames(counts_mat) <- c("Kushal", "Gao", "Hussein",
                          "Joyce")

logomaker(counts_mat, 
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
          frame_width = 1,
          ic.scale = FALSE)

