

#########  pooling names from a matrix and apply logo plots  ################


counts_mat <- rbind(c(0, 10, 100, 60, 20), 
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70)
)

colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
rownames(counts_mat) <- c("Politics", "Science", "Religion",
                          "Literature")


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

ic <- pwm2ic(counts_mat_norm)

counts_mat_norm <- apply(counts_mat, 2, function(x) return(x/sum(x)))
npos <- ncol(counts_mat_norm)
chars <- as.character(rownames(counts_mat_norm))


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

#addLetter(letters, chars[1], "green", 0, 0, 1, 1)
#addLetter(chars, chars[2], "orange", 0, 0, 1, 1)
#addLetter(chars, chars[3], "blue", 0, 0, 1, 1)
#addLetter(chars, chars[4], "red", 0, 0, 1, 1)


ic.scale=FALSE
xaxis=TRUE 
yaxis=TRUE 
xaxis_fontsize=10
xlab_fontsize=15
y_fontsize=15
mut_width=2
start=0.0001
yscale_change=TRUE
pop_name = NULL

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
  ylab <- "Information content"
}else{
  ylim <- 1
  ylab <- "Probability"
  facs <- rep(1, npos)
}


wt <- c(rep(1,npos))
x.pos <- 0 
letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
chars <- as.character(rownames(counts_mat_norm))
cols <- RColorBrewer::brewer.pal(length(chars),name = "Spectral")

for (j in 1:npos){
  
  column <- counts_mat_norm[,j]
  hts <- 0.99*column*facs[j]
  letterOrder <- order(hts)
  
  y.pos <- 0    
  for (i in 1:length(chars)){
    letter <- chars[letterOrder[i]]
    col <- cols[letterOrder[i]]
    ht <- hts[letterOrder[i]]
    if (ht>0) letters <- addLetter(letters,letter, col, x.pos,y.pos,ht,wt[j])
    y.pos <- y.pos + ht + start
  }
  x.pos <- x.pos + wt[j]
  
}



grid.newpage()
bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)
leftMargin = ifelse(yaxis, 2 + y_fontsize/3.5, 3)
pushViewport(plotViewport(c(bottomMargin,leftMargin,max(xlim)+0.5,max(xlim)+0.5)))
# pushViewport(viewport(layout = grid.layout(2, 2),
#              x = bottomMargin,
#              y = leftMargin,
#              width = max(xlim/2)+0.5,
#              height = max(ylim/2)+0.5))
pushViewport(dataViewport(0:ncol(counts_mat_norm),0:ylim,name="vp1"))
grid.polygon(x=unit(letters$x,"native"), y=unit(letters$y,"native"),
             id=letters$id, gp=gpar(fill=letters$fill,col="transparent"))
grid.polygon(x=unit(letters$x,"native"), y=unit(letters$y,"native"),
             id=letters$id,
             gp=gpar(fill=letters$fill,col="transparent"))

xlim <- cumsum(wt) - wt/2;
# xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
ylim_scale <- seq(0, ylim, length.out=6);
ic_lim_scale <- seq(0, max(ic), length.out=6)

for(n in 1:length(xlim)){
  grid.lines(x = unit(low_xlim[n], "native"),
             y = unit(c(0, ylim), "native"),
             gp=gpar(col="grey80"))
}

if(is.null(pop_name)){
  grid.text("Damage logo plot", y = unit(1, "npc") + unit(1.5, "lines"),
            gp = gpar(fontsize = 16))
}else{
  grid.text(paste0("Damage logo plot for ", pop_name), y = unit(1, "npc") + unit(1.5, "lines"),
            gp = gpar(fontsize = 16))
}

if (xaxis){
  grid.xaxis(at=seq(0.5,ncol(counts_mat_norm)-0.5),
             label=colnames(counts_mat_norm), 
             gp=gpar(fontsize=xaxis_fontsize))
  grid.text("Position",y=unit(-3,"lines"), gp=gpar(fontsize=xaxis_fontsize))
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

