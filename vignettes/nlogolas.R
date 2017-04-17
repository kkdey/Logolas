


######################   normalized Logolas plots   ################################


# counts_mat <- rbind(c(40, 10, 100, 60, 20),
#                     c(40, 30, 30, 35, 20),
#                     c(100, 0, 15, 25, 75),
#                     c(40, 30, 20, 50, 70))
# colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
# rownames(counts_mat) <- c("MAN", "MAIL", "LAWN", "CAR")



counts_mat <- rbind(c(0.3, 0.8, 0.05, 0.05, 0.1),
                    c(0.3, 0.1, 0.15, 0.45, 0.1),
                    c(0.3, 0.05, 0.2, 0.45, 0.1),
                    c(0.1, 0.05, 0.6, 0.05, 0.7))
colnames(counts_mat) <- c("1", "2", "3", "4", "5")
rownames(counts_mat) <- c("C", "T", "A", "G")


table <- counts_mat
table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x)))
npos <- ncol(table_mat_norm)
chars <- as.character(rownames(table_mat_norm))

counts_mat_adj <- apply(counts_mat, 2, function(x) return(x - median(x)))

alpha=1
hist <- FALSE
ic <- ic_computer(counts_mat, alpha, hist=hist)


counts_neg <- apply(counts_mat_adj, 2, function(x) {
                                                  y = x[x < 0]
                                                  if(length(y) == 0){
                                                    return(0)
                                                  }else{
                                                    return(abs(sum(y)))
                                                  }
})

counts_pos <- apply(counts_mat_adj, 2, function(x) {
  y = x[x > 0]
  if(length(y) == 0){
    return(0)
  }else{
    return(abs(sum(y)))
  }
})

pos_neg_scaling <- apply(rbind(counts_pos, counts_neg), 2, function(x) return(x/sum(x)))
pos_ic <- pos_neg_scaling[1, ] * ic
neg_ic <- pos_neg_scaling[2, ] * ic

if (ic.scale){
  if(yscale_change){
    if(max(ic)<1){ylim <- 1
    facs <- ic + 1 - max(ic)}
    if(max(ic)>1){ylim <- ceiling(max(pos_ic))
    facs <- ic}
  }else{
    ylim <- ceiling(max(pos_ic))
    facs <- ic
  }
  ylab <- ylab
  facs <- ic
}else{
  ylim <- 1
  ylab <- ylab
  facs <- rep(1, npos)
}

x.pos <- 0
slash_inds <- grep("/", chars)



#cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))
col_vector <- c("blue", "green", "red", "orange")
color_profile <- list("type" = "per_row",
                       "col" = col_vector[1:4])
hist=FALSE
total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
frame_width=NULL
ic.scale=TRUE
alpha=1
xaxis=TRUE
yaxis=TRUE
xaxis_fontsize= 12
xlab_fontsize=15
y_fontsize=15
main_fontsize=16
start=0.001
yscale_change=TRUE
pop_name = NULL
xlab = "Position"
ylab = "Information content"
col_line_split="grey80"
scale0=0.01
scale1=0.99
addlogos = NULL
addlogos_text = NULL
newpage = TRUE

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

letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
npos <- ncol(table_mat_norm)


counts_pos_matrix <- counts_mat_adj
counts_pos_matrix[counts_mat_adj<= 0] = 0
counts_pos_mat_norm  <- apply(counts_pos_matrix, 2, function(x) return(x/sum(x)))
counts_pos_mat_norm[counts_pos_mat_norm == "NaN"] = 0


counts_neg_matrix <- counts_mat_adj
counts_neg_matrix[counts_mat_adj >= 0] = 0
counts_neg_mat_norm  <- apply(counts_neg_matrix, 2, function(x) return(x/sum(x)))
counts_neg_mat_norm[counts_neg_mat_norm == "NaN"] = 0

facs <- pos_ic
for (j in seq_len(npos)){
  column <- counts_pos_mat_norm[,j]
  hts <- as.numeric(0.99*column*facs[j])
  letterOrder <- order(hts)

  y.pos <- 0
  for (i in seq_along(chars)){
    letter <- chars[letterOrder[i]]
    col <- color_profile$col[letterOrder[i]]
    ht <- hts[letterOrder[i]]
    if(length(intersect(letterOrder[i], slash_inds))!=0){
      if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
    }else{
      if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
    }
    y.pos <- y.pos + ht + start
  }
  x.pos <- x.pos + wt[j]
}

xlim <- cumsum(wt) - wt/2;
# xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])

if(ic.scale){
  if(yscale_change){
    if(ylim  >= 1){
   #   letters$y <- letters$y*(ylim/max(pos_ic)) + max(neg_ic);
    }
  }}

letters$y <- letters$y + max(abs(neg_ic))
letters$y <- 0.8*letters$y*(ylim/max(max(pos_ic), max(neg_ic)))
y1 <- min(letters$y)
max1 <- max(letters$y)
ylim <- max1
ylim_scale <- seq(0, max1, length.out=6);

negbins <- ceiling((y1/max1)*6)
posbins <- 6 - negbins
ic_lim_scale <- c(seq(0, y1, length.out = negbins),
                seq(y1, max1, length.out = posbins))


if(newpage){
  grid::grid.newpage()
}
#  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)
bottomMargin = ifelse(xaxis, 1 + xaxis_fontsize/3.5, 3)
#  leftMargin = ifelse(yaxis, 0.1 + y_fontsize/3.5, 3)
leftMargin = ifelse(yaxis, 1 + y_fontsize/3.5, 3)
grid::pushViewport(grid::plotViewport(c(bottomMargin,leftMargin,max(ylim)+0.5,max(ylim))))
# pushViewport(viewport(layout = grid.layout(2, 2),
#              x = bottomMargin,
#              y = leftMargin,
#              width = max(xlim/2)+0.5,
#              height = max(ylim/2)+0.5))
grid::pushViewport(grid::dataViewport(0:ncol(table_mat_norm),0:1,name="vp1"))
grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                   id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                   id=letters$id,
                   gp=grid::gpar(fill=letters$fill,col="transparent"))


for(n in 2:length(xlim)){
  grid::grid.lines(x = grid::unit(low_xlim[n], "native"),
                   y = grid::unit(c(0, max1), "native"),
                   gp=grid::gpar(col=col_line_split))
}

if(is.null(pop_name)){
  grid::grid.text("Neg Logo plot:", y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
                  gp = grid::gpar(fontsize = main_fontsize))
}else{
  grid::grid.text(paste0("", pop_name),
                  y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
                  gp = grid::gpar(fontsize = main_fontsize))
}

if (xaxis){
  grid::grid.xaxis(at=wt*seq(0.5,ncol(table_mat_norm)-0.5),
                   label=colnames(table_mat_norm),
                   gp=grid::gpar(fontsize=xaxis_fontsize))
  grid::grid.text(xlab, y=grid::unit(-3,"lines"),
                  gp=grid::gpar(fontsize=xaxis_fontsize))
}
if (yaxis){
  if(yscale_change==TRUE){
    grid::grid.yaxis(at = c(ic_lim_scale, y1),
                     label = round(c(ic_lim_scale, y1),2) - round(y1,2),
                     gp=grid::gpar(fontsize=y_fontsize))
  }else{
    grid::grid.yaxis(gp=grid::gpar(fontsize=y_fontsize))
  }
  grid::grid.text(ylab,x=grid::unit(-3.5,"lines"),rot=90,
                  gp=grid::gpar(fontsize=y_fontsize))
}


##################   negative component   ###################

x.pos <- 0
letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
npos <- ncol(table_mat_norm)

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

letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
facs <- neg_ic
ylim <- 1
for (j in seq_len(npos)){
  column <- counts_neg_mat_norm[,j]
  hts <- as.numeric(0.99*column*facs[j])
  letterOrder <- rev(order(hts))

  y.pos <- - neg_ic[j]
  for (i in seq_along(chars)){
    letter <- chars[letterOrder[i]]
    col <- color_profile$col[letterOrder[i]]
    ht <- hts[letterOrder[i]]
    if(length(intersect(letterOrder[i], slash_inds))!=0){
      if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
    }else{
      if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
    }
    y.pos <- y.pos + ht + start
  }
  x.pos <- x.pos + wt[j]
}

letters$y <- letters$y + max(abs(neg_ic))
letters$y <- 0.8*letters$y*(ylim/max(max(pos_ic), max(neg_ic)))

xlim <- cumsum(wt) - wt/2;
# xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
ylim_scale <- seq(0, ylim, length.out=6);
if(ic.scale){
  ic_lim_scale <- seq(-max(neg_ic), 0, length.out=6)
}else{
  ic_lim_scale <- ylim_scale
}
if(ic.scale){
  if(yscale_change){
    if(ylim2  >= -1){
     # letters$y <- letters$y*(ylim/max(neg_ic)) ;
    }
  }}


#  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)

grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                   id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                   id=letters$id,
                   gp=grid::gpar(fill=letters$fill,col="transparent"))

grid::grid.lines(x = grid::unit(c(0, (xlim+0.5*wt)), "native"),
                 y = grid::unit(y1, "native"),
                 gp=grid::gpar(col="black"))
# for(n in 2:length(xlim)){
#   grid::grid.lines(x = grid::unit(low_xlim[n], "native"),
#                    y = grid::unit(c(0, ylim), "native"),
#                    gp=grid::gpar(col=col_line_split))
# }
#
# if(is.null(pop_name)){
#   grid::grid.text("Logo plot:", y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
#                   gp = grid::gpar(fontsize = main_fontsize))
# }else{
#   grid::grid.text(paste0("", pop_name),
#                   y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
#                   gp = grid::gpar(fontsize = main_fontsize))
# }
#
# if (xaxis){
#   grid::grid.xaxis(at=wt*seq(0.5,ncol(table_mat_norm)-0.5),
#                    label=colnames(table_mat_norm),
#                    gp=grid::gpar(fontsize=xaxis_fontsize))
#   grid::grid.text(xlab, y=grid::unit(-3,"lines"),
#                   gp=grid::gpar(fontsize=xaxis_fontsize))
# }
# if (yaxis){
#   if(yscale_change==TRUE){
#     grid::grid.yaxis(at = ylim_scale,
#                      label = round(ic_lim_scale,1),
#                      gp=grid::gpar(fontsize=y_fontsize))
#   }else{
#     grid::grid.yaxis(gp=grid::gpar(fontsize=y_fontsize))
#   }
#   grid::grid.text(ylab,x=grid::unit(-3,"lines"),rot=90,
#                   gp=grid::gpar(fontsize=y_fontsize))
# }
#
#


grid::popViewport()
grid::popViewport()
