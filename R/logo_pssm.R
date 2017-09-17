#' @title Function to plot PSSM logo plot visualization.
#'
#' @description stacks logos created by the \code{makemylogo} function on top of
#' each other to build the PSSM logo plot.
#'
#' @param table The input table (data frame or matrix) of PSSM scores
#' (comprising of both positive and negative scores) across different
#' logos or symbols (specified along the rows) and across different sites or
#' positions or groups (specified along the columns).
#'
#' @param color_profile A list containing two elements - "type" and "col". The type can
#' be of three types - "per-row", "per-column" and "per-symbol". The "col" element
#' is a vector of colors, of same length as number of rows in table for "per-row" (assigning
#' a color to each string), of same length as number of columns in table for "per-column"
#' (assuming a color for each column), or a distinct color for a distinct symbol in "per-symbol".
#' For "per-symbol", the length of the \code{color_profile$col} should be same as library size
#' of the logos, but if the vector of colors provided is more or less, we can
#' downsample or upsample the colors as required. The colors are matched with the symbols in
#' the \code{total_chars}
#'
#' @param total_chars The total number of character symbols in the user library. The default
#' is the default library provided by Logolas, but the user can add symbols that he creates
#' to this list.
#'
#' @param frame_width The width of the frames for individual site/postion/column
#' in the logo plot. As default, all the columns have same width, equal to 1.
#'
#' @param xaxis Binary specifying if there should be a X axis in the logo plot
#' or not. Defaults to TRUE.
#'
#' @param yaxis Binary specifying if there should be a Y axis in the logo plot
#' or not. Defaults to TRUE.
#'
#' @param xaxis_fontsize The size of the X-axis axis ticks.
#'
#' @param xlab_fontsize The size of the X-axis label.
#'
#' @param y_fontsize The size of the Y-axis font.
#'
#' @param main_fontsize The size of the title.
#'
#' @param yscale_change If TRUE, adjusts the Y axis scale based on the size of
#' the bars, else keeps it to the maximum value possible, which is
#' \code{ceiling(max(ic)} under \code{ic_computer} defined IC criteria.
#'
#' @param start The starting point in Y axis for the first logo. Default is
#' 0.0001 which is very close to 0.
#'
#' @param pop_name User can mention a name of the population for which the logo
#' plot is created. Defaults to NULL when no population name is mentioned.
#'
#' @param xlab X axis label
#' @param ylab Y axis label
#'
#' @param col_line_split The color of the line split between the consecutive groups
#' or blocks
#'
#' @param ylimit The limit of the Y axis.
#'
#' @param scale1 scaling of the logo to maintain the gap between symbols.
#'
#' @param scale0 the base change of the logo to maintain the gap between symbols.
#'
#' @param addlogos Vector of additional logos/symbols defined by user
#' @param addlogos_text Vector of the names given to the additional logos/symbols defined by user.
#'
#' @param newpage if TRUE, plots the logo plot in a new page. Defaults to TRUE.
#'
#' @param control control parameters fixing whether the
#' symbols should be filled with color or border colored (\code{tofill_pos,
#' tofill_neg}), the viewport configuration details for the plot
#' (\code{viewport.margin.bottom}, \code{viewport.margin.left},
#' \code{viewport.margin.top}, \code{viewport.margin.right})  etc.
#'
#' @return Plots the logo plot for the PSSM scoring data, with column names
#' representing the sites/blocks and the row names denoting the symbols
#' for which logos are plotted
#'
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' @importFrom stats median
#' @export
#'



logo_pssm <- function(table,
                       color_profile,
                       total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                                       "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                                       "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                                       "dash", "colon", "semicolon", "leftarrow", "rightarrow"),
                       frame_width=NULL,
                       yscale_change=TRUE,
                       pop_name = NULL,
                       addlogos = NULL,
                       addlogos_text = NULL,
                       newpage = TRUE,
                       ylimit = NULL,
                       xaxis=TRUE,
                       yaxis=TRUE,
                       xaxis_fontsize=10,
                       xlab_fontsize=15,
                       y_fontsize=15,
                       main_fontsize=16,
                       start=0.001,
                       xlab = "X",
                       ylab = "PSSM  Score",
                       col_line_split="grey80",
                       control = list()){

  control.default <- list(scale0=0.01,
                          scale1=0.99, tofill_pos = TRUE, tofill_neg = TRUE,
                          lwd = 2, gap_xlab = 3, gap_ylab = 3,
                          viewport.margin.bottom = NULL,
                          viewport.margin.left = NULL,
                          viewport.margin.top = NULL,
                          viewport.margin.right = NULL,
                          use_seqLogo_heights = FALSE)

  control <- modifyList(control.default, control)
  scale0 <- control$scale0
  scale1 <- control$scale1

  table_mat_pos <- table
  table_mat_pos[table_mat_pos<= 0] = 0
  table_mat_pos_norm  <- apply(table_mat_pos, 2, function(x) return(x/sum(x)))
  table_mat_pos_norm[table_mat_pos_norm == "NaN"] = 0

  table_mat_neg <- table
  table_mat_neg[table_mat_neg >= 0] = 0
  table_mat_neg_norm  <- apply(abs(table_mat_neg), 2, function(x) return(x/sum(x)))
  table_mat_neg_norm[table_mat_neg_norm == "NaN"] = 0

  pos_ic <- colSums(table_mat_pos)
  neg_ic <- colSums(abs(table_mat_neg))

  chars <- as.character(rownames(table))
  npos <- ncol(table)


  if(color_profile$type == "per_column"){
    if(length(color_profile$col) != npos){
      stop("number of colors must equal the number of columns of the table")
    }
  }

  if(color_profile$type == "per_row"){
    if(length(color_profile$col) != nrow(table)){
      stop("the number of colors must match the number of rows of the table")
    }
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



  #####################  positive component study  ###########################

  letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
  facs <- pos_ic

  ylim <- ceiling(max(pos_ic))
  x.pos <- 0
  slash_inds <- grep("/", chars)

  if(color_profile$type == "per_row"){
    for (j in seq_len(npos)){

      column <- table_mat_pos_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- order(hts)

      y.pos <- 0
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        col <- color_profile$col[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }

  if(color_profile$type == "per_symbol"){
    for (j in seq_len(npos)){

      column <- table_mat_pos_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- order(hts)

      y.pos <- 0
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }


  if(color_profile$type == "per_column"){
    for (j in seq_len(npos)){

      column <- table_mat_pos_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- order(hts)
      y.pos <- 0
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }

  xlim <- cumsum(wt) - wt/2;
  # xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
  low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])


  letters$y <- letters$y + max(abs(neg_ic))
  # letters$y <- 0.8*letters$y*(ylim/max(max(pos_ic), max(neg_ic)))

  y1 <- min(letters$y)
  max1 <- max(letters$y)
  if(is.null(ylimit)){
    ylimit <- ceiling(max(pos_ic) + max(neg_ic))
  }
  ylim <- ylimit
  ylim_scale <- seq(0, ylim, length.out=6);

  negbins <- ceiling((y1/max1)*6)
  posbins <- 6 - negbins
  ic_lim_scale <- c(seq(0, y1, length.out = negbins),
                    seq(y1, ylim, length.out = posbins))

  letters$y <- letters$y/ylim

  markers <- round(ic_lim_scale,2) - round(y1,2)

  if(newpage){
    grid::grid.newpage()
  }
  #  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)

  if(control$use_seqLogo_heights){
    if(is.null(control$viewport.margin.bottom)){bottomMargin <- ifelse(xaxis, 1 + xaxis_fontsize/3.5, 3)}else{bottomMargin <- control$viewport.margin.bottom}
    if(is.null(control$viewport.margin.left)){leftMargin <- ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)}else{leftMargin <- control$viewport.margin.left}
    if(is.null(control$viewport.margin.top)){topMargin <- max(ylim)+0.5}else{topMargin <- control$viewport.margin.top}
    if(is.null(control$viewport.margin.right)){rightMargin <- max(ylim)}else{rightMargin <- control$viewport.margin.right}
  }else{

    if(is.null(control$viewport.margin.bottom)){control$viewport.margin.bottom = 3}
    if(is.null(control$viewport.margin.left)){control$viewport.margin.left = 5}
    if(is.null(control$viewport.margin.top)){control$viewport.margin.top = 2.5}
    if(is.null(control$viewport.margin.right)){control$viewport.margin.right = 2.5}


    topMargin <- control$viewport.margin.top
    rightMargin <- control$viewport.margin.right
    leftMargin <- control$viewport.margin.left
    bottomMargin <- control$viewport.margin.bottom
  }

  grid::pushViewport(grid::plotViewport(c(bottomMargin, leftMargin, topMargin, rightMargin)))

  # pushViewport(viewport(layout = grid.layout(2, 2),
  #              x = bottomMargin,
  #              y = leftMargin,
  #              width = max(xlim/2)+0.5,
  #              height = max(ylim/2)+0.5))
  grid::pushViewport(grid::dataViewport(0:ncol(table),0:1,name="vp1"))
  if(control$tofill_pos){
    # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
    #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
    grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(fill=letters$fill, col="transparent"))
  }else{
    grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(col=letters$colfill, lwd = control$lwd))
  }


  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id,
  #                    gp=grid::gpar(fill=letters$fill,col="transparent"))


  for(n in 2:length(xlim)){
    grid::grid.lines(x = grid::unit(low_xlim[n], "native"),
                     y = grid::unit(c(0, max(markers)/ylim), "native"),
                     gp=grid::gpar(col=col_line_split))
  }

  if(is.null(pop_name)){
    grid::grid.text(paste0("PSSM Logo plot:"), y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
                    gp = grid::gpar(fontsize = main_fontsize))
  }else{
    grid::grid.text(paste0(pop_name),
                    y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
                    gp = grid::gpar(fontsize = main_fontsize))
  }

  if (xaxis){
    grid::grid.xaxis(at=wt*seq(0.5,ncol(table)-0.5),
                     label=colnames(table),
                     gp=grid::gpar(fontsize=xaxis_fontsize))
    grid::grid.text(xlab, y=grid::unit(-control$gap_xlab,"lines"),
                    gp=grid::gpar(fontsize=xaxis_fontsize))
  }
  if (yaxis){
    # if(yscale_change==TRUE){
    grid::grid.yaxis(at = ic_lim_scale/ylim,
                     label = round(ic_lim_scale,2) - round(y1,2),
                     gp=grid::gpar(fontsize=y_fontsize))
    # }else{
    #   grid::grid.yaxis(gp=grid::gpar(fontsize=y_fontsize))
    # }
    grid::grid.text(ylab,x=grid::unit(-control$gap_ylab,"lines"),rot=90,
                    gp=grid::gpar(fontsize=y_fontsize))
  }



  ####################   negative component study  #########################


  x.pos <- 0
  letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
  npos <- ncol(table)

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
  ylim <- ylimit

  slash_inds <- grep("/", chars)

  if(color_profile$type == "per_row"){
    for (j in seq_len(npos)){

      column <- table_mat_neg_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- rev(order(hts))

      y.pos <- - neg_ic[j]
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        col <- color_profile$col[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }

  if(color_profile$type == "per_symbol"){
    for (j in seq_len(npos)){

      column <- table_mat_neg_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- rev(order(hts))

      y.pos <- - neg_ic[j]
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }

  if(color_profile$type == "per_column"){
    for (j in seq_len(npos)){

      column <- table_mat_neg_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- rev(order(hts))
      y.pos <- - neg_ic[j]
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, tofill = control$tofill, lwd = control$lwd, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }

  letters$y <- letters$y + max(abs(neg_ic))
  #  letters$y <- 0.8*letters$y*(ylim/max(max(pos_ic), max(neg_ic)))
  letters$y <- letters$y/ylim

  xlim <- cumsum(wt) - wt/2;
  # xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
  low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
  ylim_scale <- seq(0, ylim, length.out=6);
  ic_lim_scale <- seq(-max(neg_ic), 0, length.out=6)

  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id,
  #                    gp=grid::gpar(fill=letters$fill,col="transparent"))

  if(control$tofill_neg){
    # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
    #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
    grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(fill=letters$fill, col="transparent"))
  }else{
    grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(col=letters$colfill, lwd = control$lwd))
  }


  grid::grid.lines(x = grid::unit(c(0, (xlim+0.5*wt)), "native"),
                   y = grid::unit(y1/ylim, "native"),
                   gp=grid::gpar(col="black"))
  grid::popViewport()
  grid::popViewport()
}

normalize = function(x){return(x/sum(x[!is.na(x)]))}

