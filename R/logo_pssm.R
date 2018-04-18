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
#' @param color_type A list specifying the coloring scheme. Defaults to NULL,
#'                  for which, based on \code{color_seed}, a specific 
#'                  coloring scheme is chosen. The list contains 
#'                  two elements - \code{type} and \code{col}.The \code{type} 
#'                  can be of three types - "per-row", "per-column" and
#'                  "per-symbol". The \code{col} element is a vector of colors,
#'                   of same length as number of rows in table for "per-row"
#'                   (assigning a color to each string), of same length
#'                  as number of columns in table for "per-column" 
#'                  (assuming a color for each column), or a distinct color 
#'                  for a distinct symbol in "per-symbol". For "per-symbol", 
#'                  the length of the \code{color_profile$col} should be same
#'                  as library size of the logos, but if the vector of colors 
#'                  provided is more or less, we can downsample or upsample
#'                  the colors as required. The colors are matched with the 
#'                  symbols in the \code{total_chars}.
#'
#'
#' @param colors Add description here.
#'
#' @param color_seed Add description here.
#' 
#' @param total_chars The total number of character symbols in the user library. 
#' The default is the default library provided by Logolas, but the user can add
#' symbols that he creates to this list.
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
#' @param col_line_split The color of the line split between the consecutive 
#'     groups or blocks
#'
#' @param ylimit The limit of the Y axis.
#'
#' @param addlogos Vector of additional logos/symbols defined by user
#' @param addlogos_text Vector of the names given to the additional 
#'                      logos/symbols defined by user.
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
#' @importFrom  utils  modifyList
#' @examples
#'
#' data(pssm)
#' logo_pssm(pssm, control = list(round_off = 0))
#'
#' @importFrom stats median
#' @export
#'
logo_pssm <- function(table,
                      color_type = NULL,
                      colors = NULL,
                      color_seed = 2030,
                      total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", 
                                      "I", "J", "K", "L", "M", "N", "O",
                                       "P", "Q", "R", "S", "T", "U", "V", "W",
                                      "X", "Y", "Z", "zero", "one", "two",
                                       "three", "four", "five", "six", "seven",
                                      "eight", "nine", "dot", "comma",
                                       "dash", "colon", "semicolon", 
                                      "leftarrow", "rightarrow"),
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
                          lwd = 2, gap_xlab = 3, gap_ylab = 3, quant = 0,
                          totbins = 6, minbins = 2, round_off = 2,
                          posbins = NULL, negbins = NULL,
                          viewport.margin.bottom = NULL,
                          viewport.margin.left = NULL,
                          viewport.margin.top = NULL,
                          viewport.margin.right = NULL,
                          use_seqLogo_heights = FALSE)

  control <- modifyList(control.default, control)
  scale0 <- control$scale0
  scale1 <- control$scale1
  quant <- control$quant

  if(is.null(color_type)){
    message("color_type not provided, so switching to per_row option 
            for color_type")
    color_type = "per_row"
  }
  if(color_type == "per_row"){
    if(is.null(colors)){
      cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, 
                                 rownames(cols)))
      set.seed(color_seed)
      color_profile <- list("type" = color_type,
                            "col" = sample(col_vector, dim(table)[1], 
                                           replace = FALSE))
    }else{
      if (length(colors) < dim(table)[1]){
        stop("For per_row color type, the colors vector must be as large 
              as number of rows in the matrix for PFM/PWM input, or number 
              of distinct characters in each aligned sequence for sequence 
              data")
      }
      set.seed(color_seed)
      color_profile <- list("type" = color_type,
                            "col" = sample(colors, dim(table)[1],
                                           replace = FALSE))
      }
    }
  if(color_type == "per_symbol"){
    if(is.null(colors)){
      cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, 
                                 rownames(cols)))
      set.seed(color_seed)
      color_profile <- list("type" = color_type,
                            "col" = sample(col_vector, 
                                           length(control$total_chars),
                                           replace=FALSE))
    }else{
      if (length(colors) < length(control$total_chars)){
        stop("For per_symbol color type, the colors vector must be as large 
              as number of symbols in total_chars argument in control() : 
             which is 50 by default ")
      }
      set.seed(color_seed)
      color_profile <- list("type" = color_type,
                            "col" = sample(colors, length(control$total_chars),
                                           replace=FALSE))
      }
  }
  if(color_type == "per_column"){
    if(is.null(colors)){
      cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors,
                                 rownames(cols)))
      set.seed(color_seed)
      color_profile <- list("type" = color_type,
                            "col" = sample(col_vector, dim(table)[2],
                                           replace = FALSE))
    }else{
      if (length(colors) < dim(table)[2]){
        stop("For per_column color type, the colors vector must be as large 
              as number of columns in the matrix for PFM/PWM input, or number 
              of characters in each aligned sequence for sequence data")
      }
      set.seed(color_seed)
      color_profile <- list("type" = color_type,
                            "col" = sample(colors, dim(table)[2], 
                                           replace = FALSE))
      }
    }

  table_mat_adj <- apply(table, 2, function(x)
  {
    indices <- which(is.na(x))
    if(length(indices) == 0){
      y = x
      if(quant != 0){
        qq <- quantile(y, quant)
      }else{
        qq <- 0
      }
      z <- y - qq
      return(z)
    }else{
      y <- x[!is.na(x)]
      if(quant != 0){
        qq <- quantile(y, quant)
      }else{
        qq <- 0
      }
      z <- y - qq
      zext <- array(0, length(x))
      zext[indices] <- 0
      zext[-indices] <- z
      return(zext)
    }
  })


  table_mat_pos <- table_mat_adj
  table_mat_pos[table_mat_pos<= 0] = 0
  table_mat_pos_norm  <- apply(table_mat_pos, 2, function(x) return(x/sum(x)))
  table_mat_pos_norm[table_mat_pos_norm == "NaN"] = 0

  table_mat_neg <- table_mat_adj
  table_mat_neg[table_mat_neg >= 0] = 0
  table_mat_neg_norm  <- apply(abs(table_mat_neg), 2, function(x) 
                                 return(x/sum(x)))
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
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, col, 
                                              total_chars, x.pos, y.pos, 
                                              ht, wt[j], scale0 = scale0,
                                              scale1=scale1, 
                                              addlogos = addlogos, 
                                              addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, 
                                              col, total_chars, x.pos, y.pos, 
                                              ht, wt[j], scale0 = scale0, 
                                              scale1=scale1, addlogos = NULL, 
                                              addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill, 
                                              lwd = control$lwd, 
                                              color_profile$col, 
                                              total_chars, x.pos, y.pos, ht, 
                                              wt[j], scale0 = scale0, 
                                              scale1=scale1, 
                                              addlogos = addlogos, 
                                              addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, 
                                              color_profile$col, 
                                              total_chars, x.pos, y.pos, ht, 
                                              wt[j], scale0 = scale0,
                                              scale1=scale1, addlogos = NULL,
                                              addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd,
                                              color_profile$col[j],
                                              total_chars, x.pos, y.pos,
                                              ht, wt[j], scale0 = scale0,
                                              scale1=scale1, 
                                              addlogos = addlogos,
                                              addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, 
                                              color_profile$col[j], 
                                              total_chars, x.pos, y.pos,
                                              ht, wt[j], scale0 = scale0, 
                                              scale1=scale1, addlogos = NULL, 
                                              addlogos_text = NULL)
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

  if(is.null(control$negbins)){
    negbins <- max(ceiling((y1/max1)*6), control$minbins)
  }else{
    negbins <- control$negbins
  }

  if(is.null(control$posbins)){
    posbins <- max(control$totbins - negbins, control$minbins)
  }else{
    posbins <- control$posbins
  }

  ic_lim_scale <- c(seq(0, y1, length.out = negbins),
                    seq(y1, ylim, length.out = posbins))

  letters$y <- letters$y/ylim

  markers <- round(ic_lim_scale,control$round_off) - round(y1,control$round_off)

  if(newpage){
    grid::grid.newpage()
  }
  #  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)

  if(control$use_seqLogo_heights){
    if(is.null(control$viewport.margin.bottom))
    {bottomMargin <- ifelse(xaxis, 1 + xaxis_fontsize/3.5, 3)}
      else{bottomMargin <- control$viewport.margin.bottom}
    if(is.null(control$viewport.margin.left))
    {leftMargin <- ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)}
      else{leftMargin <- control$viewport.margin.left}
    if(is.null(control$viewport.margin.top))
    {topMargin <- max(ylim)+0.5}
      else{topMargin <- control$viewport.margin.top}
    if(is.null(control$viewport.margin.right))
    {rightMargin <- max(ylim)}
      else{rightMargin <- control$viewport.margin.right}
  }else{

    if(is.null(control$viewport.margin.bottom))
        {control$viewport.margin.bottom = 3}
    if(is.null(control$viewport.margin.left))
        {control$viewport.margin.left = 5}
    if(is.null(control$viewport.margin.top))
        {control$viewport.margin.top = 2.5}
    if(is.null(control$viewport.margin.right))
        {control$viewport.margin.right = 2.5}


    topMargin <- control$viewport.margin.top
    rightMargin <- control$viewport.margin.right
    leftMargin <- control$viewport.margin.left
    bottomMargin <- control$viewport.margin.bottom
  }

  grid::pushViewport(grid::plotViewport(c(bottomMargin, leftMargin,
                                          topMargin, rightMargin)))

  # pushViewport(viewport(layout = grid.layout(2, 2),
  #              x = bottomMargin,
  #              y = leftMargin,
  #              width = max(xlim/2)+0.5,
  #              height = max(ylim/2)+0.5))
  grid::pushViewport(grid::dataViewport(0:ncol(table),0:1,name="vp1"))
  if(control$tofill_pos){
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(fill=letters$fill, col="transparent"))
  }else{
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(col=letters$colfill, lwd = control$lwd))
  }


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
                     label = round(ic_lim_scale,control$round_off) - 
                         round(y1,control$round_off),
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
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, col, 
                                              total_chars, x.pos, y.pos, ht,
                                              wt[j], scale0 = scale0, 
                                              scale1=scale1, 
                                              addlogos = addlogos, 
                                              addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill, 
                                              lwd = control$lwd, col, 
                                              total_chars, x.pos, y.pos, ht,
                                              wt[j], scale0 = scale0, 
                                              scale1=scale1, addlogos = NULL, 
                                              addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, 
                                              color_profile$col, total_chars,
                                              x.pos, y.pos, ht, wt[j], 
                                              scale0 = scale0, scale1=scale1,
                                              addlogos = addlogos, 
                                              addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill, 
                                              lwd = control$lwd, 
                                              color_profile$col, total_chars,
                                              x.pos, y.pos, ht, wt[j], 
                                              scale0 = scale0, scale1=scale1,
                                              addlogos = NULL, 
                                              addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter_pssm(letters,letter, 
                                              tofill = control$tofill,
                                              lwd = control$lwd, 
                                              color_profile$col[j],
                                              total_chars, x.pos, y.pos,
                                              ht, wt[j], scale0 = scale0, 
                                              scale1=scale1, 
                                              addlogos = addlogos,
                                              addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_pssm(letters,letter,
                                              tofill = control$tofill,
                                              lwd = control$lwd,
                                              color_profile$col[j],
                                              total_chars, x.pos, y.pos,
                                              ht, wt[j], scale0 = scale0,
                                              scale1=scale1, addlogos = NULL,
                                              addlogos_text = NULL)
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
  low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
  ylim_scale <- seq(0, ylim, length.out=6);
  ic_lim_scale <- seq(-max(neg_ic), 0, length.out=6)

  
  if(control$tofill_neg){
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(fill=letters$fill, col="transparent"))
  }else{
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(col=letters$colfill, lwd = control$lwd))
  }


  grid::grid.lines(x = grid::unit(c(0, (xlim+0.5*wt)), "native"),
                   y = grid::unit(y1/ylim, "native"),
                   gp=grid::gpar(col="black"))
  grid::popViewport()
  grid::popViewport()
}

addLetter_pssm <- function(letters, letter, tofill, lwd,
                      col, total_chars, x.pos, y.pos, ht, wt,
                      scale0=0.01, scale1=0.99,
                      addlogos=NULL, addlogos_text=NULL){
  letter <- toupper(letter)
  out <- makemylogo(letter,
                    tofill = tofill,
                    colfill = col,
                    lwd = lwd,
                    total_chars = total_chars,
                    addlogos=addlogos,
                    addlogos_text = addlogos_text)
  x <- x.pos + out$x * wt
  y <- y.pos + (scale1*out$y+scale0) * ht

  letter <- list("x"=x,
                 "y"=y,
                 "id"=out$id,
                 "fill"=out$fill,
                 "colfill" = out$colfill)

  letters$x <- c(letters$x,letter$x)
  letters$y <- c(letters$y,letter$y)

  lastID <- ifelse(is.null(letters$id),0,max(letters$id))
  letters$id <- c(letters$id,lastID+letter$id)
  letters$fill <- c(letters$fill,letter$fill)
  letters$colfill <- c(letters$colfill,letter$colfill)
  return(letters)
}


normalize = function(x){return(x/sum(x[!is.na(x)]))}

