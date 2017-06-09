#' @title Main workhorse function that builds negative logo plots
#'
#' @description stacks logos created by the \code{makemylogo} function on top of
#' each other to build the logo plot.
#'
#' @param table The input table (data frame or matrix) of counts across different
#' logos or symbols (specified along the rows) ans across different sites or
#' positions or groups (specified along the columns).
#'
#' @param hist Whether to use the hist method or the information criterion
#' method to determine the heights of the logos.
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
#' @param alpha The Renyi entropy tuning parameter which is used in case of
#' scaling of the bar heights by information criterion. The default tuning
#' parameter value is 1, which corresponds to Shannon entropy.
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
#' @return Plots the logo plot for the table data, with column names representing
#' the sites/blocks and the row names denoting the symbols for which logos are
#' plotted
#'
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' mFile <- system.file("Exfiles/pwm1", package="seqLogo")
#' m <- read.table(mFile)
#' p <- seqLogo::makePWM(m)
#' pwm_mat <- slot(p,name = "pwm")
#' pwm_mat[,4] <- c(0.3, 0.3, 0.35, 0.05)
#' mat1 <- cbind(pwm_mat[,c(3,4)], rep(NA,4), pwm_mat[,c(5,6)]);
#' colnames(mat1) <- c("-2", "-1", "0", "1", "2")
#' mat2 <- cbind(rep(NA,6), rep(NA,6),
#'              c(0.8, 0.10, 0.03, 0.03, 0.0, 0),
#'              rep(NA,6), rep(NA,6))
#' rownames(mat2) <- c("C>T", "C>A", "C>G",
#'                     "T>A", "T>C", "T>G")
#'
#' table <- rbind(mat1, mat2)
#'
#' cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
#' col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))
#'
#' total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
#'                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
#'                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
#'                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
#'
#' set.seed(20)
#' col_vector[c(1,3,7, 20, 43)] <- c("red", "blue", "orange", "green", "gray")
#' color_profile <- list("type" = "per_symbol",
#'                      "col" = col_vector)
#'
#' nlogomaker(table,
#'            color_profile = color_profile,
#'            ylimit = 1.2)
#'
#'
#' @importFrom stats median
#' @export

nlogomaker <- function( table,
                        hist = FALSE,
                        color_profile,
                        total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                                       "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                                       "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                                       "dash", "colon", "semicolon", "leftarrow", "rightarrow"),
                       frame_width=NULL,
                       alpha=1,
                       xaxis=TRUE,
                       yaxis=TRUE,
                       xaxis_fontsize=10,
                       xlab_fontsize=15,
                       y_fontsize=15,
                       main_fontsize=16,
                       start=0.001,
                       yscale_change=TRUE,
                       pop_name = NULL,
                       xlab = "X",
                       ylab = "Enrichment Score",
                       col_line_split="grey80",
                       ylimit = 1.5,
                       scale0=0.01,
                       scale1=0.99,
                       addlogos = NULL,
                       addlogos_text = NULL,
                       newpage = TRUE){

  ########  data preprocessing for positive and negative scales  ##########
  table <- apply(table+0.0001,2,normalize)

  if (class(table) == "data.frame"){
    table <- as.matrix(table)
  }else if (class(table) != "matrix"){
    stop("the table must be of class matrix or data.frame")
  }

  table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x[!is.na(x)])))
  npos <- ncol(table_mat_norm)
  chars <- as.character(rownames(table_mat_norm))

  table_mat_adj <- apply(table_mat_norm, 2, function(x)
  {
    indices <- which(is.na(x))
    if(length(indices) == 0){
      y = x
      z <- y - median(y)
      return(z)
    }else{
      y <- x[!is.na(x)]
      z <- y - median(y)
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
  table_mat_neg_norm  <- apply(table_mat_neg, 2, function(x) return(x/sum(x)))
  table_mat_neg_norm[table_mat_neg_norm == "NaN"] = 0

  table_mat_norm <- replace(table_mat_norm, is.na(table_mat_norm), 0)


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


##################   obtaining ic for positive and negative scales  ##########

  ic <- ic_computer(table_mat_norm, alpha, hist=hist)
  tab_neg <- apply(table_mat_adj, 2, function(x) {
    y = x[x < 0]
    if(length(y) == 0){
      return(0)
    }else{
      return(abs(sum(y)))
    }
  })

  tab_pos <- apply(table_mat_adj, 2, function(x) {
    y = x[x > 0]
    if(length(y) == 0){
      return(0)
    }else{
      return(abs(sum(y)))
    }
  })


  pos_neg_scaling <- apply(rbind(tab_pos, tab_neg), 2, function(x) return(x/sum(x)))
  pos_ic <- pos_neg_scaling[1, ] * ic
  neg_ic <- pos_neg_scaling[2, ] * ic


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
          if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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
                     y = grid::unit(c(0, max(markers)/ylim), "native"),
                     gp=grid::gpar(col=col_line_split))
  }

  if(is.null(pop_name)){
    grid::grid.text("Neg Logo plot:", y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
                    gp = grid::gpar(fontsize = main_fontsize))
  }else{
    grid::grid.text(paste0(pop_name),
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
   # if(yscale_change==TRUE){
      grid::grid.yaxis(at = ic_lim_scale/ylim,
                       label = round(ic_lim_scale,2) - round(y1,2),
                       gp=grid::gpar(fontsize=y_fontsize))
    # }else{
    #   grid::grid.yaxis(gp=grid::gpar(fontsize=y_fontsize))
    # }
    grid::grid.text(ylab,x=grid::unit(-3.5,"lines"),rot=90,
                    gp=grid::gpar(fontsize=y_fontsize))
  }



####################   negative component study  #########################


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
          if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, color_profile$col[j], total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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



  grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                     id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
  grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
                     id=letters$id,
                     gp=grid::gpar(fill=letters$fill,col="transparent"))

  grid::grid.lines(x = grid::unit(c(0, (xlim+0.5*wt)), "native"),
                   y = grid::unit(y1/ylim, "native"),
                   gp=grid::gpar(col="black"))
  grid::popViewport()
  grid::popViewport()
}

normalize = function(x){return(x/sum(x))}

