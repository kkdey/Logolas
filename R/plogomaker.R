#' @title Main workhorse function that builds the positive logo plots
#'
#' @description stacks logos created by the \code{makemylogo} function on top of
#' each other to build the logo plot.
#'
#' @param table The input table (data frame or matrix) of counts across different
#' logos or symbols (specified along the rows) ans across different sites or
#' positions or groups (specified along the columns).
#'
#' @param ic A vector of same length as the number of columns in the \code{table},
#' repesenting the heights of the logo stacked bars for each position/site/block.
#' It defaults to NULL, in which case, the function computes the ic vector using
#' the \code{ic_computer} functionality.
#'
#' @param color_profile A list containing two elements - "type" and "col". 
#' The type can be of three types - "per-row", "per-column" and "per-symbol".
#' The "col" element is a vector of colors, of same length as number of rows
#' in table for "per-row" (assigning a color to each string), of same length 
#' as number of columns in table for "per-column" (assuming a color for each 
#' column), or a distinct color for a distinct symbol in "per-symbol". For 
#' "per-symbol", the length of the \code{color_profile$col} should be same as 
#' library size of the logos, but if the vector of colors provided is more or 
#' less, we can downsample or upsample the colors as required. The colors 
#' are matched with the symbols in the \code{total_chars}
#'
#' @param total_chars The total number of character symbols in the user library.
#'  The default is the default library provided by Logolas, but the user can 
#'  add symbols that he creates to this list.
#'
#' @param bg The background probability, which defaults to NULL, in which case
#' equal probability is assigned to each symbol. The user can however specify a
#' vector (equal to in length to the number of symbols) which specifies the
#' background probability for each symbol and assumes this background probability
#' to be the same across the columns (sites), or a matrix, whose each cell 
#' specifies the background probability of the symbols for each position.
#'
#' @param pseudocount A small pseudocount to be added mainly to bypass 0 entries. 
#'                     Default is NULL. If \code{table} is a counts matrix, 
#'                     the default changes to 0.5, if \code{table} is a 
#'                     positional weight matrix, the default becomes 0.001 times
#'                     the minimum non-zero value of the table.
#'
#' @param frame_width The width of the frames for individual site/postion/column
#' in the logo plot. As default, all the columns have same width, equal to 1.
#'
#' @param ic.scale if TRUE, the height of the bars in the stacked logo chart for
#' each column is determined based on the information criterion  input.
#' Otherwise, the bars are normalized so that the height of each bar is $1$.
#' Defaults to TRUE.
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
#'                     groups or blocks
#'
#' @param addlogos Vector of additional logos/symbols defined by user
#' @param addlogos_text Vector of the names given to the additional 
#'               logos/symbols defined by user.
#'
#'
#' @param newpage if TRUE, plots the logo plot in a new page. Defaults to TRUE.
#'
#' @param control control parameters fixing whether the height of the logos is
#' detrmined by IC or histogram proportions (\code{hist}), the scales for the
#' plot (\code{scale0}, \code{scale1}), whether the symbols should be filled
#' with color or border colored (\code{tofill}), the Renyi alpha parameter for
#' the entropy calculation (\code{alpha}), the gap between ylabel and y-axis and
#' xlabel and x-axis texts (\code{gap_ylab}, \code{gap_xlab}), and the viewport
#' configuration details for the plot (\code{viewport.margin.bottom},
#' \code{viewport.margin.left}, \code{viewport.margin.top},
#' \code{viewport.margin.right}), whether the height of the logos would be fixed
#' apriori or determined by the PWM matrix as in seqLogo
#' (\code{use_seqLogo_heights}) etc.
#'
#' @return Plots the logo plot for the table data, with column names representing
#' the sites/blocks and the row names denoting the symbols for which logos are
#' plotted
#'
#' @keywords internal
#'
#' @examples
#'
#' cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
#' col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, 
#'                          rownames(cols)))
#' counts_mat <- rbind(c(0, 10, 100, 60, 20),
#'                    c(40, 30, 30, 35, 20),
#'                    c(100, 0, 15, 25, 75),
#'                    c(10, 30, 20, 50, 70))
#' colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
#' rownames(counts_mat) <- c("MAN", "MAIL", "LAWN", "CAR")
#'
#' color_profile <- list("type" = "per_symbol",
#'                      "col" = col_vector)
#' plogomaker(counts_mat,
#'           color_profile = color_profile,
#'           frame_width = 1,
#'           ic.scale = FALSE)
#'
#' color_profile <- list("type" = "per_row",
#'                      "col" = col_vector[1:4])
#'
#' plogomaker(counts_mat,
#'          color_profile = color_profile,
#'          frame_width = 1,
#'          ic.scale = FALSE)
#'
#' color_profile <- list("type" = "per_column",
#'                       "col" = col_vector[1:5])
#'
#' plogomaker(counts_mat,
#'          color_profile = color_profile,
#'          frame_width = 1,
#'          ic.scale = FALSE)
#'
#' @import grid
#' @importFrom graphics par
#' @importFrom utils modifyList
#' @export
#' 
plogomaker <- function( table,
                       ic=NULL,
                       color_profile,
                       total_chars = c("A", "B", "C", "D", "E", "F", "G", "H",
                                       "I", "J", "K", "L", "M", "N", "O",
                                       "P", "Q", "R", "S", "T", "U", "V", "W",
                                       "X", "Y", "Z", "zero", "one", "two",
                                       "three", "four", "five", "six", "seven",
                                       "eight", "nine", "dot", "comma",
                                       "dash", "colon", "semicolon",
                                       "leftarrow", "rightarrow"),
                       bg = NULL,
                       pseudocount = NULL,
                       frame_width=NULL,
                       ic.scale=TRUE,
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
                       ylab = "Information content",
                       col_line_split="grey80",
                       addlogos = NULL,
                       addlogos_text = NULL,
                       newpage = TRUE,
                       control = list()){
  
  if(length(which(table == 0)) > 0){
    table <- zero_augment(table, pseudocount)
  }
  table <- apply(table,2,normalize2)

  control.default <- list(hist = FALSE, alpha = 1, scale0=0.01,
                          scale1=0.99, tofill = TRUE, lwd = 2,
                          gap_ylab = 3, gap_xlab = 3,
                          totbins = 5, round_off = 1,
                          viewport.margin.bottom = NULL,
                          viewport.margin.left = NULL,
                          viewport.margin.top = NULL,
                          viewport.margin.right = NULL,
                          use_seqLogo_heights = FALSE)

  # viewport margins usually c(3, 5, 3, 3)

  control <- modifyList(control.default, control)
  scale0 <- control$scale0
  scale1 <- control$scale1
  hist <- control$hist
  alpha <- control$alpha


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

  if (class(table) == "data.frame"){
    table <- as.matrix(table)
  }else if (class(table) != "matrix"){
    stop("the table must be of class matrix or data.frame")
  }

  #print(table)
  table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x[!is.na(x)])))
  #table_mat_norm <- replace(table_mat_norm, is.na(table_mat_norm), 0)

  npos <- ncol(table_mat_norm)
  chars <- as.character(rownames(table_mat_norm))
  #print(table_mat_norm)

  if(is.null(ic)){
    if(hist==FALSE){
      ic <- ic_computer(table_mat_norm, alpha, hist=hist, bg = bg)
    }else{
      ic <- ic_computer(table, alpha, hist=hist, bg = bg)
    }
  }

  table_mat_norm <- replace(table_mat_norm, is.na(table_mat_norm), 0)

  letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL, colfill = NULL)
  npos <- ncol(table_mat_norm)


  if (ic.scale){
    if(yscale_change){
      if(max(ic)<1){ylim <- 1
      facs <- ic + 1 - max(ic)}
      if(max(ic)>1){ylim <- ceiling(max(ic))
      facs <- ic}
    }else{
      ylim <- ceiling(max(ic))
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

  if(color_profile$type == "per_row"){
    for (j in seq_len(npos)){

      column <- table_mat_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- order(hts)

      y.pos <- 0
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        col <- color_profile$col[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, 
                                         tofill = control$tofill, 
                                         lwd = control$lwd, col, total_chars,
                                         x.pos, y.pos, ht, wt[j], 
                                         scale0 = scale0, scale1=scale1,
                                         addlogos = addlogos, 
                                         addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, 
                                         tofill = control$tofill, 
                                         lwd = control$lwd, col, total_chars,
                                         x.pos, y.pos, ht, wt[j], 
                                         scale0 = scale0, scale1=scale1, 
                                         addlogos = NULL, addlogos_text = NULL)
        }
        y.pos <- y.pos + ht + start
      }
      x.pos <- x.pos + wt[j]
    }
  }

  if(color_profile$type == "per_symbol"){
    for (j in seq_len(npos)){

      column <- table_mat_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- order(hts)

      y.pos <- 0
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, 
                                         tofill = control$tofill, 
                                         lwd = control$lwd, color_profile$col,
                                         total_chars, x.pos, y.pos, ht, wt[j],
                                         scale0 = scale0, scale1=scale1, 
                                         addlogos = addlogos, 
                                         addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter, 
                                         tofill = control$tofill,
                                         lwd = control$lwd, 
                                         color_profile$col, 
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

  if(color_profile$type == "per_column"){
    for (j in seq_len(npos)){

      column <- table_mat_norm[,j]
      hts <- as.numeric(0.99*column*facs[j])
      letterOrder <- order(hts)
      y.pos <- 0
      for (i in seq_along(chars)){
        letter <- chars[letterOrder[i]]
        ht <- hts[letterOrder[i]]
        if(length(intersect(letterOrder[i], slash_inds))!=0){
          if (ht>0) letters <- addLetter(letters,letter, 
                                         tofill = control$tofill,
                                         lwd = control$lwd, 
                                         color_profile$col[j],
                                         total_chars, x.pos, y.pos,
                                         ht, wt[j], scale0 = scale0, 
                                         scale1=scale1, addlogos = addlogos, 
                                         addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter(letters,letter,
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
  ylim_scale <- seq(0, ylim, length.out=control$totbins);
  if(ic.scale){
      ic_lim_scale <- seq(0, max(ic), length.out=control$totbins)
  }else{
      ic_lim_scale <- ylim_scale
  }
  if(ic.scale){
  if(yscale_change){
      # if(ylim  > 1){
         letters$y <- letters$y*(ylim/max(ic));
     #  }
  }}

 if(newpage){
   grid::grid.newpage()
 }

  if(control$use_seqLogo_heights){
    if(is.null(control$viewport.margin.bottom))
    {bottomMargin <- ifelse(xaxis, 1 + xaxis_fontsize/3.5, 3)}
      else{bottomMargin <- control$viewport.margin.bottom}
    if(is.null(control$viewport.margin.left))
    {leftMargin <- ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)}
      else{leftMargin <- control$viewport.margin.left}
    if(is.null(control$viewport.margin.top)){topMargin <- max(ylim)+0.5}
      else{topMargin <- control$viewport.margin.top}
    if(is.null(control$viewport.margin.right)){rightMargin <- max(ylim)}
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
  grid::pushViewport(grid::dataViewport(0:ncol(table_mat_norm),
                                        0:ylim,name="vp1"))
  if(control$tofill){
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
               y = grid::unit(c(0, ylim), "native"),
               gp=grid::gpar(col=col_line_split))
  }

  if(is.null(pop_name)){
    grid::grid.text("Logolas plot:", y = grid::unit(1, "npc") + grid::unit(0.8, "lines"),
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
    grid::grid.text(xlab, y=grid::unit(-control$gap_xlab,"lines"),
                    gp=grid::gpar(fontsize=xaxis_fontsize))
  }
  if (yaxis){
    if(yscale_change==TRUE){
      s <- 1
      tempp <- round(ic_lim_scale,control$round_off)
      while (length(unique(tempp)) < length(tempp)){
        s <- s+1
        tempp <- round(ic_lim_scale,s)
      }
      grid::grid.yaxis(at = ylim_scale,
                 label = tempp,
                 gp=grid::gpar(fontsize=y_fontsize))
    }else{
      grid::grid.yaxis(gp=grid::gpar(fontsize=y_fontsize))
    }
    grid::grid.text(ylab,x=grid::unit(-control$gap_ylab,"lines"),rot=90,
              gp=grid::gpar(fontsize=y_fontsize))
  }
  grid::popViewport()
  grid::popViewport()
  par(ask=FALSE)
  return(ic)
}

addLetter <- function(letters, letter, tofill, lwd,
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

normalize2 = function(x){return(x/sum(x[!is.na(x)]))}
