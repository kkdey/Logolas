#'@title Main workhorse function that builds negative logo plots
#'
#' @description stacks logos created by the \code{makemylogo} function
#'  on top of each other to build the logo plot.
#'
#' @param table The input table (data frame or matrix) of counts
#'  across different logos or symbols (specified along the rows) 
#'  and across different sites or positions or groups (specified 
#'  along the columns).
#'
#' @param ic Boolean, denoting whether information content based 
#' scaling is used on top of the scoring scheme used or not. 
#' Default is FALSE
#'
#' @param score Can take either of the options - \code{diff},
#' \code{log}, \code{log-odds}, \code{probKL}, \code{ratio}, 
#' \code{unscaled_log}, \code{wKL}. Each option corresponds to 
#'  a different scoring scheme. The most recommended option is 
#'  \code{log}.
#'
#' @param color_profile A list containing two elements - "type" and "col".
#'  The type can be of three types - "per-row", "per-column" and 
#'  "per-symbol". The "col" element is a vector of colors, of same length
#'  as number of rows in table for "per-row" (assigning a color to each 
#'  string), of same length as number of columns in table for "per-column"
#' (assuming a color for each column), or a distinct color for a distinct
#'  symbol in "per-symbol". For "per-symbol", the length of the 
#'  \code{color_profile$col} should be same as library size of the logos, 
#'  but if the vector of colors provided is more or less, we can 
#'  downsample or upsample the colors as required. The colors are 
#'  matched with the symbols in the \code{total_chars}
#'
#' @param total_chars The total number of character symbols in the user
#'  library. The default is the default library provided by Logolas,
#'  but the user can add symbols that he creates to this list.
#'
#' @param bg The background probability, which defaults to NULL, 
#' in which case equal probability is assigned to each symbol. The user 
#' can however specify a vector (equal to in length to the number of 
#' symbols) which specifies the background probability for each symbol 
#' and assumes this background probability to be the same across the 
#' columns (sites), or a matrix, whose each cell specifies
#' the background probability of the symbols for each position.
#'
#' @param frame_width The width of the frames for individual 
#' site/postion/column in the logo plot. As default, all the 
#' columns have same width, equal to 1.
#'
#' @param xaxis Binary specifying if there should be a X axis in 
#' the logo plot or not. Defaults to TRUE.
#'
#' @param yaxis Binary specifying if there should be a Y axis in 
#' the logo plot or not. Defaults to TRUE.
#'
#' @param xaxis_fontsize The size of the X-axis axis ticks.
#'
#' @param xlab_fontsize The size of the X-axis label.
#'
#' @param y_fontsize The size of the Y-axis font.
#'
#' @param main_fontsize The size of the title.
#'
#' @param yscale_change If TRUE, adjusts the Y axis scale based on
#'  the size of the bars, else keeps it to the maximum value possible,
#'  which is \code{ceiling(max(ic)} under \code{ic_computer} 
#'  defined IC criteria.
#'
#' @param start The starting point in Y axis for the first logo. 
#' Default is 0.0001 which is very close to 0.
#'
#' @param pop_name User can mention a name of the population for which
#'  the logo plot is created. Defaults to NULL when no population name 
#'  is mentioned.
#'
#' @param xlab X axis label
#' @param ylab Y axis label
#'
#' @param col_line_split The color of the line split between the 
#' consecutive groups or blocks
#'
#' @param yrange The limit of the Y axis.
#'
#' @param addlogos Vector of additional logos/symbols defined by user
#' @param addlogos_text Vector of the names given to the additional 
#' logos/symbols defined by user.
#'
#' @param newpage if TRUE, plots the logo plot in a new page. Defaults 
#' to TRUE.
#'
#' @param control control parameters fixing whether the height of the 
#' logos is detrmined by IC or histogram proportions (\code{hist}),
#' the scales for the plot (\code{scale0}, \code{scale1}), the 
#' additive factor epsilon added to log transform to avoid log(0) 
#' errors (\code{epsilon}), the quantile adjustment (\code{quant}), 
#' whether to use symmetric KL for scaling (\code{symm}),
#' whether the symbols should be filled with color or border colored
#' (\code{tofill_pos, tofill_neg}), the Renyi alpha parameter for the 
#' entropy calculation (\code{alpha}), the gap between ylabel and 
#' y-axis and xlabel and x-axis texts (\code{gap_ylab}, \code{gap_xlab}),
#' the viewport configuration details for the plot 
#' (\code{viewport.margin.bottom},\code{viewport.margin.left},
#'  \code{viewport.margin.top}, \code{viewport.margin.right}),
#'  whether the height of the logos would be fixed
#' apriori or determined by the PWM matrix as in seqLogo
#' (\code{use_seqLogo_heights}) etc.
#'
#' @return Plots the logo plot for the table data, with column names 
#' representing the sites/blocks and the row names denoting the symbols
#' for which logos are plotted
#'
#' @import grid
#' @importFrom graphics par
#' @importFrom  utils  modifyList
#'
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
#' col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, 
#'                    rownames(cols)))
#'
#' total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", 
#'                 "K", "L", "M", "N", "O",
#'                "P", "Q", "R", "S", "T", "U", "V", 
#'                "W", "X", "Y", "Z", "zero", "one", "two",
#'                "three", "four", "five", "six", "seven", 
#'                "eight", "nine", "dot", "comma",
#'                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
#'
#' set.seed(20)
#' col_vector[c(1,3,7, 20, 43)] <- c("red", "blue", 
#'                               "orange", "green", "gray")
#' color_profile <- list("type" = "per_symbol",
#'                      "col" = col_vector)
#'
#' nlogomaker(table,
#'            color_profile = color_profile,
#'            yrange = 1.2)
#'
#' @importFrom stats median
#' @import ggplot2
#' @import gridBase
#' @export

nlogomaker <- function(table,
                       ic = FALSE,
                       score = c("diff", "log", "log-odds", "probKL", 
                                 "ratio", "unscaled_log", "wKL"),
                       color_profile,
                       total_chars = c("A", "B", "C", "D", "E", "F",
                                       "G", "H", "I", "J", "K", "L", "M",
                                       "N", "O", "P", "Q", "R", "S", "T",
                                       "U", "V", "W", "X", "Y", "Z", "zero",
                                       "one", "two","three", "four",
                                       "five", "six", "seven", "eight",
                                       "nine", "dot", "comma",
                                       "dash", "colon", "semicolon",
                                       "leftarrow", "rightarrow"),
                       bg = NULL,
                       frame_width=NULL,
                       yscale_change=TRUE,
                       pop_name = NULL,
                       addlogos = NULL,
                       addlogos_text = NULL,
                       newpage = TRUE,
                       yrange = NULL,
                       xaxis=TRUE,
                       yaxis=TRUE,
                       xaxis_fontsize=10,
                       xlab_fontsize=15,
                       y_fontsize=15,
                       main_fontsize=16,
                       start=0.001,
                       xlab = "X",
                       ylab = "Enrichment Score",
                       col_line_split="grey80",
                       control = list()){

  control.default <- list(hist = FALSE, alpha = 1, opt = 1, scale0=0.01,
                          scale1=0.99, tofill_pos = TRUE, tofill_neg = TRUE,
                          lwd = 2, epsilon = 0.01,
                          quant = 0.5, symm = TRUE,
                          gap_xlab = 3, gap_ylab = 3.5,
                          minbins = 2, round_off = 1,
                          lowrange = 0, uprange = 0,
                          negbins = 3, posbins = 3,
                          size_port = 0,
                          npc_units_main = 1,
                          lines_units_main = 0.8,
                          viewport.margin.bottom = NULL,
                          viewport.margin.left = NULL,
                          viewport.margin.top = NULL,
                          viewport.margin.right = NULL,
                          use_seqLogo_heights = FALSE)

  # viewport margins usually c(3, 5, 3, 3)

  control <- modifyList(control.default, control)
  scale0 <- control$scale0
  scale1 <- control$scale1
  
  if(length(score) != 1){
    score <- "log"
  }

  if(ic & score == "unscaled_log"){
    warning("ic = TRUE not compatible with score = `unscaled-log`: 
            switching to ic = FALSE")
    ic = FALSE
  }
  if(ic & score == "wKL"){
    warning("ic = TRUE not compatible with score = `wKL`:
            switching to ic = FALSE")
    ic = FALSE
  }
  
  if (class(table) == "data.frame"){
    table <- as.matrix(table)
  }else if (class(table) != "matrix"){
    stop("the table must be of class matrix or data.frame")
  }
  chars <- as.character(rownames(table))
  npos <- ncol(table)

  table <- apply(table+0.0001,2,normalize3)

  control_heights <- list(alpha = control$alpha, epsilon = control$epsilon,
                          opt = control$opt, hist = control$hist,
                          quant = control$quant, symm = control$symm)
  ll <- do.call(get_logo_heights, append(list(table = table,
                                              ic = ic,
                                              bg = bg,
                                              score = score),
                                       control_heights))


  # if(ic & score == "ratio"){
  #   ll <- get_logo_heights_ic_ratio(table, alpha = control$alpha,
  #                             epsilon = control$epsilon,
  #                             bg = bg,
  #                             opt = control$opt,
  #                             hist = control$hist,
  #                             quant = control$quant,
  #                             symm = control$symm)
  # } else if (ic & score == "diff"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_ic_diff(table, alpha = control$alpha,
  #                                   epsilon = control$epsilon,
  #                                   bg = bg,
  #                                   opt = control$opt,
  #                                   hist = control$hist,
  #                                   quant = control$quant,
  #                                   symm = control$symm)
  # }else if (logoheight == "ic_log"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_ic_log(table, alpha = control$alpha,
  #                                  epsilon = control$epsilon,
  #                                  bg = bg,
  #                                  opt = control$opt,
  #                                  hist = control$hist,
  #                                  quant = control$quant,
  #                                  symm = control$symm)
  # }else if (logoheight == "ic_log_odds"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_ic_log_odds(table, alpha = control$alpha,
  #                                 epsilon = control$epsilon,
  #                                 bg = bg,
  #                                 opt = control$opt,
  #                                 hist = control$hist,
  #                                 quant = control$quant,
  #                                 symm = control$symm)
  # }else if (logoheight == "log"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_log(table, epsilon = control$epsilon,
  #                              bg = bg,
  #                              alpha = control$alpha, hist = control$hist,
  #                              quant = control$quant)
  # } else if (logoheight == "log_odds"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_log_odds(table, epsilon = control$epsilon,
  #                                   bg = bg,
  #                                   alpha = control$alpha, hist = control$hist,
  #                                   quant = control$quant)
  # }else if (logoheight == "diff"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_diff(table, epsilon = control$epsilon,
  #                               bg = bg,
  #                               alpha = control$alpha, hist = control$hist,
  #                               quant = control$quant)
  # }else if (logoheight == "ratio"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_ratio(table, epsilon = control$epsilon,
  #                               bg = bg,
  #                               alpha = control$alpha, hist = control$hist,
  #                               quant = control$quant)
  # }else if (logoheight == "probKL"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_probKL(table, epsilon = control$epsilon,
  #                                bg = bg,
  #                                alpha = control$alpha, hist = control$hist,
  #                                quant = control$quant)
  # }else if (logoheight == "ic_probKL"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_ic_probKL(table, alpha = control$alpha,
  #                                 epsilon = control$epsilon,
  #                                 bg = bg,
  #                                 opt = control$opt,
  #                                 hist = control$hist,
  #                                 quant = control$quant,
  #                                 symm = control$symm)
  # }else if (logoheight == "wKL"){
  #   table <- apply(table+0.0001,2,normalize3)
  #   ll <- get_logo_heights_wKL(table, epsilon = control$epsilon,
  #                                 bg = bg,
  #                                 alpha = control$alpha, hist = control$hist,
  #                                 quant = control$quant)
  # }else if (logoheight == "unscaled_log"){
  #   ll <- get_logo_heights_unscaled_log(table, epsilon = control$epsilon,
  #                              bg = bg,
  #                              alpha = control$alpha, hist = control$hist,
  #                              quant = control$quant)
  # }else{
  #   stop("logoheight option must be one of log, ratio, diff, probKL,
  #         wKL, log_odds, ic_log, ic_diff, ic_ratio, ic_log_odds,
  #         ic_probKL, unscaled_log")
  # }

  pos_ic <- ll$pos_ic
  neg_ic <- ll$neg_ic
  table_mat_pos_norm <- ll$table_mat_pos_norm
  table_mat_neg_norm <- ll$table_mat_neg_norm

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

 # print(pos_ic)
 # print(neg_ic)
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
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_pos,
                                           lwd = control$lwd,
                                           col, total_chars, x.pos,
                                           y.pos, ht, wt[j], 
                                           scale0 = scale0, 
                                           scale1=scale1, 
                                           addlogos = addlogos,
                                           addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_pos, 
                                           lwd = control$lwd, 
                                           col, total_chars, 
                                           x.pos, y.pos, 
                                           ht, wt[j], 
                                           scale0 = scale0, 
                                           scale1=scale1, 
                                           addlogos = NULL, 
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
          if (ht>0) letters <- addLetter_n(letters,letter, tofill = control$tofill_pos, lwd = control$lwd, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = addlogos, addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_n(letters,letter, tofill = control$tofill_pos, lwd = control$lwd, color_profile$col, total_chars, x.pos, y.pos, ht, wt[j], scale0 = scale0, scale1=scale1, addlogos = NULL, addlogos_text = NULL)
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
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_pos,
                                           lwd = control$lwd, 
                                           color_profile$col[j],
                                           total_chars, 
                                           x.pos, y.pos, ht, wt[j], 
                                           scale0 = scale0, scale1=scale1,
                                           addlogos = addlogos, 
                                           addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_pos,
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


  letters$y <- letters$y + max(max(abs(neg_ic)), control$lowrange)
 # letters$y <- 0.8*letters$y*(ylim/max(max(pos_ic), max(neg_ic)))

  y1 <- min(letters$y)
  max1 <- max(letters$y)
  # if(is.null(yrange)){
  #   if( (max(pos_ic) + max(neg_ic)) < 1){
  #     yrange <- max(pos_ic) + max(neg_ic) + 0.01
  #   }else{
  #     yrange <- ceiling(max(max(pos_ic), control$uprange) + max(max(neg_ic), control$lowrange))
  #   }
  # }else{
  #   if(yrange > ceiling(max(pos_ic) + max(neg_ic))){
  #     ylim <- yrange
  #   }else{
  #     warning("yrange chosen does not contain the whole range of variation of the logo heights,
  #             keep yrange as NULL for full visualization")
  #     ylim <- yrange
  #   }
  # }
 # yrange <- ceiling(max(max(pos_ic), control$uprange) + max(max(neg_ic), control$lowrange))
  yrange <- max(max(pos_ic), control$uprange) + max(max(neg_ic), 
                                                    control$lowrange)
  ylim <- yrange
  # print(pos_ic)
  # print(neg_ic)
  # print(yrange)
  ylim_scale <- seq(0, ylim, length.out=6);


    negbins <- control$negbins
    posbins <- control$posbins


   ic_lim_scale <- c(seq(0, y1, length.out = negbins),
                    seq(y1, ylim, length.out = posbins))
 # print(ic_lim_scale)

  letters$y <- letters$y/ylim

  markers <- round(ic_lim_scale,control$round_off) - round(y1,control$round_off)

  if(newpage){
    grid::grid.newpage()
  }
  #  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)

  if(control$use_seqLogo_heights){
    if(is.null(control$viewport.margin.bottom)){
      bottomMargin <- ifelse(xaxis, 1 + xaxis_fontsize/3.5, 3)}
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

  start1 <- 0.5
  wt <- wt/min(wt)
  wtdiff <- abs(wt[2:length(wt)] - wt[1:(length(wt)-1)])
  xticks <- array(0, dim(table)[2])
  for(w in 1:length(wt)){
    xticks[w] <- start1
    start1 <- xticks[w] + 1 + (wtdiff[w])/2
  }


  grid::pushViewport(grid::plotViewport(c(bottomMargin, leftMargin,
                                          topMargin, rightMargin)))

  # pushViewport(viewport(layout = grid.layout(2, 2),
  #              x = bottomMargin,
  #              y = leftMargin,
  #              width = max(xlim/2)+0.5,
  #              height = max(ylim/2)+0.5))
  grid::pushViewport(grid::dataViewport(0:(ncol(table)+control$size_port),
                                        0:1,name="vp1"))
  if(control$tofill_pos){
    # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
    #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(fill=letters$fill, col="transparent"))
  }else{
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(col=letters$colfill, 
                                     lwd = control$lwd))
  }


  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id,
  #                    gp=grid::gpar(fill=letters$fill,col="transparent"))


  for(n in 2:length(xlim)){
    grid::grid.lines(x = grid::unit(low_xlim[n], "native"),
                     y = grid::unit(c(0, 1), "native"),
                     gp=grid::gpar(col=col_line_split))
  }

  if(is.null(pop_name)){
    if(ic){
      grid::grid.text(paste0("EDLogo plot: (",score,"-ic",  ")"), 
                      y = grid::unit(control$npc_units_main, "npc") + grid::unit(control$lines_units_main, "lines"),
                      gp = grid::gpar(fontsize = main_fontsize))
    }else{
      grid::grid.text(paste0("EDLogo plot: (",score, ")"), 
                      y = grid::unit(control$npc_units_main, "npc") + grid::unit(control$lines_units_main, "lines"),
                      gp = grid::gpar(fontsize = main_fontsize))
    }
  }else{
    grid::grid.text(paste0(pop_name),
                    y = grid::unit(control$npc_units_main, "npc") + grid::unit(control$lines_units_main, "lines"),
                    gp = grid::gpar(fontsize = main_fontsize))
  }


  if (xaxis){
    grid::grid.xaxis(at=xticks, draw = TRUE,
                     label=colnames(table),
                     gp=grid::gpar(fontsize=xaxis_fontsize))
    grid::grid.text(xlab, y=grid::unit(-control$gap_xlab,"lines"),
                    gp=grid::gpar(fontsize=xaxis_fontsize))
  }
  if (yaxis){
   # if(yscale_change==TRUE){
      grid::grid.yaxis(at = ic_lim_scale/ylim, draw = TRUE,
                       label = round(ic_lim_scale,control$round_off) - round(y1,control$round_off),
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
  ylim <- yrange

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
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_neg,
                                           lwd = control$lwd, 
                                           col, total_chars, 
                                           x.pos, y.pos, ht, wt[j], 
                                           scale0 = scale0, scale1=scale1, 
                                           addlogos = addlogos, 
                                           addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_neg, 
                                           lwd = control$lwd, col, 
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
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_neg,
                                           lwd = control$lwd, 
                                           color_profile$col, 
                                           total_chars, x.pos, y.pos,
                                           ht, wt[j], scale0 = scale0, 
                                           scale1=scale1, 
                                           addlogos = addlogos, 
                                           addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_neg,
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
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_neg,
                                           lwd = control$lwd, 
                                           color_profile$col[j], 
                                           total_chars, x.pos, y.pos,
                                           ht, wt[j], scale0 = scale0, 
                                           scale1=scale1, addlogos = addlogos,
                                           addlogos_text = addlogos_text)
        }else{
          if (ht>0) letters <- addLetter_n(letters,letter, 
                                           tofill = control$tofill_neg,
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

  letters$y <- letters$y + max(max(abs(neg_ic)), control$lowrange)
#  letters$y <- 0.8*letters$y*(ylim/max(max(pos_ic), max(neg_ic)))
  letters$y <- letters$y/ylim

  xlim <- cumsum(wt) - wt/2;
  # xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
  low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
  ylim_scale <- seq(0, ylim, length.out=6);
  ic_lim_scale <- seq(-max(max(neg_ic), control$lowrange), 0, length.out=6)

  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
  # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
  #                    id=letters$id,
  #                    gp=grid::gpar(fill=letters$fill,col="transparent"))

  if(control$tofill_neg){
    # grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
    #                    id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(fill=letters$fill, 
                                     col="transparent"))
  }else{
    grid::grid.polygon(x=grid::unit(letters$x,"native"), 
                       y=grid::unit(letters$y,"native"),
                       id=letters$id,
                       gp=grid::gpar(col=letters$colfill, 
                                     lwd = control$lwd))
  }


  grid::grid.lines(x = grid::unit(c(0, (xlim+0.5*wt)), "native"),
                   y = grid::unit(y1/ylim, "native"),
                   gp=grid::gpar(col="black"))
  grid::popViewport()
  grid::popViewport()
  return(ll)
}


addLetter_n <- function(letters, letter, tofill, lwd,
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

normalize3 = function(x){return(x/sum(x[!is.na(x)]))}

