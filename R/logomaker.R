#' @title Main workhorse function that builds the logo plots
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
#' @param cols A vector of colors for the different logos or symbols stacked in the
#' logo plot. The length of this vector should match with number of symbols or
#' logos used in the plot, which is again ame as the number of rows in the input
#' table.
#'
#' @param frame_width The width of the frames for individual site/postion/column
#' in the logo plot. As default, all the columns have same width, equal to 1.
#'
#' @param ic.scale if TRUE, the height of the bars in the stacked logo chart for
#' each column is determined based on the information criterion  input.
#' Otherwise, the bars are normalized so that the height of each bar is $1$.
#' Defaults to TRUE.
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
#' @param addlogos Vector of additional logos/symbols defined by user
#' @param addlogos_text Vector of the names given to the additional logos/symbols defined by user.
#'
#' @return Plots the logo plot for the table data, with column names representing
#' the sites/blocks and the row names denoting the symbols for which logos are
#' plotted
#'
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' counts_mat <- rbind(c(0, 10, 100, 60, 20),
#'                     c(40, 30, 30, 35, 20),
#'                     c(100, 0, 15, 25, 75),
#'                     c(10, 30, 20, 50, 70))
#'
#' colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
#' rownames(counts_mat) <- c("M", "U", "T", "D")
#' logomaker(counts_mat,
#'           cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
#'           frame_width = 1,
#'           ic.scale = FALSE)
#'
#' logomaker(counts_mat,
#'          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
#'          frame_width = 1)
#'
#' @export
#'
#'

logomaker <- function( table,
                       ic=NULL,
                       cols,
                       frame_width=NULL,
                       ic.scale=TRUE,
                       alpha=1,
                       xaxis=TRUE,
                       yaxis=TRUE,
                       xaxis_fontsize=10,
                       xlab_fontsize=15,
                       y_fontsize=15,
                       start=0.0001,
                       yscale_change=TRUE,
                       pop_name = NULL,
                       xlab = "X",
                       ylab = "Information content",
                       col_line_split="grey80",
                       addlogos = NULL,
                       addlogos_text = NULL){

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
    ic <- ic_computer(table_mat_norm, alpha)
  }

  letters <- list(x=NULL,y=NULL,id=NULL,fill=NULL)
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
  }else{
    ylim <- 1
    ylab <- "Probability"
    facs <- rep(1, npos)
  }

  x.pos <- 0
  slash_inds <- grep("/", chars)

  for (j in 1:npos){

    column <- table_mat_norm[,j]
    hts <- as.numeric(0.99*column*facs[j])
    letterOrder <- order(hts)

    y.pos <- 0
    for (i in 1:length(chars)){
      letter <- chars[letterOrder[i]]
      col <- cols[letterOrder[i]]
      ht <- hts[letterOrder[i]]
      if(length(intersect(letterOrder[i], slash_inds))!=0){
        if (ht>0) letters <- addLetter(letters,letter, col, x.pos, y.pos, ht, wt[j], addlogos = addlogos, addlogos_text = addlogos_text)
      }else{
        if (ht>0) letters <- addLetter(letters,letter, col, x.pos, y.pos, ht, wt[j], addlogos = NULL, addlogos_text = NULL)
      }
      y.pos <- y.pos + ht + start
    }
    x.pos <- x.pos + wt[j]

  }

  xlim <- cumsum(wt) - wt/2;
  # xlim <- c(wt[1]/2, wt[1] + wt[2]/2, wt[1]+wt[2]+wt[3]/2, wt[1]+wt[2]+wt[3], 5.5)
  low_xlim <- c(xlim - 0.5*wt, xlim[length(xlim)]+0.5*wt[length(xlim)])
  ylim_scale <- seq(0, ylim, length.out=6);
  if(ic.scale){
      ic_lim_scale <- seq(0, max(ic), length.out=6)
  }else{
      ic_lim_scale <- ylim_scale
  }
  if(ic.scale){
  if(yscale_change){
       if(ylim  > 1){
         letters$y <- letters$y*(ylim/max(ic));
       }
  }}


  grid::grid.newpage()
  bottomMargin = ifelse(xaxis, 2 + xaxis_fontsize/3.5, 3)
  leftMargin = ifelse(yaxis, 0.1 + y_fontsize/3.5, 3)
  grid::pushViewport(grid::plotViewport(c(bottomMargin,leftMargin,max(xlim)+0.5,max(xlim)*wt+0.5)))
  # pushViewport(viewport(layout = grid.layout(2, 2),
  #              x = bottomMargin,
  #              y = leftMargin,
  #              width = max(xlim/2)+0.5,
  #              height = max(ylim/2)+0.5))
  grid::pushViewport(grid::dataViewport(0:ncol(table_mat_norm),0:ylim,name="vp1"))
  grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
               id=letters$id, gp=grid::gpar(fill=letters$fill,col="transparent"))
  grid::grid.polygon(x=grid::unit(letters$x,"native"), y=grid::unit(letters$y,"native"),
               id=letters$id,
               gp=grid::gpar(fill=letters$fill,col="transparent"))


  for(n in 1:length(xlim)){
    grid::grid.lines(x = grid::unit(low_xlim[n], "native"),
               y = grid::unit(c(0, ylim), "native"),
               gp=grid::gpar(col=col_line_split))
  }

  if(is.null(pop_name)){
    grid::grid.text("Logo plot", y = grid::unit(1, "npc") + grid::unit(1, "lines"),
              gp = grid::gpar(fontsize = 16))
  }else{
    grid::grid.text(paste0("Logo plot of ", pop_name),
                    y = grid::unit(1, "npc") + grid::unit(1, "lines"),
              gp = grid::gpar(fontsize = 16))
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
      grid::grid.yaxis(at = ylim_scale,
                 label = round(ic_lim_scale,1),
                 gp=grid::gpar(fontsize=y_fontsize))
    }else{
      grid::grid.yaxis(gp=grid::gpar(fontsize=y_fontsize))
    }
    grid::grid.text(ylab,x=grid::unit(-3,"lines"),rot=90,
              gp=grid::gpar(fontsize=y_fontsize))
  }
  grid::popViewport()
  grid::popViewport()
  par(ask=FALSE)
}

addLetter <- function(letters, letter,
                      col, x.pos, y.pos,ht,wt,
                      addlogos, addlogos_text){
  letter <- as.character(toupper(letter))
  out <- makemylogo(letter,
                    colfill = col,
                    addlogos=addlogos,
                    addlogos_text = addlogos_text)
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
  return(letters)
}
