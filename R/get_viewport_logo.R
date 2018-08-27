#' @title Function for creating a multi-panel logo viewport
#'
#' @description This is a void function to be run prior to a multi-panel Logolas
#' plot for a fixed number of rows and columns of the panel.
#'
#' @param  layout.rows The number of rows in the panel
#' @param  layout.cols The number of columns in the panel
#' @param  widths.val  The widths of each viewport in the panel
#' @param  heights.val The heights of each viewport in the panel
#' @return Creates a panel of viewports for each row and each column, to be
#' subsequently used for creating logo plots.
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' get_viewport_logo(layout.rows = 2, layout.cols = 2)
#' @export


get_viewport_logo <- function(layout.rows, layout.cols,
                              widths.val = 6, heights.val = 20){
  grid::grid.newpage()
    
  if(length(widths.val) == 1){
      widths_vec <- rep(widths.val,layout.cols)
  }else{
    if(length(widths.val) != layout.cols) stop("widths must be a number of a vector of numbers of same size as layout.cols")
    widths_vec <- widths.val
  }
    
  if(length(heights.val) == 1){
      heights_vec <- rep(heights.val, layout.rows)
  }else{
    if(length(heights.val) != layout.cols) stop("heights must be a number of a vector of numbers of same size as layout.rows")
    heights_vec <- heights.val
  }
  
  top.vp <- viewport(layout=grid.layout(layout.rows, layout.cols,
                                        widths=unit(widths_vec, 
                                                    rep("null", layout.cols)),
                                        heights=unit(heights_vec,
                                                rep("null", layout.rows))))

  plot_reg <- vpList()
  l <- 1
  for(i in 1:layout.rows){
    for(j in 1:layout.cols){
      plot_reg[[l]] <- viewport(layout.pos.col = j, layout.pos.row = i, 
                                name = paste0("plotlogo", l))
      l <- l+1
    }
  }


  plot_tree <- vpTree(top.vp, plot_reg)

  pushViewport(plot_tree)
}

