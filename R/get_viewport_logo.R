#' @title Function for creating a multi-panel logo viewport
#'
#' @description This is a void function to be run prior to a multi-panel Logolas
#' plot for a fixed number of rows and columns of the panel.
#'
#' @param  layout.rows The number of rows in the panel
#' @param  layout.cols The number of columns in the panel
#' @param  widths_1  The widths of each viewport in the panel
#' @param  heights_1 The heights of each viewport in the panel
#' @return Creates a panel of viewports for each row and each column, to be
#' subsequently used for creating logo plots.
#' @import grid
#' @importFrom graphics par
#' @examples
#'
#' get_viewport_logo(layout.rows = 2, layout.cols = 2)
#' @export


get_viewport_logo <- function(layout.rows, layout.cols,
                              widths_1 = 6, heights_1 = 20){
  grid::grid.newpage()
  top.vp <- viewport(layout=grid.layout(layout.rows, layout.cols,
                                        widths=unit(rep(widths_1,layout.cols), 
                                                    rep("null", layout.cols)),
                                        heights=unit(rep(heights_1,layout.rows),
                                                rep("lines", layout.rows))))

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

