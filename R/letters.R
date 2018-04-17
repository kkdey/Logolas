# @title Logo plot for numbers 0-9, A-Z and punctuation marks
#
# @description Plots the symbol or logo for numbers 0-9 and alphabets A-Z,
# left arrow, right arrow, colon, semic colon, dash, comma and dot.
#
# @param plot A binary. If FALSE, only outputs grid co-ordinates for the logo,
#        along with color labels. If TRUE, also plots the logo in a new grid
#        window. Defaults to FALSE.
# @param fill_symbol A binary. If TRUE, the function would fill the symbol by
#        the color represented in \code{colfill}, else colors the boundary
#        of the symbol by \code{colfill}. Defaults to TRUE.
# @param colfill  The color used to highlight the symbol.  Defaults to "green".
# @param lwd Specifies the border width of the symbol. Defaults to 10.
# @name letters
#
# @return Returns a list with the following items.
#         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#         \item{id}{id vector representing blocks in the logo co-ordinates}
#         \item{fill}{a vector equal to the number of distinct ids or blocks in
#                    the logo, whose elements correspond to colors of these blocks}
# @keywords internal
# @import grid
#
# @rdname letters
# @examples
# out <- zeroletter(plot=TRUE, fill_symbol = TRUE, colfill = "green")

#######################   zero    ######################################

zeroletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  angle2 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  x1 <- 0.5 + 0.3*cos(angle2)
  y1 <- 0.5 + 0.5*sin(angle2)

  x2 <- 0.5 + 0.15*cos(angle2)
  y2 <- 0.5 + 0.35*sin(angle2)

  x <- c(x1, x2)
  y <- c(y1, y2)

  id <- c(rep(1, length(x1)), rep(2, length(x2)))
  fill <- c(colfill, "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

###########################   one   ####################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- oneletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- oneletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")
oneletter <- function(plot = FALSE,
                      fill_symbol = TRUE,
                      colfill="green",
                      lwd =10){
  x <- c(0.15, 0.15, 0.45, 0.45, 0.28, 0.15, 0.45, 0.6, 0.6, 0.9, 0.9)
  y <- c(0, 0.2, 0.2, 0.75, 0.60, 0.60, 1, 1, 0.2, 0.2, 0)

  id <- rep(1, length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)

  return(ll)
}

#########################    two    ######################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- twoletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")
# out <- twoletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")

twoletter <- function(plot = FALSE,
                      fill_symbol = TRUE,
                      colfill="green",
                      lwd =10){

  x <- c(0.9, 0.1, 0.1, 0.7, 0.5, 0.25, 0.1, 0.5, 0.95, 0.35, 0.9)
  y <- c(0, 0, 0.2, 0.65, 0.80, 0.6, 0.7, 1, 0.65, 0.2, 0.2)
  x <- 0.05 + 0.9*x

  id <- rep(1, length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#########################   three    #####################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- threeletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")
# out <- threeletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")

threeletter <- function(plot = FALSE, fill_symbol = TRUE,
                        colfill="green", lwd =10){



  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
  y.l1 <- 0.75 + 0.25*sin(angle)
  x.l1 <- 0.5 + 0.30*cos(angle)

  y.l2 <- 0.25 + 0.25*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)


  y.l3 <- 0.75 + 0.15*sin(angle)
  x.l3 <- 0.4 + 0.20*cos(angle)

  outer_x1 <- c(0.25, x.l1)
  outer_y1 <- c(1, y.l1)

  outer_x2 <- c(x.l2, 0.25)
  outer_y2 <- c(y.l2, 0)

  inner_x1 <- c(0.25, 0.40, x.l3, 0.25)
  inner_y1 <- c(0.90, 0.90, y.l3, 0.60)

  y.l4 <- 0.25 + 0.15*sin(angle)
  x.l4 <- 0.4 + 0.20*cos(angle)

  inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
  inner_y2 <- c(0.40, 0.40, y.l4, 0.10)

  #plot(c(outer_x1, outer_x2, rev(inner_x2), rev(inner_x1)),
  #     c(outer_y1, outer_y2, rev(inner_y2), rev(inner_y1)))



  x <- c(outer_x1, outer_x2, rev(inner_x2), rev(inner_x1))
  x <- 0.05 + 0.90*x
  y <- c(outer_y1, outer_y2, rev(inner_y2), rev(inner_y1))

  id <- rep(1, length(x))
  fill <- c(colfill)
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)

}



######################    four    #######################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- fourletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- fourletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

fourletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  x <- c(0.3, 0.15, 0.55, 0.55, 0.70, 0.70, 0.80, 0.80, 0.70, 0.70, 0.55, 0.55, 0.32, 0.45)
  y <- c(1, 0.25, 0.25, 0, 0, 0.25, 0.25, 0.40, 0.40, 0.55, 0.55, 0.40, 0.40, 1)


  id <- rep(1, length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



#######################    five   #######################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- fiveletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- fiveletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

fiveletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))

  y.l2 <- 0.25 + 0.25*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  y.l4 <- 0.25 + 0.10*sin(angle)
  x.l4 <- 0.5 + 0.10*cos(angle)


  outer_x2 <- c(x.l2, 0.25)
  outer_y2 <- c(y.l2, 0)

  inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
  inner_y2 <- c(0.35, 0.35, y.l4, 0.15)

  x_curve <- c(outer_x2, rev(inner_x2))
  y_curve <- c(outer_y2, rev(inner_y2))

  x.l3 <- c(0.25, 0.8, 0.8, 0.45, 0.45)
  y.l3 <- c(1, 1, 0.8, 0.8, 0.5)

  x <- c(x_curve, x.l3)
  y <- c(y_curve, y.l3)

  id <- rep(1, length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#########################    six   ########################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- sixletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- sixletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

sixletter <- function(plot = FALSE,
                      fill_symbol = TRUE,
                      colfill="green",
                      lwd =10){

  angle <- c(seq((2*pi/3) - 0.2, 0, length.out=100), seq(0, -(pi) , length.out=100))

  y.l2 <- 0.27 + 0.27*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  angle2 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  y.l4 <- 0.27 + 0.12*sin(angle2)
  x.l4 <- 0.5 + 0.12*cos(angle2)


  # x.l <- c(x.l2, x.l2[length(x.l2)], 0.65, 0.65, 0.50, 0.50, x.l2[1])
  #  y.l <- c(y.l2, 1, 1, 0.7, 0.7, 0.85, 0.85)

  x.l <- c(x.l2, x.l2[length(x.l2)], 0.40, 0.60)
  y.l <- c(y.l2, 0.5, 1, 1)

  inner_x2 <- c(0.3, 0.40, x.l4, 0.30)
  inner_y2 <- c(0.35, 0.35, y.l4, 0.15)

  x <- c(x.l, x.l4)
  y <- c(y.l, y.l4)


  id <- c(rep(1, length(x.l)), rep(2, length(x.l4)))
  fill <- c(colfill, "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


###########################   seven  #####################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- sevenletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- sevenletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

sevenletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  x <- c(0.35, 0.65, 0.90, 0.90, 0.70, 0.9, 0.1, 0.1, 0.67, 0.55, 0.30, 0.30, 0.50, 0.2)
  y <- c(0, 0.5, 0.5, 0.65, 0.65, 1, 1, 0.85, 0.85, 0.65, 0.65, 0.5, 0.5, 0)

  fill <- colfill
  id <- rep(1, length(x))
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



#########################   eight   #######################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- eightletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- eightletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

eightletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  angle1 <- c(seq(-(pi/2) - 0.35, -2*pi, length.out=100), seq(0, -(pi/2)+0.35, length.out=100))

  y.l1 <- 0.75 + 0.25*sin(angle1)
  x.l1 <- 0.5 + 0.35*cos(angle1)

  angle2 <- c(seq(pi/2 - 0.35, 0, length.out=100), seq(0, -(3*pi/2) + 0.35, length.out=100))

  y.l2 <- 0.25 + 0.25*sin(angle2)
  x.l2 <- 0.5 + 0.35*cos(angle2)

  angle3 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  y.l3 <- 0.25 + 0.15*sin(angle3)
  x.l3 <- 0.5 + 0.15*cos(angle3)

  y.l4 <- 0.75 + 0.15*sin(angle3)
  x.l4 <- 0.5 + 0.15*cos(angle3)

  x <- c(x.l2, x.l1, x.l3, x.l4)
  y <- c(y.l2, y.l1, y.l3, y.l4)

  id <- c(rep(1, length(x.l1)+length(x.l2)), rep(2, length(x.l3)), rep(3, length(x.l4)))
  fill <- c(colfill, "white", "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)

  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



#####################   nine   ######################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- nineletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- nineletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")


nineletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  angle <- c(seq((2*pi/3)-0.2, 0, length.out=100), seq(0, -(pi), length.out=100))

  y.l2 <- 0.30 + 0.30*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  angle2 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  y.l4 <- 0.30 + 0.12*sin(angle2)
  x.l4 <- 0.5 + 0.12*cos(angle2)


  # x.l <- c(x.l2, x.l2[length(x.l2)], 0.65, 0.65, 0.50, 0.50, x.l2[1])
  #  y.l <- c(y.l2, 1, 1, 0.7, 0.7, 0.85, 0.85)

  x.l <- c(x.l2, x.l2[length(x.l2)], 0.40, 0.60)
  y.l <- c(y.l2, 0.5, 1, 1)

  inner_x2 <- c(0.3, 0.40, x.l4, 0.30)
  inner_y2 <- c(0.35, 0.35, y.l4, 0.15)

  x <- c(x.l, x.l4)
  y <- c(y.l, y.l4)

  x <- 1 - x
  y <- 1 - y

  id <- c(rep(1, length(x.l)), rep(2, length(x.l4)))
  fill <- c(colfill, "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

################################  A letter  ###############################
# @import grid
# @keywords internal
# @rdname letters
# @examples
# out <- Aletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
# out <- Aletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

Aletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

  x <- c(0,4,6,10,8,6.8,3.2,2,0,3.6,5,6.4,3.6)
  y <- c(0,10,10,0,0,3,3,0,0,4,7.5,4,4)
  x <- 0.1*x
  x <- 0.10 + 0.80*x
  y <- 0.1*y

  id <- c(rep(1,9),rep(2,4))
  fill <- c(colfill,"white")

  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


###########################  B letter  ##################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Bletter(plot=TRUE)
#

Bletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

  angle <- c(seq((pi/2), 0, length.out=100), seq(0, -(pi/2), length.out=100))

  y.l1 <- 0.75 + 0.25*sin(angle)
  x.l1 <- 0.5 + 0.15*cos(angle)

  y.l2 <- 0.75 + 0.10*sin(angle)
  x.l2 <- 0.5 + 0.1*cos(angle)


  y.l3 <- 0.25 + 0.25*sin(angle)
  x.l3 <- 0.5 + 0.15*cos(angle)

  y.l4 <- 0.25 + 0.10*sin(angle)
  x.l4 <- 0.5 + 0.1*cos(angle)

  x <- c(x.l1, x.l2, x.l3, x.l4)
  y <- c(y.l1, y.l2, y.l3, y.l4)

  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
  y.l1 <- 0.75 + 0.25*sin(angle)
  x.l1 <- 0.5 + 0.30*cos(angle)

  y.l2 <- 0.25 + 0.25*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)


  y.l3 <- 0.75 + 0.15*sin(angle)
  x.l3 <- 0.4 + 0.20*cos(angle)

  inner_x1 <- c(0.25, 0.40, x.l3, 0.25)
  inner_y1 <- c(0.90, 0.90, y.l3, 0.60)

  y.l4 <- 0.25 + 0.15*sin(angle)
  x.l4 <- 0.4 + 0.20*cos(angle)

  inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
  inner_y2 <- c(0.40, 0.40, y.l4, 0.10)


  x <- c(0, 0, 0.5, x.l1, x.l2, inner_x1, inner_x2)
  x <- 0.15 + 0.90*x
  y <- c(0, 1, 1, y.l1, y.l2, inner_y1, inner_y2)

  id <- c(rep(1, length(x)-length(inner_x1) - length(inner_x2)),
          rep(2, (length(inner_x1))), rep(3, length(inner_x2)))
  fill <- c(colfill, "white", "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



#######################   C letter   ####################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Cletter(plot=TRUE, fill_symbol = TRUE, colfill = "green")
# out <- Cletter(plot=TRUE, fill_symbol = FALSE, colfill = "green")

Cletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

  angle1 <- seq(0.3+pi/2,pi,length=100)
  angle2 <- seq(pi,1.5*pi,length=100)
  x.l1 <- 0.5 + 0.5*sin(angle1)
  y.l1 <- 0.5 + 0.5*cos(angle1)
  x.l2 <- 0.5 + 0.5*sin(angle2)
  y.l2 <- 0.5 + 0.5*cos(angle2)

  x.l <- c(x.l1,x.l2)
  y.l <- c(y.l1,y.l2)

  x <- c(x.l,rev(x.l))
  y <- c(y.l,1-rev(y.l))

  x.i1 <- 0.5 +0.30*sin(angle1)
  y.i1 <- 0.5 +0.30*cos(angle1)
  x.i1 <- x.i1[y.i1<=max(y.l1)]
  y.i1 <- y.i1[y.i1<=max(y.l1)]
  y.i1[1] <- max(y.l1)

  x.i2 <- 0.5 +0.30*sin(angle2)
  y.i2 <- 0.5 +0.30*cos(angle2)

  x.i <- c(x.i1,x.i2)
  y.i <- c(y.i1,y.i2)

  x1 <- c(x.i,rev(x.i))
  y1 <- c(y.i,1-rev(y.i))

  x <- c(x,rev(x1))
  x <- 0.05 + 0.90*x
  y <- c(y,rev(y1))

  id <- rep(1, length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }



  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


#######################  colon   ######################################
# @description Plots the symbol or logo for a colon.
#
# @param y_pos_1 The y-position of the center of the lower dot in [0,1] X [0,1]
#        window. Defaults to 0.1.
# @param y_pos_2  The y-position of the center of the upper dot in [0,1] X [0,1] window.
#        Defaults to 0.7.
# @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#        Defaults to 0.5.
# @param rad The radius of the indivudual symbols in the semicolon.
# @param lwd Specifies the border width of the symbol. Defaults to 10.
# @keywords internal
# @import grid
# @rdname letters
#
#
# @examples
# out <- colonletter(plot=TRUE)
#
colonletter <- function(plot = FALSE,
                        fill_symbol = FALSE,
                        colfill="green",
                        y_pos_1 = 0.3,
                        y_pos_2 = 0.7,
                        x_pos = 0.5,
                        rad = 0.1,
                        lwd=10){

  if(fill_symbol){fill_symbol = FALSE}

  angle2 <- seq(0,2*pi,length=200)

  x1 <- x_pos + rad*cos(angle2)
  y1 <- y_pos_1 + rad*sin(angle2)

  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)

  x <- c(x1, x2)
  y <- c(y1, y2)


  id <- c(rep(1, length(x1)), rep(2, length(x2)))
  fill <- c(colfill, colfill)
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                      clip=TRUE))
    grid::grid.polygon(x, y,
                       default.unit="native",
                       id=id,
                       gp=grid::gpar(fill=fill,
                                     lwd=lwd))
  }
  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

##########################  comma   #####################################
# @description Plots the symbol or logo for comma punctuation
#
# @param y_pos The y-position of the center of the comma in [0,1] X [0,1] window.
#        Defaults to 0.1.
# @param x_pos The x-postition of the center of the comma in [0,1] X [0,1] window.
#        Defaults to 0.5.
# @param lwd Specifies boundary of the comma. Defaults 10.
#
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- commaletter(plot=TRUE)

commaletter <- function(plot = FALSE,
                        fill_symbol = FALSE,
                        colfill="green",
                        y_pos = 0.1,
                        x_pos = 0.5,
                        lwd=10){

  if(fill_symbol){fill_symbol = FALSE}

  x <- x_pos + c(-0.05, -.05, 0.05, 0.05, -0.03, 0.01)
  y <- y_pos + c(0, 0.1, 0.1, 0, -0.1, 0)

  id <- c(rep(1, length(x)))
  fill <- c(colfill)
  colfill <- rep(colfill, length(unique(id)))
  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                      clip=TRUE))
    grid::grid.polygon(x, y,
                       default.unit="native",
                       id=id,
                       gp=grid::gpar(fill=fill,
                                     lwd=lwd))
  }
  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

##########################  dash   #######################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- dashletter(plot=TRUE)
#

dashletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  x <- c(0.25, 0.25, 0.75, 0.75)
  y <- c(0.45, 0.6, 0.6, 0.45)

  id <- rep(1, length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))
  if(plot){
    get_plot(x, y, id, fill)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


########################   D letter   ##################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Dletter(plot=TRUE)
#

Dletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

  angle <- c(seq((pi/2), 0, length.out=100), seq(0, -(pi/2), length.out=100))

  y.l1 <- 0.5 + 0.5*sin(angle)
  x.l1 <- 0.5 + 0.5*cos(angle)

  y.l2 <- 0.5 + 0.3*sin(angle)
  x.l2 <- 0.5 + 0.3*cos(angle)

  x_out <- c(0, x.l1, 0)
  y_out <- c(1, y.l1, 0)

  x_in <- c(0.2, 0.5, rev(x.l2), 0.2, 0.2)
  y_in <- c(0.2, 0.2, rev(y.l2), 0.8, 0.2)


  x <- c(x_out, x_in)
  x <- 0.10 + 0.80*x
  y <- c(y_out, y_in)

  id <- c(rep(1, length(x_out)), rep(2, length(x_in)))
  fill <- c(colfill, "white")
  colfill <- rep(colfill, length(unique(id)))


  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#####################   E letter  #####################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Eletter(plot=TRUE)

Eletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){


  x <- c(0, 0, 0.7, 0.7, 0.15, 0.15, 0.5, 0.5, 0.15, 0.15, 0.7, 0.7)
  x <- 0.15 + 0.90*x
  y <- c(0,1,1,0.8,0.8,0.59,0.59,0.40,0.40,0.2,0.2,0)

  id <- rep(1,12)
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

########################   F letter   ####################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Fletter(plot=TRUE)

Fletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.7, 0.7, 0.15, 0.15, 0.4, 0.4, 0.15, 0.15)
  x <- 0.25 + 0.90*x
  y <- c(0,1,1,0.8,0.8,0.59,0.59,0.40,0.40,0)

  id <- rep(1,10)
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


######################   G letter   #####################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Gletter(plot=TRUE)

Gletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle1 <- seq(0.3+pi/2,pi,length=100)
  angle2 <- seq(pi,1.5*pi,length=100)
  x.l1 <- 0.5 + 0.5*sin(angle1)
  y.l1 <- 0.5 + 0.5*cos(angle1)
  x.l2 <- 0.5 + 0.5*sin(angle2)
  y.l2 <- 0.5 + 0.5*cos(angle2)

  x.l <- c(x.l1,x.l2)
  y.l <- c(y.l1,y.l2)

  x <- c(x.l,rev(x.l))
  y <- c(y.l,1-rev(y.l))

  x.i1 <- 0.5 +0.30*sin(angle1)
  y.i1 <- 0.5 +0.30*cos(angle1)
  x.i1 <- x.i1[y.i1<=max(y.l1)]
  y.i1 <- y.i1[y.i1<=max(y.l1)]
  y.i1[1] <- max(y.l1)

  x.i2 <- 0.5 +0.30*sin(angle2)
  y.i2 <- 0.5 +0.30*cos(angle2)

  x.i <- c(x.i1,x.i2)
  y.i <- c(y.i1,y.i2)

  x1 <- c(x.i,rev(x.i))
  y1 <- c(y.i,1-rev(y.i))

  x <- c(x,rev(x1))
  y <- c(y,rev(y1))

  h1 <- max(y.l1)
  r1 <- max(x.l1)

  h1 <- 0.4
  x.add <- c(r1,0.5,0.5,r1-0.2,r1-0.2,r1,r1)
  y.add <- c(h1,h1,h1-0.1,h1-0.1,0,0,h1)

  id <- c(rep(1,length(x)),rep(2,length(x.add)))

  x <- c(rev(x),x.add)
  x <- 0.05 + 0.90*x
  y <- c(rev(y),y.add)

  fill <- c(colfill, colfill)

  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


#####################  H letter   ##################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Hletter(plot=TRUE)

Hletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.25, 0.25, 0.75, 0.75, 1, 1, 0.75, 0.75, 0.25, 0.25)
  x <- 0.15 + 0.70*x
  y <- c(0, 1, 1, 0.62, 0.62, 1, 1, 0, 0, 0.38, 0.38, 0)

  id <- rep(1,length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


######################  I letter  ######################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Iletter(plot=TRUE)

Iletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.4, 0.4, 0, 0, 1, 1, 0.6, 0.6, 1, 1)
  x <- 0.15 + 0.70*x
  y <- c(0, 0.15, 0.15, 0.85, 0.85, 1, 1, 0.85, 0.85, 0.15, 0.15, 0)

  id <- rep(1,length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)

  return(ll)
}

######################   J letter  #####################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Jletter(plot=TRUE)

Jletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0.1, 0.1, 0.25, 0.25, 0.40, 0.40, 0.10, 0.10, 0.85, 0.85, 0.57, 0.57)
  x <- 0.15 + 0.80*x
  y <- c(0, 0.35, 0.35, 0.17, 0.17, 0.85, 0.85, 1, 1, 0.85, 0.85, 0)

  id <- rep(1,length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#####################  K  letter   ######################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Kletter(plot=TRUE)


Kletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.19, 0.19, 0.50, 0.75, 0.25, 0.75, 0.50, 0.19, 0.19)
  x <- 0.20 + 0.80*x
  y <- c(0, 1, 1, 0.70, 1, 1, 0.5, 0, 0, 0.30, 0)

  id <- rep(1,length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#######################  L letter  ######################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Lletter(plot=TRUE)

Lletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.2, 0.2, 0.8, 0.8)
  x <- 0.15 + 0.80*x
  y <- c(0, 1, 1, 0.2, 0.2, 0)


  id <- rep(1,length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)

  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


####################  M letter ########################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Mletter(plot=TRUE, fill_symbol = TRUE, colfill = "green")
# out <- Mletter(plot=TRUE, fill_symbol = FALSE, colfill = "green")

Mletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.2, 0.5, 0.8, 1, 1, 0.8, 0.8, 0.5, 0.2, 0.2)
  x <- 0.12 + 0.75*x
  y <- c(0, 1, 1, 0.6, 1, 1, 0, 0, 0.6, 0.2, 0.6, 0)


  id <- rep(1,length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)

  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


#####################   N letter  ########################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Nletter(plot=TRUE, fill_symbol = TRUE, colfill = "green")
# out <- Nletter(plot=TRUE, fill_symbol = FALSE, colfill = "green")

Nletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 0, 0.2, 0.8, 0.8, 1, 1, 0.8, 0.2, 0.2)
  x <- 0.12 + 0.75*x
  y <- c(0, 1, 1, 0.325, 1, 1, 0, 0, 0.675, 0)

  id <- rep(1,length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


######################   O  letter  ######################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Oletter(plot=TRUE)

Oletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle <- c(seq(0, 2*pi, length.out=100))

  y.l1 <- 0.5 + 0.5*sin(angle)
  x.l1 <- 0.5 + 0.5*cos(angle)


  y.l2 <- 0.5 + 0.30*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  id <- c(rep(1, length(x.l1)), rep(2, length(x.l2)))

  x <- c(x.l1, x.l2)
  x <- 0.10 + 0.80*x
  y <- c(y.l1, y.l2)

  fill <- c(colfill,"white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


#######################   P letter  ####################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Pletter(plot=TRUE)

Pletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
  y.l1 <- 0.75 + 0.25*sin(angle)
  x.l1 <- 0.4 + 0.30*cos(angle)

  y.l2 <- 0.75 + 0.10*sin(angle)
  x.l2 <- 0.4 + 0.15*cos(angle)

  inner_x <- c(0.25, 0.40, x.l2, 0.25)
  inner_y <- c(0.85, 0.85, y.l2, 0.65)


  x <- 0.2+c(0, 0, 0.5, x.l1, 0.25, 0.25, inner_x)
  x <- 0.05 + 0.90*x
  y <- c(0, 1,  1,  y.l1, 0.5, 0, inner_y)

  id <- c(rep(1, length(x)-length(inner_x)), rep(2, length(inner_x)))

  fill <- c(colfill, "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)

  return(ll)
}

#########################  Q letter  ##################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Qletter(plot=TRUE)

Qletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle <- c(seq(0, 2*pi, length.out=100))

  y.l1 <- 0.5 + 0.5*sin(angle)
  x.l1 <- 0.5 + 0.5*cos(angle)


  y.l2 <- 0.5 + 0.30*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  x.l3 <- c(0.6, 0.8, 1, 0.8)
  y.l3 <- c(0.3, 0.3, 0, 0)


  x <- c(x.l1, x.l2, x.l3)
  x <- 0.1 + 0.80*x
  y <- c(y.l1, y.l2, y.l3)

  id <- c(rep(1, length(x.l1)), rep(2, length(x.l2)), rep(3, length(x.l3)))
  fill=c(colfill,"white", colfill)
  colfill <- rep(colfill, length(unique(id)))


  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


######################   R letter   ######################################
# @import grid
# @keywords internal
# @rdname letters
#
# @examples
# out <- Rletter(plot=TRUE)

Rletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
  y.l1 <- 0.75 + 0.25*sin(angle)
  x.l1 <- 0.5 + 0.30*cos(angle)

  y.l2 <- 0.75 + 0.10*sin(angle)
  x.l2 <- 0.4 + 0.20*cos(angle)

  inner_x <- c(0.25, 0.40, x.l2, 0.25)
  inner_y <- c(0.85, 0.85, y.l2, 0.65)


  x <- c(0, 0, 0.5, x.l1, 0.3, 0.9, 0.6, 0.2, 0.2, inner_x)
  x <- 0.15 + 0.80*x
  y <- c(0, 1,  1,  y.l1, 0.5, 0,  0,  0.4,  0, inner_y)

  id <- c(rep(1, length(x)-length(inner_x)), rep(2, length(inner_x)))

  fill <- c(colfill, "white")
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)

  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


##################   semi colon letter  ##################################
# @description Plots the symbol or logo for a semi-colon.
#
# @param y_pos_1 The y-position of the center of the lower dot in [0,1] X [0,1]
#        window. Defaults to 0.1.
# @param y_pos_2  The y-position of the center of the upper dot in [0,1] X [0,1] window.
#        Defaults to 0.7.
# @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#        Defaults to 0.5.
# @param rad The radius of the indivudual symbols in the semicolon.
# @param lwd Specifies the border width of the symbol. Defaults to 10.
#
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- semicolonletter(plot=TRUE)
#
semicolonletter <- function(plot = FALSE,
                            fill_symbol = FALSE,
                            colfill="green",
                            y_pos_1 = 0.1,
                            y_pos_2 = 0.4,
                            x_pos = 0.5,
                            rad = 0.1,
                            lwd=10){

  if(fill_symbol){fill_symbol = FALSE}

  angle2 <- seq(0,2*pi,length=200)

  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)

  x1 <- x_pos + c(-0.05, -.05, 0.05, 0.05, -0.03, 0.01)
  y1 <- y_pos_1 + c(0, 0.1, 0.1, 0, -0.1, 0)

  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)

  x <- c(x1, x2)
  y <- c(y1, y2)


  id <- c(rep(1, length(x1)), rep(2, length(x2)))
  fill <- c(colfill, colfill)
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                      clip=TRUE))
    grid::grid.polygon(x, y,
                       default.unit="native",
                       id=id,
                       gp=grid::gpar(fill=fill,
                                     lwd=lwd))
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#####################   S letter   ##################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Sletter(plot=TRUE)

Sletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle1 <- c(seq((pi/2), 3*(pi/2), length.out=100))

  y.l1 <- 0.70 + 0.15*sin(angle1)
  x.l1 <- 0.5 + 0.30*cos(angle1)

  x_out <- c(1, x.l1)
  y_out <- c(1, y.l1)

  angle2 <- c(seq(-(pi/2), (pi/2), length.out=100))

  y.l2 <- 0.275 + 0.275*sin(angle2)
  x.l2 <- 0.5 + 0.45*cos(angle2)

  y.l3 <- 0.27 + 0.13*sin(angle2)
  x.l3 <- 0.5 + 0.25*cos(angle2)

  y.l4 <- 0.70 + 0.30*sin(angle1)
  x.l4 <- 0.5 + 0.5*cos(angle1)

  x <- c(0.85, x.l1, rev(x.l2), 0.1, 0.1, 0.5, x.l3, rev(x.l4), 0.85)
  x <- 0.10 + 0.80*x
  y <- c(0.85, y.l1, rev(y.l2), 0, 0.14, 0.14, y.l3, rev(y.l4), 1)

  id <- c(rep(1, length(x)))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


#######################   T  letter   ####################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Tletter(plot=TRUE)

Tletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c( 0.4, 0.4, 0, 0, 1, 1, 0.6, 0.6)
  x <- 0.10 + 0.80*x
  y <- c( 0, 0.80, 0.80, 1, 1, 0.80, 0.80, 0)

  id <- rep(1,length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

#####################  U letter   ######################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Uletter(plot=TRUE)

Uletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  angle <- c(seq(pi, 3*(pi/2), length.out=100), seq(3*(pi/2), 2*pi, length.out=100))

  y.l1 <- 0.5 + 0.5*sin(angle)
  x.l1 <- 0.5 + 0.5*cos(angle)

  y.l2 <- 0.5 + 0.3*sin(angle)
  x.l2 <- 0.5 + 0.3*cos(angle)


  x <- c(0, x.l1, 1, 0.8, rev(x.l2), 0.2)
  x <- 0.1 + 0.80*x
  y <- c(1, y.l1, 1, 1, rev(y.l2), 1)

  id <- c(rep(1, length(x)))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))


  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


####################   V letter ####################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Vletter(plot=TRUE)

Vletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c( 0.5, 0, 0.2, 0.5, 0.8, 1)
  x <- 0.10 + 0.80*x
  y <- c( 0, 1, 1, 0.35, 1, 1)

  id <- rep(1,length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd , fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}

######################   W letter  ###################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Wletter(plot=TRUE)

Wletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  x <- c( 0.25, 0, 0.15, 0.3, 0.5, 0.7, 0.85, 1, 0.75, 0.5)
  x <- 0.10 + 0.80*x
  y <- c( 0, 1, 1, 0.35, 0.8, 0.35, 1, 1, 0, 0.4)


  id <- rep(1,length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



#######################   X letter  ##################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Xletter(plot=TRUE)

Xletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c( 0, 0.4, 0, 0.2, 0.5, 0.8, 1, 0.6, 1, 0.8, 0.5, 0.2)
  x <- 0.10 + 0.80*x
  y <- c( 0, 0.5, 1, 1, 0.6, 1, 1, 0.5, 0, 0, 0.4, 0)


  id <- rep(1,length(x))
  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


####################   Y letter   ##################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Yletter(plot=TRUE)

Yletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(  0.4, 0.4, 0, 0.2, 0.5, 0.8, 1, 0.6, 0.6)
  x <- 0.1 + 0.80*x
  y <- c(  0, 0.5, 1, 1, 0.6, 1, 1, 0.5, 0)


  id <- rep(1,length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}



#####################   Z letter  ###################################
# @keywords internal
# @import grid
# @rdname letters
#
# @examples
# out <- Zletter(plot=TRUE)


Zletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

  x <- c(0, 1, 1, 0.25, 1, 1, 0, 0, 0.75, 0)
  x <- 0.15 + 0.75*x
  y <- c(1, 1, 0.8, 0.2, 0.2, 0, 0, 0.25, 0.8, 0.8)

  id <- c(rep(1, length(x)))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


################  right arrow   #####################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- rightarrowletter(plot=TRUE)

rightarrowletter <- function(plot = FALSE,
                             fill_symbol = TRUE,
                             colfill="green",
                             lwd =10){

  x <- c(0.25, 0.25, 0.7, 0.7, 0.85, 0.7, 0.7)
  y <- c(0.45, 0.6, 0.6, 0.7, 0.53, 0.36, 0.45)

  id <- rep(1, length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)
  return(ll)
}


####################  left arrow   #################################
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- leftarrowletter(plot=TRUE)


leftarrowletter <- function(plot = FALSE,
                            fill_symbol = TRUE,
                            colfill="green",
                            lwd =10){

  x <- 1 - c(0.25, 0.25, 0.7, 0.7, 0.85, 0.7, 0.7)
  y <- c(0.45, 0.6, 0.6, 0.7, 0.53, 0.36, 0.45)

  id <- rep(1, length(x))

  fill <- colfill

  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    get_plot(x, y, id, fill)
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)

  return(ll)
}


##################   dot   ###################################

# @description Plots the symbol or logo for dot punctuation
#
# @param y_pos The y-position of the center of the dot in [0,1] X [0,1] window.
#        Defaults to 0.1.
# @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#        Defaults to 0.5.
# @param rad The radius of the dot. Defaults to 0.1.
#
# @keywords internal
# @import grid
# @rdname letters
# @examples
# out <- dotletter(plot=TRUE)

dotletter <- function(plot = FALSE,
                      fill_symbol = FALSE,
                      colfill="green",
                      y_pos = 0.1,
                      x_pos = 0.5,
                      rad = 0.1){

  if(fill_symbol){fill_symbol = FALSE}

  angle2 <- seq(0,2*pi,length=200)
  x <- x_pos + rad*cos(angle2)
  y <- y_pos + rad*sin(angle2)

  id <- rep(1, length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                      clip=TRUE))
    grid::grid.polygon(x, y,
                       default.unit="native",
                       id=id,
                       gp=grid::gpar(fill=fill,
                                     lwd=1))
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)

  return(ll)
}

