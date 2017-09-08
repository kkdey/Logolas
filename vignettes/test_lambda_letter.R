
###############  test new symbols in Logolas   ############################


LAMBDAletter <- function(plot=FALSE,
                         fill_symbol = TRUE,
                         colfill="green",
                         lwd=10){

  x <- c(0.15, 0.5, 0.85, 0.75, 0.5, 0.25)
  y <- c(0, 1, 0, 0, 0.8, 0)
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
counts_mat <- rbind(c(0, 10, 100, 60, 20),
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70)
)
colnames(counts_mat) <- c("Pos 1", "Pos 2", "Pos 3", "Pos 4", "Pos 5")
rownames(counts_mat) <- c("R/LMBD/Q", "A", "X", "Y")
lambda <- LAMBDAletter(plot=TRUE)
grid::grid.newpage()
grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                  clip=TRUE))
grid::grid.polygon(lambda$x, lambda$y,
                   default.unit="native",
                   id=lambda$id,
                   gp=grid::gpar(fill=lambda$fill,
                                 lwd=10))
color_profile <- list("type" = "per_row",
                      "col" = RColorBrewer::brewer.pal(dim(counts_mat)[1], name = "Spectral"))
logomaker(counts_mat,
          color_profile = color_profile,
          frame_width = 1,
          addlogos="LMBD",
          addlogos_text="LAMBDA")

