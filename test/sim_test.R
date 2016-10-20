
###########  Simulation test for Logo plot  ################################

counts_mat <- rbind(c(0, 10, 100, 60, 20),
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70)
)

colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
rownames(counts_mat) <- c("M", "U", "T",
                          "D")

logomaker(counts_mat,
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
          frame_width = 1,
          ic.scale = FALSE)

logomaker(counts_mat,
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
          frame_width = 1)


