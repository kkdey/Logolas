
###########  Simulation test for Logo plot  ################################

counts_mat <- rbind(c(0, 10, 100, 60, 20),
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70),
                    c(4, 2, 3, 7, 10),
                    c(12, 8, 0, 21, 3),
                    rep(0, 5),
                    c(24, 21, 17, 14, 18),
                    c(12, 8, 16, 7, 20),
                    c(55, 0, 1, 23, 12),
                    rep(0,5),
                    c(rep(0,3), 20, 0),
                    c(43, 21, 23, 20, 26),
                    c(12, 20, 3, 17, 22),
                    c(4, 7, 12, 3, 10),
                    c(32, 12, 8, 12, 9),
                    c(5, 6, 3, 34, 1),
                    c(23, 12, 11, 35, 14),
                    c(14, 15, 8, 13, 22),
                    c(10, 15, 4, 23, 23))



rownames(counts_mat) <- c("A", "R", "N", "D",
                          "C", "E", "Q", "G",
                          "H", "I", "L", "K",
                          "M", "F", "P", "S",
                          "T", "W", "Y", "V")

colnames(counts_mat) <- c("Pos 1", "Pos 2", "Pos 3", "Pos 4", "Pos 5")

cols1 <- c(rev(RColorBrewer::brewer.pal(12, "Paired"))[c(3,4,7,8,11,12,5,6,9,10)],
           RColorBrewer::brewer.pal(12, "Set3")[c(1,2,5,8,9)],
           RColorBrewer::brewer.pal(9, "Set1")[c(9,7)],
           RColorBrewer::brewer.pal(8, "Dark2")[c(3,4,8)])


logomaker(counts_mat,
          cols= cols1,
          frame_width = 1,
          ic.scale  = FALSE,
          yscale_change = TRUE)



############  sequence motif generation  #########################

counts_mat <- rbind(c(0, 10, 100, 60, 20),
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70)
)

colnames(counts_mat) <- c("Pos 1", "Pos 2", "Pos 3", "Pos 4", "Pos 5")
rownames(counts_mat) <- c("A", "C", "G", "T")

logomaker(counts_mat,
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
          frame_width = 1,
          ic.scale = FALSE)

logomaker(counts_mat,
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
          frame_width = 1)

logomaker(counts_mat,
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],name = "Spectral"),
          frame_width = 1,
          yscale_change = FALSE,
          pop_name = "Pop A")

############  protein motif generation  #########################

counts_mat <- rbind(c(24, 4, 14, 10, 15),
                    c(0, 13, 0, 5, 6),
                    c(20, 54, 0, 0, 12),
                    c(5, 6, 34, 9, 23),
                    )
rownames(counts_mat) <- c("A", "R", "N", "D",
                          "C", "E", "Q", "G",
                          "H", "I", "L", "K",
                          "M", "F", "P", "S",
                          "T", "W", "Y", "V")

colnames(counts_mat) <- c("Pos 1", "Pos 2", "Pos 3", "Pos 4", "Pos 5")

