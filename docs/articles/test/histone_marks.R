

########   Logolas plot for HISTONE marks  ##########################

###  histone marks data from GMO6990 ChIP-CHIP  data ###########

mat <- rbind(c(326, 296, 81, 245, 71),
             c(258, 228, 55, 273, 90),
             c(145, 121, 29, 253, 85),
             c(60, 52, 23, 180, 53),
             c(150, 191, 63, 178, 63))

rownames(mat) <- c("H3K4ME1", "H3K4ME2", "H3K4ME3", "H3AC", "H4AC")
colnames(mat) <- c("Intergenic",
                   "Intron",
                   "Exon \n 1000 KB window",
                   "Gene start \n 1000 KB window",
                   "Gene end \n 1000 KB window")



logomaker(mat,
          cols= sample(RColorBrewer::brewer.pal(10,name = "Spectral"), dim(mat)[1]),
          frame_width = 1,
          ic.scale = TRUE,
          pop_name = "Histone marks prop. abundance in various genomic regions",
          xlab = "",
          ylab = "Information content",
          yscale_change = TRUE,
          col_line_split = "black")


