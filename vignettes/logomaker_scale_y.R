

#####################  logomaker y axis scaling issue   ###############################


mat <- rbind(c(326, 296, 81, 245, 71),
             c(258, 228, 55, 273, 90),
             c(145, 121, 29, 253, 85),
             c(60, 52, 23, 180, 53),
             c(150, 191, 63, 178, 63))
rownames(mat) <- c("H3K4ME1", "H3K4ME2", "H3K4ME3", "H3AC", "H4AC")
colnames(mat) <- c("Intergenic","Intron","Exon \n 1000 KB window",
                   "Gene start \n 1000 KB window","Gene end \n 1000 KB window")

color_profile <- list("type" = "per_row",
                      "col" = RColorBrewer::brewer.pal(dim(mat)[1],name ="Spectral"))

logomaker(mat,color_profile = color_profile,frame_width = 1, ic.scale = TRUE, yscale_change = TRUE)
