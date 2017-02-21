

library(Logolas)


mFile <- system.file("Exfiles/pwm1", package="seqLogo")
m <- read.table(mFile)
p <- seqLogo::makePWM(m)

color_profile <- list("type" = "per_row",
                      "col" = RColorBrewer::brewer.pal(dim(p@pwm)[1],name ="Spectral"))
logomaker(p@pwm,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale = TRUE,
          yscale_change=TRUE,
          xlab="position",
          col_line_split = "grey80", pop_name = "Base composition across DNA")


#######   Amino acid composition across protein  #######################

counts_mat <- rbind(c(0, 0, 100, 1, 2), c(4, 3, 30, 35, 2),
                    c(100, 0, 10, 2, 7),rep(0,5),
                    c(4, 2, 3, 7, 70), c(1, 8, 0, 60, 3),
                    rep(0, 5), c(4, 2, 100, 1, 1),
                    c(12, 8, 16, 7, 20), c(55, 0, 1, 0, 12),
                    rep(0,5), c(rep(0,3), 20, 0),
                    rep(0,5), c(0, 0, 30, 0, 22),
                    c(1, 0, 12, 3, 10), rep(0,5),
                    c(0, 1, 0, 34, 1), c(0, 1, 12, 35, 1),
                    c(0, 30, 1, 10, 2), c(0, 1, 4, 100, 2))

rownames(counts_mat) <- c("A", "R", "N", "D","C", "E", "Q", "G",
                          "H", "I", "L", "K", "M", "F", "P", "S",
                          "T", "W", "Y", "V")

colnames(counts_mat) <- c("Pos 1", "Pos 2", "Pos 3", "Pos 4", "Pos 5")

cols1 <- c(rev(RColorBrewer::brewer.pal(12, "Paired"))[c(3,4,7,8,11,12,5,6,9,10)],
           RColorBrewer::brewer.pal(12, "Set3")[c(1,2,5,8,9)],
           RColorBrewer::brewer.pal(9, "Set1")[c(9,7)],
           RColorBrewer::brewer.pal(8, "Dark2")[c(3,4,8)])

color_profile <- list("type" = "per_row",
                      "col" = cols1)

logomaker(counts_mat,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale  = TRUE,
          yscale_change = TRUE,
          pop_name = "Amino acid composition across protein")



##############  Mutation trios example  ####################

mat1 <- cbind(c(0.20, 0.28, 0.20, 0.32), rep(0,4), c(0.2, 0.2, 0.3, 0.3));
rownames(mat1) <-  c("C", "G", "T", "A")
colnames(mat1) <- c("left flanking", "mutation", "right flanking")
mat2 <- cbind(rep(0,6),
              c(0.4, 0.3, 0.1, 0.13, 0.05, 0.02),
              rep(0,6))
rownames(mat2) <- c("C>A", "C>T", "C>G",
                    "T>A", "T>C", "T>G")

table <- rbind(mat1, mat2)

cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))

total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")

set.seed(20)
color_profile <- list("type" = "per_symbol",
                      "col" = sample(col_vector, length(total_chars), replace=FALSE))

logomaker(table,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale = TRUE,
          yscale_change=TRUE,
          alpha=500,
          xlab = "Position",
          ylab = "Information content",
          pop_name = "Mutation trio composition")


##########   Ecological bird species abundance in three groups  #############


set.seed(20)
data("himalayan_fauna_3_clusters")
colnames(himalayan_fauna_3_clusters) <- c("Region-1", "Regions-2", "Region-3")

color_profile <- list("type" = "per_column",
                      "col" = sample(RColorBrewer::brewer.pal(10,name = "Spectral"),
                                     dim(himalayan_fauna_3_clusters)[2], replace=TRUE))
logomaker(himalayan_fauna_3_clusters,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale = TRUE,
          alpha=200,
          yscale_change = TRUE,
          pop_name = "Bird family abundance composition across regions",
          xlab = "Regions",
          ylab = "Information content")

###########   Histone modification composition ############################


mat <- rbind(c(326, 296, 81, 245, 71),
             c(258, 228, 55, 273, 90),
             c(145, 121, 29, 253, 85),
             c(60, 52, 23, 180, 53),
             c(150, 191, 63, 178, 63))

rownames(mat) <- c("H3K4ME1", "H3K4ME2", "H3K4ME3", "H3AC", "H4AC")
colnames(mat) <- c("Intergenic","Intron","Exon \n 1000 KB window",
                   "Gene start \n 1000 KB window","Gene end \n 1000 KB window")

color_profile <- list("type" = "per_row",
                      "col" = sample(RColorBrewer::brewer.pal(10,name = "Spectral"),
                                     dim(mat)[1]))


logomaker(mat,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale = TRUE,
          pop_name = "Histone marks in various genomic regions",
          xlab = "",
          ylab = "Information content",
          yscale_change = TRUE,
          col_line_split = "black")


