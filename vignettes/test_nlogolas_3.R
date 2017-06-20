

mFile <- system.file("Exfiles/pwm1", package="seqLogo")
m <- read.table(mFile)
p <- seqLogo::makePWM(m)
pwm_mat <- slot(p,name = "pwm")
pwm_mat <- apply(pwm_mat+0.0001,2,normalize)
mat1 <- cbind(pwm_mat[,c(3,4)], rep(0,4), pwm_mat[,c(5,6)]);

colnames(mat1) <- c("-2", "-1", "0", "1", "2")
mat2 <- cbind(rep(0,6), rep(0,6),
              c(0.9, 0.01, 0.01, 0.02, 0.03, 0.03),
              rep(0,6), rep(0,6))
rownames(mat2) <- c("C>T", "C>A", "C>G",
                    "T>A", "T>C", "T>G")

table <- rbind(mat1, mat2)

color_profile <- list("type" = "per_row",
                      "col" = RColorBrewer::brewer.pal(dim(table)[1],name ="Spectral"))

logomaker(table,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale = TRUE,
          yscale_change=TRUE,
          xlab = "Position",
          ylab = "Information content")

table[table == 0] <- NA

nlogomaker(table,
          color_profile = color_profile,
          frame_width = 1,
          yscale_change=TRUE,
          xlab = "Position",
          ylab = "Information content",
          ylimit = 2)


