

counts_mat <- rbind(c(0, 10, 100, 60, 20),
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70)
)

colnames(counts_mat) <- c("Pos 1", "Pos 2", "Pos 3", "Pos 4", "Pos 5")
rownames(counts_mat) <- c("/lambda/", "A", "X", "Y")



table <- counts_mat
ic=NULL
cols <-  RColorBrewer::brewer.pal(dim(counts_mat)[1],
                                  name = "Spectral")
frame_width=NULL
ic.scale=TRUE
xaxis=TRUE
yaxis=TRUE
xaxis_fontsize=10
xlab_fontsize=15
y_fontsize=15
start=0.0001
yscale_change=TRUE
pop_name = NULL
xlab = "X"
ylab = "Information content"
addlogos = "LAMBDA"
addlogos_text = "LAMBDA"

logomaker(counts_mat,
          cols= RColorBrewer::brewer.pal(dim(counts_mat)[1],
                                         name = "Spectral"),
          frame_width = 1,
          addlogos="LAMBDA",
          addlogos_text="LAMBDA")
