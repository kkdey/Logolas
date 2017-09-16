

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


Logolas::logomaker(mat,xlab = 'position',color_profile = color_profile,
          bg = NULL, ic.scale = TRUE, yscale_change = TRUE,
          frame_width = 1, control = list(viewport.margin.bottom = 4))

nlogomaker(mat,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(ic_scale = 1, quant = 0.5,
                          depletion_weight = 0,
                          viewport.margin.bottom = 4))



table <- mat
logoheight = "ic"
total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
bg = NULL
frame_width=NULL
yscale_change=TRUE
pop_name = NULL
addlogos = NULL
addlogos_text = NULL
newpage = TRUE
yrange = NULL
xaxis=TRUE
yaxis=TRUE
xaxis_fontsize=10
xlab_fontsize=15
y_fontsize=15
main_fontsize=16
start=0.001
xlab = "X"
ylab = "Enrichment Score"
col_line_split="grey80"
control = list()
