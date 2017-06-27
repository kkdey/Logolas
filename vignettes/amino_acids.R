

#########   amino acids testing  ####################

counts <- get(load("countmat.Rda"))

cols1 <- c(rev(RColorBrewer::brewer.pal(12, "Paired"))[c(3,4,7,8,11,12,5,6,9,10)],
           RColorBrewer::brewer.pal(12, "Set3")[c(1,2,5,8,9)],
           RColorBrewer::brewer.pal(9, "Set1")[c(9,7)],
           RColorBrewer::brewer.pal(8, "Dark2")[c(3,4,8)])

color_profile <- list("type" = "per_row",
                      "col" = cols1)

nlogomaker(counts,
          color_profile = color_profile,
          frame_width = 1)

table <- counts
total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
frame_width=NULL
alpha=1
xaxis=TRUE
yaxis=TRUE
xaxis_fontsize=10
xlab_fontsize=15
y_fontsize=15
main_fontsize=16
start=0.001
yscale_change=TRUE
pop_name = NULL
xlab = "X"
ylab = "Enrichment Score"
col_line_split="grey80"
ylimit = 3
scale0=0.01
scale1=0.99
addlogos = NULL
addlogos_text = NULL
newpage = TRUE
