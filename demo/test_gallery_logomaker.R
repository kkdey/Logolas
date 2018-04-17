

##########  test gallery examples logomaker   #################

#####################  Example 1   ################################

library(Logolas)
library(grid)

data("seqlogo_example")
get_viewport_logo(2, 2, heights_1 = 16)

seekViewport(paste0("plotlogo", 1))
logomaker(seqlogo_example, type = "EDLogo", color_type = "per_row", colors = c("#ABDDA4","#FDAE61", "#2B83BA", "#D7191C"),
          logo_control = list(newpage = FALSE, control = list(tofill_pos = TRUE, tofill_neg=TRUE)))


seekViewport(paste0("plotlogo", 2))
logomaker(seqlogo_example, type = "EDLogo", color_type = "per_row", colors = c("#ABDDA4","#FDAE61", "#2B83BA", "#D7191C"),
          logo_control = list(newpage = FALSE, control = list(tofill_pos = TRUE, tofill_neg=FALSE)))


seekViewport(paste0("plotlogo", 3))
logomaker(seqlogo_example, type = "EDLogo", color_type = "per_row", colors = c("#ABDDA4","#FDAE61", "#2B83BA", "#D7191C"),
          logo_control = list(newpage = FALSE, control = list(tofill_pos = FALSE, tofill_neg=TRUE)))


seekViewport(paste0("plotlogo", 4))
logomaker(seqlogo_example, type = "EDLogo", color_type = "per_row", colors = c("#ABDDA4","#FDAE61", "#2B83BA", "#D7191C"),
          logo_control = list(newpage = FALSE, control = list(tofill_pos = FALSE, tofill_neg=FALSE)))

#############  Example  2 ##############################

get_viewport_logo(2, 2, heights_1 = 16)

seekViewport(paste0("plotlogo", 1))
logomaker(seqlogo_example, type = "Logo", color_type = "per_row", colors = RColorBrewer::brewer.pal(dim(seqlogo_example)[1],name ="PiYG"),
          logo_control = list(newpage = FALSE))

seekViewport(paste0("plotlogo", 2))
logomaker(seqlogo_example, type = "Logo", logo_control = list(yscale_change = FALSE, newpage = FALSE))

seekViewport(paste0("plotlogo", 3))
logomaker(seqlogo_example, type = "Logo", logo_control = list(ic.scale = FALSE, newpage = FALSE))

bg=c(0.32, 0.18, 0.2, 0.3)
names(bg) <- c("A", "C", "G", "T")
seekViewport(paste0("plotlogo", 4))
logomaker(seqlogo_example, type = "Logo", bg = bg, logo_control = list(newpage = FALSE))


############  Example 5  ############################

library(Logolas)
data(pssm)
cols1 <- c(rev(RColorBrewer::brewer.pal(12, "Paired"))[c(3,4,7,8,11,12,5,6,9,10)],
           RColorBrewer::brewer.pal(12, "Set3")[c(1,2,5,8,9)],
           RColorBrewer::brewer.pal(9, "Set1")[c(9,7)],
           RColorBrewer::brewer.pal(8, "Dark2")[c(3,4,8)])

logo_pssm(pssm, color_type = "per_row", colors = cols1,
          control = list(round_off = 0, quant = 0.5))


